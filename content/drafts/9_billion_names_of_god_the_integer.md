+++
title = "9 billion names of God the integer"
description = ""
date = 2019-10-03T18:19:12Z
aliases = []
[extra]
id = 13385
[taxonomies]
categories = []
tags = []
+++

## Description
This task is a variation of the [short story by Arthur C. Clarke](https://en.wikipedia.org/wiki/The Nine Billion Names of God#Plot_summary).

(Solvers should be aware of the consequences of completing this task.)

In detail, to specify what is meant by a   “name”:
:The integer 1 has 1 name       “1”.
:The integer 2 has 2 names   “1+1”,   and   “2”.
:The integer 3 has 3 names   “1+1+1”,   “2+1”,   and   “3”.
:The integer 4 has 5 names   “1+1+1+1”,   “2+1+1”,   “2+2”,   “3+1”,   “4”.
:The integer 5 has 7 names   “1+1+1+1+1”,   “2+1+1+1”,   “2+2+1”,   “3+1+1”,   “3+2”,   “4+1”,   “5”.


## Task
Display the first 25 rows of a number triangle which begins:

```txt

                                      1
                                    1   1
                                  1   1   1
                                1   2   1   1
                              1   2   2   1   1
                            1   3   3   2   1   1

```


Where row   <math>n</math>   corresponds to integer   <math>n</math>,   and each column   <math>C</math>   in row   <math>m</math>   from left to right corresponds to the number of names beginning with   <math>C</math>.

A function   <math>G(n)</math>   should return the sum of the   <math>n</math>-th   row.

Demonstrate this function by displaying:   <math>G(23)</math>,   <math>G(123)</math>,   <math>G(1234)</math>,   and   <math>G(12345)</math>.

Optionally note that the sum of the   <math>n</math>-th   row   <math>P(n)</math>   is the    [integer partition function](http://mathworld.wolfram.com/PartitionFunctionP.html).

Demonstrate this is equivalent to   <math>G(n)</math>   by displaying:   <math>P(23)</math>,   <math>P(123)</math>,   <math>P(1234)</math>,   and   <math>P(12345)</math>.


;Extra credit

If your environment is able, plot   <math>P(n)</math>   against   <math>n</math>   for   <math>n=1\ldots 999</math>.





## AutoHotkey


```AutoHotkey
SetBatchLines -1

InputBox, Enter_value, Enter the no. of lines sought
array := []
Loop, % 2*Enter_value - 1
	Loop, % x := A_Index
		y := A_Index, Array[x, y] := 1

x := 3

Loop
{
	base_r := x - 1
	, x++
	, y := 2
	, index := x
	, new := 1

	Loop, % base_r - 1
	{
		array[x, new+1] := array[x-1, new] + array[base_r, y]
		, x++
		, new ++
		, y++
	}
	x := index
	If ( mod(x,2) = 0 )
	{
		to_run := floor(x - x/2)
		, y2 := to_run + 1
	}
	Else
	{
		to_run := x - floor(x/2)
		, y2 := to_run
	}
	Loop, % to_run
	{
		array[x, y2] := array[x-1, y2-1]
		, y2++
		If ( y2 = Enter_value + 1 ) && ( x = Enter_value )
		{
			Loop, % Enter_value
			{
				Loop, % x11 := A_Index
				{
					y11 := A_Index
					, string2 .= " " array[x11, y11]
				}
				string2 .= "`n"
			}
			MsgBox % string2
			ExitApp
		}
	}
}

~Esc::ExitApp
```

### Output
If user inputs 25, the result shall be:

```txt

1
1 1
1 1 1
1 2 1 1
1 2 2 1 1
1 3 3 2 1 1
1 3 4 3 2 1 1
1 4 5 5 3 2 1 1
1 4 7 6 5 3 2 1 1
1 5 8 9 7 5 3 2 1 1
1 5 10 11 10 7 5 3 2 1 1
1 6 12 15 13 11 7 5 3 2 1 1
1 6 14 18 18 14 11 7 5 3 2 1 1
1 7 16 23 23 20 15 11 7 5 3 2 1 1
1 7 19 27 30 26 21 15 11 7 5 3 2 1 1
1 8 21 34 37 35 28 22 15 11 7 5 3 2 1 1
1 8 24 39 47 44 38 29 22 15 11 7 5 3 2 1 1
1 9 27 47 57 58 49 40 30 22 15 11 7 5 3 2 1 1
1 9 30 54 70 71 65 52 41 30 22 15 11 7 5 3 2 1 1
1 10 33 64 84 90 82 70 54 42 30 22 15 11 7 5 3 2 1 1
1 10 37 72 101 110 105 89 73 55 42 30 22 15 11 7 5 3 2 1 1
1 11 40 84 119 136 131 116 94 75 56 42 30 22 15 11 7 5 3 2 1 1
1 11 44 94 141 163 164 146 123 97 76 56 42 30 22 15 11 7 5 3 2 1 1
1 12 48 108 164 199 201 186 157 128 99 77 56 42 30 22 15 11 7 5 3 2 1 1
1 12 52 120 192 235 248 230 201 164 131 100 77 56 42 30 22 15 11 7 5 3 2 1 1

```



## C

*Library: GMP*

If we forgo the rows and only want to calculate <math>P(n)</math>, using the recurrence relation <math>P_n = \sum_{k=1}^n (-1)^{k+1} \Big(P_{n-k(3k-1)/2} + P_{n-k(3k+1)/2}\Big)</math> is a better way.  This requires <math>O(n^2)</math> storage for caching instead the <math>O(n^3)</math>-ish for storing all the rows.

```c
#include <stdio.h>
#include <gmp.h>

#define N 100000
mpz_t p[N + 1];

void calc(int n)
{
	mpz_init_set_ui(p[n], 0);

	for (int k = 1; k <= n; k++) {
		int d = n - k * (3 * k - 1) / 2;
		if (d < 0) break;

		if (k&1)mpz_add(p[n], p[n], p[d]);
		else	mpz_sub(p[n], p[n], p[d]);

		d -= k;
		if (d < 0) break;

		if (k&1)mpz_add(p[n], p[n], p[d]);
		else	mpz_sub(p[n], p[n], p[d]);
	}
}

int main(void)
{
	int idx[] = { 23, 123, 1234, 12345, 20000, 30000, 40000, 50000, N, 0 };
	int at = 0;

	mpz_init_set_ui(p[0], 1);

	for (int i = 1; idx[at]; i++) {
		calc(i);
		if (i != idx[at]) continue;

		gmp_printf("%2d:\t%Zd\n", i, p[i]);
		at++;
	}
}
```

### Output

```txt

23:     1255
123:    2552338241
1234:   156978797223733228787865722354959930
12345:  69420357953926116819562977205209384460667673094671463620270321700806074195845953959951425306140971942519870679768681736
...

```


## C#

{{trans|Python}} {{trans|C}}
(this requires a System.Numerics registry reference)


```c#
using System;
using System.Collections.Generic;
using System.Linq;
using System.Numerics;

namespace NamesOfGod
{
    public class RowSummer
    {
        const int N = 100000;
        public BigInteger[] p;

        private void calc(int n)
            /* Translated from C */
        {
            p[n] = 0;

            for (int k = 1; k <= n; k++)
            {
                int d = n - k * (3 * k - 1) / 2;
                if (d < 0) break;

                if ((k & 1) != 0) p[n] += p[d];
                else p[n] -= p[d];

                d -= k;
                if (d < 0) break;

                if ((k & 1) != 0) p[n] += p[d];
                else p[n] -= p[d];
            }

        }
        public void PrintSums()
            /* translated from C */
        {
            p = new BigInteger[N + 1];
            var idx = new int[] { 23, 123, 1234, 12345, 20000, 30000, 40000, 50000, N, 0 };
            int at = 0;

            p[0] = 1;

            for (int i = 1; idx[at] > 0; i++)
            {
                calc(i);
                if (i != idx[at]) continue;
                Console.WriteLine(i + ":\t" + p[i]);
                at++;
            }
        }
    }

    public class RowPrinter
        /* translated from Python */
    {
        List<List<int>> cache;
        public RowPrinter()
        {
            cache = new List<List<int>> { new List<int> { 1 } };
        }
        public List<int> cumu(int n)
        {
            for (int l = cache.Count; l < n + 1; l++)
            {
                var r = new List<int> { 0 };
                for (int x = 1; x < l + 1; x++)
                    r.Add(r.Last() + cache[l - x][Math.Min(x, l - x)]);
                cache.Add(r);
            }
            return cache[n];
        }
        public List<int> row(int n)
        {
            var r = cumu(n);
            return (from i in Enumerable.Range(0, n) select r[i + 1] - r[i]).ToList();
        }
        public void PrintRows()
        {
            var rows = Enumerable.Range(1, 25).Select(x => string.Join(" ", row(x))).ToList();
            var widest = rows.Last().Length;
            foreach (var r in rows)
                Console.WriteLine(new String(' ', (widest - r.Length) / 2) + r);
        }
    }

    class Program
    {
        static void Main(string[] args)
        {
            var rpr = new RowPrinter();
            rpr.PrintRows();
            var ros = new RowSummer();
            ros.PrintSums();
            Console.ReadLine();
        }
    }
}

```

### Output

```txt
                                     1
                                    1 1
                                   1 1 1
                                  1 2 1 1
                                 1 2 2 1 1
                                1 3 3 2 1 1
                               1 3 4 3 2 1 1
                              1 4 5 5 3 2 1 1
                             1 4 7 6 5 3 2 1 1
                            1 5 8 9 7 5 3 2 1 1
                          1 5 10 11 10 7 5 3 2 1 1
                        1 6 12 15 13 11 7 5 3 2 1 1
                       1 6 14 18 18 14 11 7 5 3 2 1 1
                     1 7 16 23 23 20 15 11 7 5 3 2 1 1
                    1 7 19 27 30 26 21 15 11 7 5 3 2 1 1
                  1 8 21 34 37 35 28 22 15 11 7 5 3 2 1 1
                 1 8 24 39 47 44 38 29 22 15 11 7 5 3 2 1 1
               1 9 27 47 57 58 49 40 30 22 15 11 7 5 3 2 1 1
              1 9 30 54 70 71 65 52 41 30 22 15 11 7 5 3 2 1 1
            1 10 33 64 84 90 82 70 54 42 30 22 15 11 7 5 3 2 1 1
         1 10 37 72 101 110 105 89 73 55 42 30 22 15 11 7 5 3 2 1 1
       1 11 40 84 119 136 131 116 94 75 56 42 30 22 15 11 7 5 3 2 1 1
     1 11 44 94 141 163 164 146 123 97 76 56 42 30 22 15 11 7 5 3 2 1 1
  1 12 48 108 164 199 201 186 157 128 99 77 56 42 30 22 15 11 7 5 3 2 1 1
1 12 52 120 192 235 248 230 201 164 131 100 77 56 42 30 22 15 11 7 5 3 2 1 1
23:     1255
123:    2552338241
1234:   156978797223733228787865722354959930
12345:  69420357953926116819562977205209384460667673094671463620270321700806074195845953959951425306140971942519870679768681736
20000:  252114813812529697916619533230470452281328949601811593436850314108034284423801564956623970731689824369192324789351994903016411826230578166735959242113097
30000:  42963584246325385174883157483005920912690248645401139066014480612764163986215458185192990173314832179564211367228855321718015074490598095469727784182254987592569621576375743614022636192786
40000:  22807728274470728289340571240816959704646220378351611859439499408672657828590548093703330014605000554127042566412316061732771683740688051264237478893869163586426487354600342477491620506603389595232890082673857997469797
50000:  3626186097141667844592140891595633728165383082527785049015872755414109904256712082718122747316610565824630881772910217544261659239432670671532413858378256188987333877121891586607957389750538447474712592979263719012461858719791627302489739548263
100000: 27493510569775696512677516320986352688173429315980054758203125984302147328114964173055050741660736621590157844774296248940493063070200461792764493033510116079342457190155718943509725312466108452006369558934464248716828789832182345009262853831404597021307130674510624419227311238999702284408609370935531629697851569569892196108480158600569421098519

```



## C++


### The Code

see [[The Green Triangle](http://rosettacode.org/wiki/Talk:9_billion_names_of_God_the_integer#The_Green_Triangle)].

```cpp

// Calculate hypotenuse n of OTT assuming only nothingness, unity, and hyp[n-1] if n>1
// Nigel Galloway, May 6th., 2013
#include <gmpxx.h>
int N{123456};
mpz_class hyp[N-3];
const mpz_class G(const int n,const int g){return g>n?0:(g==1 or n-g<2)?1:hyp[n-g-2];};
void G_hyp(const int n){for(int i=0;i<N-2*n-1;i++) n==1?hyp[n-1+i]=1+G(i+n+1,n+1):hyp[n-1+i]+=G(i+n+1,n+1);}
}

```


### The Alpha and Omega, Beauty
Before displaying the triangle the following code displays hyp as it is transformed by consequtive calls of G_hyp.

```cpp

#include <iostream>
#include <iomanip>
int main(){
  N=25;
  for (int n=1; n<N/2; n++){
    G_hyp(n);
    for (int g=0; g<N-3; g++) std::cout << std::setw(4) << hyp[g];
    std::cout << std::endl;
  }
}

```

### Output

```txt

   2   2   3   3   4   4   5   5   6   6   7   7   8   8   9   9  10  10  11  11  12  12
   2   3   4   5   7   8  10  12  14  16  19  21  24  27  30  33  37  40  44  48  52  12
   2   3   5   6   9  11  15  18  23  27  34  39  47  54  64  72  84  94 108 120  52  12
   2   3   5   7  10  13  18  23  30  37  47  57  70  84 101 119 141 164 192 120  52  12
   2   3   5   7  11  14  20  26  35  44  58  71  90 110 136 163 199 235 192 120  52  12
   2   3   5   7  11  15  21  28  38  49  65  82 105 131 164 201 248 235 192 120  52  12
   2   3   5   7  11  15  22  29  40  52  70  89 116 146 186 230 248 235 192 120  52  12
   2   3   5   7  11  15  22  30  41  54  73  94 123 157 201 230 248 235 192 120  52  12
   2   3   5   7  11  15  22  30  42  55  75  97 128 164 201 230 248 235 192 120  52  12
   2   3   5   7  11  15  22  30  42  56  76  99 131 164 201 230 248 235 192 120  52  12
   2   3   5   7  11  15  22  30  42  56  77 100 131 164 201 230 248 235 192 120  52  12

```


:The first row is the hypotenuse of the green triangle.
:The second row cols 2 to 21 is the hypotenuse 1 in. Col 1 is the last entry in the horizontal edge of the grey triangle. Col 22 is the first entry of the horizontal edge of the green triangle.
:With subsequent calls the horizontal edges expand until, on the final row, the sequence of hypotenuses is finished and hyp contains the horizontal edge of the OTT.

This must be the most beautiful thing on rosettacode!!! Note that the algorithm requires only this data, and requires only N/2 iterations with the nth iteration performing N-3-2*n calculations.

### The One True Triangle, OTT
The following will display OTT(25).

```cpp

int main(){
  N = 25;
  std::cout << std::setw(N+52) << "1" << std::endl;
  std::cout << std::setw(N+55) << "1     1" << std::endl;
  std::cout << std::setw(N+58) << "1     1     1" << std::endl;
  std::string ott[N-3];
  for (int n=1; n<N/2; n++) {
    G_hyp(n);
    for (int g=(n-1)*2; g<N-3; g++) {
      std::string t = hyp[g-(n-1)].get_str();
      //if (t.size()==1) t.insert(t.begin(),1,' ');
      ott[g].append(t);
      ott[g].append(6-t.size(),' ');
    }
  }
  for(int n = 0; n<N-3; n++) {
    std::cout <<std::setw(N+43-3*n) << 1 << "     " << ott[n];
    for (int g = (n+1)/2; g>0; g--) {
      std::string t{hyp[g-1].get_str()};
      t.append(6-t.size(),' ');
      std::cout << t;
    }
    std::cout << "1     1" << std::endl;
  }

```

### Output

```txt

                                                                            1
                                                                         1     1
                                                                      1     1     1
                                                                   1     2     1     1
                                                                1     2     2     1     1
                                                             1     3     3     2     1     1
                                                          1     3     4     3     2     1     1
                                                       1     4     5     5     3     2     1     1
                                                    1     4     7     6     5     3     2     1     1
                                                 1     5     8     9     7     5     3     2     1     1
                                              1     5     10    11    10    7     5     3     2     1     1
                                           1     6     12    15    13    11    7     5     3     2     1     1
                                        1     6     14    18    18    14    11    7     5     3     2     1     1
                                     1     7     16    23    23    20    15    11    7     5     3     2     1     1
                                  1     7     19    27    30    26    21    15    11    7     5     3     2     1     1
                               1     8     21    34    37    35    28    22    15    11    7     5     3     2     1     1
                            1     8     24    39    47    44    38    29    22    15    11    7     5     3     2     1     1
                         1     9     27    47    57    58    49    40    30    22    15    11    7     5     3     2     1     1
                      1     9     30    54    70    71    65    52    41    30    22    15    11    7     5     3     2     1     1
                   1     10    33    64    84    90    82    70    54    42    30    22    15    11    7     5     3     2     1     1
                1     10    37    72    101   110   105   89    73    55    42    30    22    15    11    7     5     3     2     1     1
             1     11    40    84    119   136   131   116   94    75    56    42    30    22    15    11    7     5     3     2     1     1
          1     11    44    94    141   163   164   146   123   97    76    56    42    30    22    15    11    7     5     3     2     1     1
       1     12    48    108   164   199   201   186   157   128   99    77    56    42    30    22    15    11    7     5     3     2     1     1
    1     12    52    120   192   235   248   230   201   164   131   100   77    56    42    30    22    15    11    7     5     3     2     1     1

```



### Values of Integer Partition Function

Values of the Integer Partition function may be extracted as follows:

```cpp

#include <iostream>
int main(){
  for (int n=1; n<N/2; n++) G_hyp(n);
  std::cout << "G(23)     = " << hyp[21] << std::endl;
  std::cout << "G(123)    = " << hyp[121] << std::endl;
  std::cout << "G(1234)   = " << hyp[1232] << std::endl;
  std::cout << "G(12345)  = " << hyp[12343] << std::endl;
  mpz_class r{3};
  for (int i = 0; i<N-3; i++) r += hyp[i];
  std::cout << "G(123456) = " << r << std::endl;
}

```

### Output

```txt

G(23)     = 1255
G(123)    = 2552338241
G(1234)   = 156978797223733228787865722354959930
G(12345)  = 69420357953926116819562977205209384460667673094671463620270321700806074195845953959951425306140971942519870679768681736
G(123456) = 30817659578536496678545317146533980855296613274507139217608776782063054452191537379312358383342446230621170608408020911309259407611257151683372221925128388387168451943800027128045369650890220060901494540459081545445020808726917371699102825508039173543836338081612528477859613355349851184591540231790254269948278726548570660145691076819912972162262902150886818986555127204165221706149989

```



## Clojure


```clojure
(defn nine-billion-names [row column]
  (cond (<= row 0) 0
        (<= column 0) 0
        (< row column) 0
        (= row 1) 1
        :else (let [addend (nine-billion-names (dec row) (dec column))
                    augend (nine-billion-names (- row column) column)]
	            (+ addend augend))))

(defn print-row [row]
  (doseq [x (range 1 (inc row))]
    (print (nine-billion-names row x) \space))
    (println))

(defn print-triangle [rows]
  (doseq [x (range 1 (inc rows))]
    (print-row x)))

(print-triangle 25)
```



## Common Lisp


```lisp
(defun 9-billion-names (row column)
  (cond ((<= row 0) 0)
        ((<= column 0) 0)
	((< row column) 0)
	((equal row 1) 1)
	(t (let ((addend (9-billion-names (1- row) (1- column)))
		 (augend (9-billion-names (- row column) column)))
	     (+ addend augend)))))

(defun 9-billion-names-triangle (rows)
  (loop for row from 1 to rows
     collect (loop for column from 1 to row
		collect (9-billion-names row column))))

(9-billion-names-triangle 25)

```



## Crystal

{{trans|Ruby}}

### Naive Solution


```ruby
def g(n,g)
  return 1 unless 1 < g && g < n-1
  (2..g).reduce(1){|res,q| res + (q > n-g ? 0 : g(n-g,q))}
end

(1..25).each {|n|
  puts (1..n).map {|g| "%4s" % g(n,g)}.join
}

```

### Output

```txt

   1
   1   1
   1   1   1
   1   2   1   1
   1   2   2   1   1
   1   3   3   2   1   1
   1   3   4   3   2   1   1
   1   4   5   5   3   2   1   1
   1   4   7   6   5   3   2   1   1
   1   5   8   9   7   5   3   2   1   1
   1   5  10  11  10   7   5   3   2   1   1
   1   6  12  15  13  11   7   5   3   2   1   1
   1   6  14  18  18  14  11   7   5   3   2   1   1
   1   7  16  23  23  20  15  11   7   5   3   2   1   1
   1   7  19  27  30  26  21  15  11   7   5   3   2   1   1
   1   8  21  34  37  35  28  22  15  11   7   5   3   2   1   1
   1   8  24  39  47  44  38  29  22  15  11   7   5   3   2   1   1
   1   9  27  47  57  58  49  40  30  22  15  11   7   5   3   2   1   1
   1   9  30  54  70  71  65  52  41  30  22  15  11   7   5   3   2   1   1
   1  10  33  64  84  90  82  70  54  42  30  22  15  11   7   5   3   2   1   1
   1  10  37  72 101 110 105  89  73  55  42  30  22  15  11   7   5   3   2   1   1
   1  11  40  84 119 136 131 116  94  75  56  42  30  22  15  11   7   5   3   2   1   1
   1  11  44  94 141 163 164 146 123  97  76  56  42  30  22  15  11   7   5   3   2   1   1
   1  12  48 108 164 199 201 186 157 128  99  77  56  42  30  22  15  11   7   5   3   2   1   1
   1  12  52 120 192 235 248 230 201 164 131 100  77  56  42  30  22  15  11   7   5   3   2   1   1

```



## D


### Producing rows

{{trans|Python}}

```d
import std.stdio, std.bigint, std.algorithm, std.range;

auto cumu(in uint n) {
    __gshared cache = [[1.BigInt]];
    foreach (l; cache.length .. n + 1) {
        auto r = [0.BigInt];
        foreach (x; 1 .. l + 1)
            r ~= r.back + cache[l - x][min(x, l - x)];
        cache ~= r;
    }
    return cache[n];
}

auto row(in uint n) {
    auto r = n.cumu;
    return n.iota.map!(i => r[i + 1] - r[i]);
}

void main() {
    writeln("Rows:");
    foreach (x; 1 .. 11)
        writefln("%2d: %s", x, x.row);

    writeln("\nSums:");
    foreach (x; [23, 123, 1234])
        writeln(x, " ", x.cumu.back);
}
```

### Output

```txt
Rows:
 1: [1]
 2: [1, 1]
 3: [1, 1, 1]
 4: [1, 2, 1, 1]
 5: [1, 2, 2, 1, 1]
 6: [1, 3, 3, 2, 1, 1]
 7: [1, 3, 4, 3, 2, 1, 1]
 8: [1, 4, 5, 5, 3, 2, 1, 1]
 9: [1, 4, 7, 6, 5, 3, 2, 1, 1]
10: [1, 5, 8, 9, 7, 5, 3, 2, 1, 1]

Sums:
23 1255
123 2552338241
1234 156978797223733228787865722354959930
```




### Only partition functions

{{trans|C}}

```d
import std.stdio, std.bigint, std.algorithm;

struct Names {
    BigInt[] p = [1.BigInt];

    int opApply(int delegate(ref immutable int, ref BigInt) dg) {
        int result;

        foreach (immutable n; 1 .. int.max) {
            p.assumeSafeAppend;
            p ~= 0.BigInt;

            foreach (immutable k; 1 .. n + 1) {
                auto d = n - k * (3 * k - 1) / 2;
                if (d < 0)
                    break;

                if (k & 1)
                    p[n] += p[d];
                else
                    p[n] -= p[d];

                d -= k;
                if (d < 0)
                    break;

                if (k & 1)
                    p[n] += p[d];
                else
                    p[n] -= p[d];
            }

            result = dg(n, p[n]);
            if (result) break;
        }

        return result;
    }
}

void main() {
    immutable ns = [23:0, 123:0, 1234:0, 12345:0];
    immutable maxNs = ns.byKey.reduce!max;

    foreach (immutable i, p; Names()) {
        if (i > maxNs)
            break;
        if (i in ns)
            writefln("%6d: %s", i, p);
    }
}
```

### Output

```txt
    23: 1255
   123: 2552338241
  1234: 156978797223733228787865722354959930
 12345: 69420357953926116819562977205209384460667673094671463620270321700806074195845953959951425306140971942519870679768681736
```

### Output for a larger input, with newlines:

```txt
123456:
3081765957853649667854531714653398085529661327450713921760877
6782063054452191537379312358383342446230621170608408020911309
2594076112571516833722219251283883871684519438000271280453696
5089022006090149454045908154544502080872691737169910282550803
9173543836338081612528477859613355349851184591540231790254269
9482787265485706601456910768199129721622629021508868189865551
27204165221706149989
```

Runtime up to 123456: about 56 seconds (about 50 with ldc2) because currently std.bigint is not fast.


## Dart


*Works with: Dart 2*

{{trans|Python}}


```Dart
import 'dart:math';

List<BigInt> partitions(int n) {
  var cache = List<List<BigInt>>.filled(1, List<BigInt>.filled(1, BigInt.from(1)), growable: true);
  for(int length = cache.length; length < n + 1; length++) {
    var row = List<BigInt>.filled(1, BigInt.from(0), growable: true);
    for(int index = 1; index < length + 1; index++) {
      var partAtIndex = row[row.length - 1] + cache[length - index][min(index, length - index)];
      row.add(partAtIndex);
    }
    cache.add(row);
  }
  return cache[n];
}

List<BigInt> row(int n) {
  var parts = partitions(n);
  return List<BigInt>.generate(n, (int index) => parts[index + 1] - parts[index]);
}

void printRows({int min = 1, int max = 11}) {
  int maxDigits = max.toString().length;
  print('Rows:');
  for(int i in List.generate(max - min, (int index) => index + min)) {
    print((' ' * (maxDigits - i.toString().length)) + '$i: ${row(i)}');
  }
}

void printSums(List<int> args) {
  print('Sums:');
  for(int i in args) {
    print('$i: ${partitions(i)[i]}');
  }
}
```


In main:

```Dart
 import 'package:DD1_NamesOfGod/DD1_NamesOfGod.dart' as names_of_god;

main(List<String> arguments) {
  names_of_god.printRows(min: 1, max: 11);
  names_of_god.printSums([23, 123, 1234, 12345]);
}
```


### Output

```txt
 Rows:
 1: [1]
 2: [1, 1]
 3: [1, 1, 1]
 4: [1, 2, 1, 1]
 5: [1, 2, 2, 1, 1]
 6: [1, 3, 3, 2, 1, 1]
 7: [1, 3, 4, 3, 2, 1, 1]
 8: [1, 4, 5, 5, 3, 2, 1, 1]
 9: [1, 4, 7, 6, 5, 3, 2, 1, 1]
10: [1, 5, 8, 9, 7, 5, 3, 2, 1, 1]
Sums:
23: 1255
123: 2552338241
1234: 156978797223733228787865722354959930
12345: 69420357953926116819562977205209384460667673094671463620270321700806074195845953959951425306140971942519870679768681736

```




## Dyalect



```Dyalect
var cache = [[1]]

func min(x, y) {
    if x < y {
        x
    } else {
        y
    }
}

func namesOfGod(n) {
    for l in cache.len()..n {
        var r = [0]
        for x in 1..l {
            r.add(r[r.len() - 1] + cache[l - x][min(x, l-x)])
        }
        cache.add(r)
    }
    return cache[n]
}

func row(n) {
    const r = namesOfGod(n)
    var returnArray = []
    for i in 0..(n - 1) {
        returnArray.add(r[i + 1] - r[i])
    }
    return returnArray
}

for x in 1..25 {
    print("\(x): \(row(x))")
}
```


Output:


```txt
1: [1]
2: [1, 1]
3: [1, 1, 1]
4: [1, 2, 1, 1]
5: [1, 2, 2, 1, 1]
6: [1, 3, 3, 2, 1, 1]
7: [1, 3, 4, 3, 2, 1, 1]
8: [1, 4, 5, 5, 3, 2, 1, 1]
9: [1, 4, 7, 6, 5, 3, 2, 1, 1]
10: [1, 5, 8, 9, 7, 5, 3, 2, 1, 1]
11: [1, 5, 10, 11, 10, 7, 5, 3, 2, 1, 1]
12: [1, 6, 12, 15, 13, 11, 7, 5, 3, 2, 1, 1]
13: [1, 6, 14, 18, 18, 14, 11, 7, 5, 3, 2, 1, 1]
14: [1, 7, 16, 23, 23, 20, 15, 11, 7, 5, 3, 2, 1, 1]
15: [1, 7, 19, 27, 30, 26, 21, 15, 11, 7, 5, 3, 2, 1, 1]
16: [1, 8, 21, 34, 37, 35, 28, 22, 15, 11, 7, 5, 3, 2, 1, 1]
17: [1, 8, 24, 39, 47, 44, 38, 29, 22, 15, 11, 7, 5, 3, 2, 1, 1]
18: [1, 9, 27, 47, 57, 58, 49, 40, 30, 22, 15, 11, 7, 5, 3, 2, 1, 1]
19: [1, 9, 30, 54, 70, 71, 65, 52, 41, 30, 22, 15, 11, 7, 5, 3, 2, 1, 1]
20: [1, 10, 33, 64, 84, 90, 82, 70, 54, 42, 30, 22, 15, 11, 7, 5, 3, 2, 1, 1]
21: [1, 10, 37, 72, 101, 110, 105, 89, 73, 55, 42, 30, 22, 15, 11, 7, 5, 3, 2, 1, 1]
22: [1, 11, 40, 84, 119, 136, 131, 116, 94, 75, 56, 42, 30, 22, 15, 11, 7, 5, 3, 2, 1, 1]
23: [1, 11, 44, 94, 141, 163, 164, 146, 123, 97, 76, 56, 42, 30, 22, 15, 11, 7, 5, 3, 2, 1, 1]
24: [1, 12, 48, 108, 164, 199, 201, 186, 157, 128, 99, 77, 56, 42, 30, 22, 15, 11, 7, 5, 3, 2, 1, 1]
25: [1, 12, 52, 120, 192, 235, 248, 230, 201, 164, 131, 100, 77, 56, 42, 30, 22, 15, 11, 7, 5, 3, 2, 1, 1]
```



## Elixir

{{trans|Ruby}}
Naive Solution

```elixir
defmodule God do
  def g(n,g) when g == 1 or n < g, do: 1
  def g(n,g) do
    Enum.reduce(2..g, 1, fn q,res ->
      res + (if q > n-g, do: 0, else: g(n-g,q))
    end)
  end
end

Enum.each(1..25, fn n ->
  IO.puts Enum.map(1..n, fn g -> "#{God.g(n,g)} " end)
end)
```


### Output

```txt

1
1 1
1 1 1
1 2 1 1
1 2 2 1 1
1 3 3 2 1 1
1 3 4 3 2 1 1
1 4 5 5 3 2 1 1
1 4 7 6 5 3 2 1 1
1 5 8 9 7 5 3 2 1 1
1 5 10 11 10 7 5 3 2 1 1
1 6 12 15 13 11 7 5 3 2 1 1
1 6 14 18 18 14 11 7 5 3 2 1 1
1 7 16 23 23 20 15 11 7 5 3 2 1 1
1 7 19 27 30 26 21 15 11 7 5 3 2 1 1
1 8 21 34 37 35 28 22 15 11 7 5 3 2 1 1
1 8 24 39 47 44 38 29 22 15 11 7 5 3 2 1 1
1 9 27 47 57 58 49 40 30 22 15 11 7 5 3 2 1 1
1 9 30 54 70 71 65 52 41 30 22 15 11 7 5 3 2 1 1
1 10 33 64 84 90 82 70 54 42 30 22 15 11 7 5 3 2 1 1
1 10 37 72 101 110 105 89 73 55 42 30 22 15 11 7 5 3 2 1 1
1 11 40 84 119 136 131 116 94 75 56 42 30 22 15 11 7 5 3 2 1 1
1 11 44 94 141 163 164 146 123 97 76 56 42 30 22 15 11 7 5 3 2 1 1
1 12 48 108 164 199 201 186 157 128 99 77 56 42 30 22 15 11 7 5 3 2 1 1
1 12 52 120 192 235 248 230 201 164 131 100 77 56 42 30 22 15 11 7 5 3 2 1 1

```



## Erlang


Step 1: Print the pyramid for a smallish number of names. The P function is implement as described on [partition function](http://mathworld.wolfram.com/PartitionFunctionP.html), (see 59 on that page). This is slow for N > 100, but works fine for the example: 10.

```Erlang

-module(triangle).
-export([start/1]).
start(N)->
  print(1,1,N).
print(N,N,N)->
1;
print(A,B,N) when A>=B->
   io:format("~p ",[formula(A,B)]),
   print(A,B+1,N);
print(A,B,N) when B>A->
   io:format("~n"),
   print(A+1,1,N).

formula(_,0)->
  0;
formula(B,B)->
  1;
formula(A,B) when B>A->
  0;
formula(A1,B1)->
  formula(A1-1,B1-1)+formula(A1-B1,B1).

```


### Output
If user inputs 25, the result shall be:

```txt

1
1 1
1 1 1
1 2 1 1
1 2 2 1 1
1 3 3 2 1 1
1 3 4 3 2 1 1
1 4 5 5 3 2 1 1
1 4 7 6 5 3 2 1 1
1 5 8 9 7 5 3 2 1 1
1 5 10 11 10 7 5 3 2 1 1
1 6 12 15 13 11 7 5 3 2 1 1
1 6 14 18 18 14 11 7 5 3 2 1 1
1 7 16 23 23 20 15 11 7 5 3 2 1 1
1 7 19 27 30 26 21 15 11 7 5 3 2 1 1
1 8 21 34 37 35 28 22 15 11 7 5 3 2 1 1
1 8 24 39 47 44 38 29 22 15 11 7 5 3 2 1 1
1 9 27 47 57 58 49 40 30 22 15 11 7 5 3 2 1 1
1 9 30 54 70 71 65 52 41 30 22 15 11 7 5 3 2 1 1
1 10 33 64 84 90 82 70 54 42 30 22 15 11 7 5 3 2 1 1
1 10 37 72 101 110 105 89 73 55 42 30 22 15 11 7 5 3 2 1 1
1 11 40 84 119 136 131 116 94 75 56 42 30 22 15 11 7 5 3 2 1 1
1 11 44 94 141 163 164 146 123 97 76 56 42 30 22 15 11 7 5 3 2 1 1
1 12 48 108 164 199 201 186 157 128 99 77 56 42 30 22 15 11 7 5 3 2 1 1
1 12 52 120 192 235 248 230 201 164 131 100 77 56 42 30 22 15 11 7 5 3 2 1 1

```



## Forth


```txt

NEEDS -xopg
ANEW -nbnog \ The Nine Billion Names of God
.arbitrary.p

#100000 =: N
CREATE idx[ #23 , #123 , #1234 , #12345 , #20000 , #30000 , #40000 , #50000 , N , 0 ,
N GARRAY p

: CALC ( n -- )
	0 LOCALS| d n |
	n 1+ 1 ?DO  I 3 * 1-  I 2 */  n SWAP -  TO d   d 0< ?LEAVE
		    I 1 AND IF  LET p[n]=p[n]+p[d]: ELSE  LET p[n]=p[n]-p[d]: ENDIF
		    I -TO d   d 0< ?LEAVE
		    I 1 AND IF  LET p[n]=p[n]+p[d]: ELSE  LET p[n]=p[n]-p[d]: ENDIF
	      LOOP ;

: .GOD ( -- )
	0 LOCAL at
	LET p[0]=1: N 1 DO  I CALC
			    idx[ at CELL[] @ I = IF  CR I 5 .R ." : " LET. p[I]:
			    			     1 +TO at
			    		      ENDIF
		      LOOP ;

: .ABOUT ( -- ) ." Try: .GOD" ;

```


### Output


```txt

FORTH> .god
   23:  1.2550000000000000000000000000000000000000e+0003
  123:  2.5523382410000000000000000000000000000000e+0009
 1234:  1.5697879722373322878786572235495993000000e+0035
12345:  6.9420357953926116819562977205209384460667e+0118
20000:  2.5211481381252969791661953323047045228132e+0152
30000:  4.2963584246325385174883157483005920912690e+0187
40000:  2.2807728274470728289340571240816959704646e+0217
50000:  3.6261860971416678445921408915956337281653e+0243 ok

```


## FreeBASIC

*Library: GMP*

```freebasic
' version 03-11-2016
' compile with: fbc -s console

#Include Once "gmp.bi"

Sub partitions(max As ULong, p() As MpZ_ptr)
    ' based on Numericana code example
    Dim As ULong a, b, i, k
    Dim As Long j

    Dim As Mpz_ptr s = Allocate(Len(__mpz_struct)) : Mpz_init(s)

    Mpz_set_ui(p(0), 1)

    For i = 1 To max
        j = 1 : k = 1 : b = 2 : a = 5
        While j > 0
            ' j = i - (3*k*k+k) \ 2
            j = i - b : b = b + a : a = a + 3
            If j >= 0 Then
                If k And 1 Then Mpz_add(s, s, p(j)) Else Mpz_sub(s, s, p(j))
            End If
            j = j + k
            If j >= 0 Then
                If k And 1 Then Mpz_add(s, s, p(j)) Else Mpz_sub(s, s, p(j))
            End If
            k = k +1
        Wend
        Mpz_swap(p(i), s)
    Next

    Mpz_clear(s)

End Sub

' ------=< MAIN >=------

Dim As ULong n, k, max = 25              ' with max > 479 the numbers become
Dim As ULongInt p(max, max)              ' to big for a 64bit unsigned integer

p(1, 1) = 1                              ' fill the first 3 rows
p(2, 1) = 1 : p(2, 2) = 1
p(3, 1) = 1 : p(3, 2) = 1 : p(3, 3) = 1

For n = 4 To max                         ' fill the rest
    For k = 1 To n
        If k * 2 > n  Then
           p(n,k)= p(n-1,k-1)
        Else
           p(n,k) = p(n-1,k-1) + p(n-k, k)
        End If
    Next
Next

For n = 1 To 25                          ' print the triangle
    Print Space((max - n) * 2);
    For k = 1 To n
        Print Using "####"; p(n, k);
    Next
    Print
Next
Print : print

                                         ' calculate the integer partition
max = 123456                             ' 1234567 takes about ten minutes
Dim As ZString Ptr ans

ReDim big_p(max) As Mpz_ptr
For n = 0 To max
    big_p(n) = Allocate(Len(__mpz_struct)) : Mpz_init(big_p(n))
Next

partitions(max, big_p())

For n = 1 To Len(Str(max))
    k = Val(Left(Str(max), n))
    ans = Mpz_get_str (0, 10, big_p(k))
    Print Space(10 - n); "P("; Str(k); ") = "; *ans
Next

For n = 0 To max
    Mpz_clear(big_p(n))
Next

' empty keyboard buffer
While InKey <> "" : Wend
Print : Print "hit any key to end program"
Sleep
End
```

### Output

```txt
                                                   1
                                                 1   1
                                               1   1   1
                                             1   2   1   1
                                           1   2   2   1   1
                                         1   3   3   2   1   1
                                       1   3   4   3   2   1   1
                                     1   4   5   5   3   2   1   1
                                   1   4   7   6   5   3   2   1   1
                                 1   5   8   9   7   5   3   2   1   1
                               1   5  10  11  10   7   5   3   2   1   1
                             1   6  12  15  13  11   7   5   3   2   1   1
                           1   6  14  18  18  14  11   7   5   3   2   1   1
                         1   7  16  23  23  20  15  11   7   5   3   2   1   1
                       1   7  19  27  30  26  21  15  11   7   5   3   2   1   1
                     1   8  21  34  37  35  28  22  15  11   7   5   3   2   1   1
                   1   8  24  39  47  44  38  29  22  15  11   7   5   3   2   1   1
                 1   9  27  47  57  58  49  40  30  22  15  11   7   5   3   2   1   1
               1   9  30  54  70  71  65  52  41  30  22  15  11   7   5   3   2   1   1
             1  10  33  64  84  90  82  70  54  42  30  22  15  11   7   5   3   2   1   1
           1  10  37  72 101 110 105  89  73  55  42  30  22  15  11   7   5   3   2   1   1
         1  11  40  84 119 136 131 116  94  75  56  42  30  22  15  11   7   5   3   2   1   1
       1  11  44  94 141 163 164 146 123  97  76  56  42  30  22  15  11   7   5   3   2   1   1
     1  12  48 108 164 199 201 186 157 128  99  77  56  42  30  22  15  11   7   5   3   2   1   1
   1  12  52 120 192 235 248 230 201 164 131 100  77  56  42  30  22  15  11   7   5   3   2   1   1


         P(1) = 1
        P(12) = 77
       P(123) = 2552338241
      P(1234) = 156978797223733228787865722354959930
     P(12345) = 69420357953926116819562977205209384460667673094671463620270321700806074195845953959951425306140971942519870679768681736
    P(123456) = 30817659578536496678545317146533980855296613274507139217608776782063054452191537379312358383342446230621170608408020911309259407611257151683372221925128388387168451943800027128045369650890220060901494540459081545445020808726917371699102825508039173543836338081612528477859613355349851184591540231790254269948278726548570660145691076819912972162262902150886818986555127204165221706149989
```



## Frink

This demonstrates using a class that memoizes results to improve efficiency and reduce later calculation.  It verifies its results against Frink's built-in and much more memory-and-space-efficient partitionCount function which uses Euler's pentagonal method for counting partitions.


```frink

class PartitionCount
{
   // Array of elements
   class var triangle = [[0],[0,1]]

   // Array of cumulative sums in each row.
   class var sumTriangle = [[1],[0,1]]

   class calcRowsTo[toRow] :=
   {
      for row = length[triangle] to toRow
      {
         triangle@row = workrow = new array[[row+1],0]
         sumTriangle@row = sumworkrow = new array[[row+1],0]
         oversum = 0
         for col = 1 to row
         {
            otherRow = row-col
            sum = sumTriangle@otherRow@min[col,otherRow]
            workrow@col = sum
            oversum = oversum + sum
            sumworkrow@col = oversum
         }
      }
   }

   class rowSum[row] :=
   {
      calcRowsTo[row]
      return sumTriangle@row@row
   }
}

PartitionCount.calcRowsTo[25]
for row=1 to 25
{
   for col=1 to row
      print[PartitionCount.triangle@row@col + " "]
   println[]
}

// Test against Frink's built-in much faster partitionCount function that uses
// Euler's pentagonal method for counting partitions.
testRow[row] :=
{
   sum = PartitionCount.rowSum[row]
   println["$row\t$sum\t" + (sum == partitionCount[row] ? "correct" : "incorrect")]
}

println[]
testRow[23]
testRow[123]
testRow[1234]
testRow[12345]

```



```txt

1
1 1
1 1 1
1 2 1 1
1 2 2 1 1
1 3 3 2 1 1
1 3 4 3 2 1 1
1 4 5 5 3 2 1 1
1 4 7 6 5 3 2 1 1
1 5 8 9 7 5 3 2 1 1
1 5 10 11 10 7 5 3 2 1 1
1 6 12 15 13 11 7 5 3 2 1 1
1 6 14 18 18 14 11 7 5 3 2 1 1
1 7 16 23 23 20 15 11 7 5 3 2 1 1
1 7 19 27 30 26 21 15 11 7 5 3 2 1 1
1 8 21 34 37 35 28 22 15 11 7 5 3 2 1 1
1 8 24 39 47 44 38 29 22 15 11 7 5 3 2 1 1
1 9 27 47 57 58 49 40 30 22 15 11 7 5 3 2 1 1
1 9 30 54 70 71 65 52 41 30 22 15 11 7 5 3 2 1 1
1 10 33 64 84 90 82 70 54 42 30 22 15 11 7 5 3 2 1 1
1 10 37 72 101 110 105 89 73 55 42 30 22 15 11 7 5 3 2 1 1
1 11 40 84 119 136 131 116 94 75 56 42 30 22 15 11 7 5 3 2 1 1
1 11 44 94 141 163 164 146 123 97 76 56 42 30 22 15 11 7 5 3 2 1 1
1 12 48 108 164 199 201 186 157 128 99 77 56 42 30 22 15 11 7 5 3 2 1 1
1 12 52 120 192 235 248 230 201 164 131 100 77 56 42 30 22 15 11 7 5 3 2 1 1

23	1255	correct
123	2552338241	correct
1234	156978797223733228787865722354959930	correct
12345	69420357953926116819562977205209384460667673094671463620270321700806074195845953959951425306140971942519870679768681736	correct

```


##  ## GAP
The partition function is built-in.

```gap
PrintArray(List([1 .. 25], n -> List([1 .. n], k -> NrPartitions(n, k))));

[ [    1 ],
  [    1,    1 ],
  [    1,    1,    1 ],
  [    1,    2,    1,    1 ],
  [    1,    2,    2,    1,    1 ],
  [    1,    3,    3,    2,    1,    1 ],
  [    1,    3,    4,    3,    2,    1,    1 ],
  [    1,    4,    5,    5,    3,    2,    1,    1 ],
  [    1,    4,    7,    6,    5,    3,    2,    1,    1 ],
  [    1,    5,    8,    9,    7,    5,    3,    2,    1,    1 ],
  [    1,    5,   10,   11,   10,    7,    5,    3,    2,    1,    1 ],
  [    1,    6,   12,   15,   13,   11,    7,    5,    3,    2,    1,    1 ],
  [    1,    6,   14,   18,   18,   14,   11,    7,    5,    3,    2,    1,    1 ],
  [    1,    7,   16,   23,   23,   20,   15,   11,    7,    5,    3,    2,    1,    1 ],
  [    1,    7,   19,   27,   30,   26,   21,   15,   11,    7,    5,    3,    2,    1,    1 ],
  [    1,    8,   21,   34,   37,   35,   28,   22,   15,   11,    7,    5,    3,    2,    1,    1 ],
  [    1,    8,   24,   39,   47,   44,   38,   29,   22,   15,   11,    7,    5,    3,    2,    1,    1 ],
  [    1,    9,   27,   47,   57,   58,   49,   40,   30,   22,   15,   11,    7,    5,    3,    2,    1,    1 ],
  [    1,    9,   30,   54,   70,   71,   65,   52,   41,   30,   22,   15,   11,    7,    5,    3,    2,    1,    1 ],
  [    1,   10,   33,   64,   84,   90,   82,   70,   54,   42,   30,   22,   15,   11,    7,    5,    3,    2,    1,    1 ],
  [    1,   10,   37,   72,  101,  110,  105,   89,   73,   55,   42,   30,   22,   15,   11,    7,    5,    3,    2,    1,    1 ],
  [    1,   11,   40,   84,  119,  136,  131,  116,   94,   75,   56,   42,   30,   22,   15,   11,    7,    5,    3,    2,    1,    1 ],
  [    1,   11,   44,   94,  141,  163,  164,  146,  123,   97,   76,   56,   42,   30,   22,   15,   11,    7,    5,    3,    2,    1,    1 ],
  [    1,   12,   48,  108,  164,  199,  201,  186,  157,  128,   99,   77,   56,   42,   30,   22,   15,   11,    7,    5,    3,    2,    1,    1 ],
  [    1,   12,   52,  120,  192,  235,  248,  230,  201,  164,  131,  100,   77,   56,   42,   30,   22,   15,   11,    7,    5,    3,    2,    1,    1 ] ]


List([23, 123, 1234, 12345], NrPartitions);

[ 1255, 2552338241, 156978797223733228787865722354959930,
  69420357953926116819562977205209384460667673094671463620270321700806074195845953959951425306140971942519870679768681736 ]
```



## Go


```go
package main

import (
	"fmt"
	"math/big"
)

func main() {

	intMin := func(a, b int) int {
		if a < b {
			return a
		} else {
			return b
		}
	}

	var cache = [][]*big.Int{{big.NewInt(1)}}

	cumu := func(n int) []*big.Int {
		for y := len(cache); y <= n; y++ {
			row := []*big.Int{big.NewInt(0)}
			for x := 1; x <= y; x++ {
				cacheValue := cache[y-x][intMin(x, y-x)]
				row = append(row, big.NewInt(0).Add(row[len(row)-1], cacheValue))
			}
			cache = append(cache, row)
		}
		return cache[n]
	}

	row := func(n int) {
		e := cumu(n)
		for i := 0; i < n; i++ {
			fmt.Printf(" %v ", (big.NewInt(0).Sub(e[i+1], e[i])).Text(10))
		}
		fmt.Println()
	}

	fmt.Println("rows:")
	for x := 1; x < 11; x++ {
		row(x)
	}
	fmt.Println()

	fmt.Println("sums:")
	for _, num := range [...]int{23, 123, 1234, 12345} {
		r := cumu(num)
		fmt.Printf("%d %v\n", num, r[len(r)-1].Text(10))
	}
}
```

### Output

```txt

rows:
 1
 1  1
 1  1  1
 1  2  1  1
 1  2  2  1  1
 1  3  3  2  1  1
 1  3  4  3  2  1  1
 1  4  5  5  3  2  1  1
 1  4  7  6  5  3  2  1  1
 1  5  8  9  7  5  3  2  1  1

 sums:
23 1255
123 2552338241
1234 156978797223733228787865722354959930
12345 69420357953926116819562977205209384460667673094671463620270321700806074195845953959951425306140971942519870679768681736

```



## Groovy


```groovy

def partitions(c)
{

    def p=[];
    int k = 0;
     p[k] = c;
    int counter=0;
    def counts=[];
	for (i in 0..c-1)
	{counts[i]=0;}
    while (true)
    {

        counter++;
		counts[p[0]-1]=counts[p[0]-1]+1;
		int rem_val = 0;
        while (k >= 0 && p[k] == 1)
        { rem_val += p[k];
            k--;}
        if (k < 0)  { break;}
        p[k]--;
        rem_val++;
        while (rem_val > p[k])
        {
            p[k+1] = p[k];
            rem_val = rem_val - p[k];
            k++;
        }
        p[k+1] = rem_val;
        k++;
    }
	println counts;
	return counter;
}


static void  main(String[] args)
{
for( i in 1..25 )
{partitions(i);}
}

```

### Output

```txt

[1]
[1, 1]
[1, 1, 1]
[1, 2, 1, 1]
[1, 2, 2, 1, 1]
[1, 3, 3, 2, 1, 1]
[1, 3, 4, 3, 2, 1, 1]
[1, 4, 5, 5, 3, 2, 1, 1]
[1, 4, 7, 6, 5, 3, 2, 1, 1]
[1, 5, 8, 9, 7, 5, 3, 2, 1, 1]
[1, 5, 10, 11, 10, 7, 5, 3, 2, 1, 1]
[1, 6, 12, 15, 13, 11, 7, 5, 3, 2, 1, 1]
[1, 6, 14, 18, 18, 14, 11, 7, 5, 3, 2, 1, 1]
[1, 7, 16, 23, 23, 20, 15, 11, 7, 5, 3, 2, 1, 1]
[1, 7, 19, 27, 30, 26, 21, 15, 11, 7, 5, 3, 2, 1, 1]
[1, 8, 21, 34, 37, 35, 28, 22, 15, 11, 7, 5, 3, 2, 1, 1]
[1, 8, 24, 39, 47, 44, 38, 29, 22, 15, 11, 7, 5, 3, 2, 1, 1]
[1, 9, 27, 47, 57, 58, 49, 40, 30, 22, 15, 11, 7, 5, 3, 2, 1, 1]
[1, 9, 30, 54, 70, 71, 65, 52, 41, 30, 22, 15, 11, 7, 5, 3, 2, 1, 1]
[1, 10, 33, 64, 84, 90, 82, 70, 54, 42, 30, 22, 15, 11, 7, 5, 3, 2, 1, 1]
[1, 10, 37, 72, 101, 110, 105, 89, 73, 55, 42, 30, 22, 15, 11, 7, 5, 3, 2, 1, 1]
[1, 11, 40, 84, 119, 136, 131, 116, 94, 75, 56, 42, 30, 22, 15, 11, 7, 5, 3, 2, 1, 1]
[1, 11, 44, 94, 141, 163, 164, 146, 123, 97, 76, 56, 42, 30, 22, 15, 11, 7, 5, 3, 2, 1, 1]
[1, 12, 48, 108, 164, 199, 201, 186, 157, 128, 99, 77, 56, 42, 30, 22, 15, 11, 7, 5, 3, 2, 1, 1]
[1, 12, 52, 120, 192, 235, 248, 230, 201, 164, 131, 100, 77, 56, 42, 30, 22, 15, 11, 7, 5, 3, 2, 1, 1]



```


## Haskell


```haskell
import Data.List (mapAccumL)

cumu :: [[Integer]]
cumu = [1] : map (scanl (+) 0) rows

rows :: [[Integer]]
rows = snd $ mapAccumL f [] cumu where
    f r row = (rr, new_row) where
        new_row = map head rr
        rr = map tailKeepOne (row:r)
    tailKeepOne [x] = [x]
    tailKeepOne (_:xs) = xs

sums n = cumu !! n !! n
--curiously, the following seems to be faster
--sums = sum . (rows!!)

main :: IO ()
main = do
    mapM_ print $ take 10 rows
    mapM_ (print.sums) [23, 123, 1234, 12345]
```

### Output

```txt

[1]
[1,1]
[1,1,1]
[1,2,1,1]
[1,2,2,1,1]
[1,3,3,2,1,1]
[1,3,4,3,2,1,1]
[1,4,5,5,3,2,1,1]
[1,4,7,6,5,3,2,1,1]
[1,5,8,9,7,5,3,2,1,1]
1255
2552338241
156978797223733228787865722354959930
^C (probably don't have enough memory for 12345 anyway)

```


## Icon and ## Unicon

This is a Unicon-specific solution.
{{trans|Python}}


```unicon
procedure main(A)
    n := integer(!A) | 10
    every r := 2 to (n+1) do write(right(r-1,2),": ",showList(row(r)))
    write()
    every r := 23 | 123 | 1234 | 12345 do write(r," ",cumu(r+1)[-1])
end

procedure cumu(n)
    static cache
    initial cache := [[1]]
    every l := *cache to n do {
        every (r := [0], x := !l) do put(r, r[-1]+cache[1+l-x][1+min(x,l-x)])
        put(cache, r)
        }
    return cache[n]
end

procedure row(n)
    return (r := cumu(n), [: (i := !(*r-1), r[i+1]-r[i]) :]) | r
end

procedure showList(A)
    every (s := "[") ||:= (!A||", ")
    return s[1:-2]||"]"
end
```


### Output (terminated without waiting for output of cumu(12345)):

```txt

->9bnogti
 1: [1]
 2: [1, 1]
 3: [1, 1, 1]
 4: [1, 2, 1, 1]
 5: [1, 2, 2, 1, 1]
 6: [1, 3, 3, 2, 1, 1]
 7: [1, 3, 4, 3, 2, 1, 1]
 8: [1, 4, 5, 5, 3, 2, 1, 1]
 9: [1, 4, 7, 6, 5, 3, 2, 1, 1]
10: [1, 5, 8, 9, 7, 5, 3, 2, 1, 1]

23 1255
123 2552338241
1234 156978797223733228787865722354959930
^C
->

```



## J

Recursive calculation of a row element:

```j
T=: 0:`1:`(($:&<:+ - $: ])`0:@.(0=]))@.(1+*@-) M. "0
```

Calculation of the triangle:

```j
rows=: <@(#~0<])@({: T ])\@i.
```

**Show triangle**:

```j
   ({.~1+1 i:~ '1'=])"1 ":> }.rows 1+10
1
1 1
1 1 1
1 2 1 1
1 2 2 1 1
1 3 3 2 1 1
1 3 4 3 2 1 1
1 4 5 5 3 2 1 1
1 4 7 6 5 3 2 1 1
1 5 8 9 7 5 3 2 1 1
```


Note that we've gone to extra work, here, in this **show triangle** example, to keep columns aligned when we have multi-digit values. But then we limited the result to one digit values because that is prettier.

Calculate row sums:

```j
rowSums=: 3 :0"0
  z=. (y+1){. 1x
  for_ks. <\1+i.y do.
    n=.{: k=.>ks
    r=.#c=. ({.~* i._1:)(n,0.5 _1.5) p. k
    s=.#d=.({.~* i._1:)c-r{.k
    'v i'=.|: \:~(c,d),. r ,&({.&k) s
    a=. +/(n{z),(_1^1x+2|i) * v{z
    z=. a n}z
  end.
)
```

### Output

```txt
   ({ [: rowSums >./) 3 23 123 1234
3 1255 2552338241 156978797223733228787865722354959930
```



## Java

Translation of [Python](/tasks/9_billion_names_of_God_the_integer#Python) via [D](/tasks/9_billion_names_of_God_the_integer#D)
*Works with: Java 8*

```java
import java.math.BigInteger;
import java.util.*;
import static java.util.Arrays.asList;
import static java.util.stream.Collectors.toList;
import static java.util.stream.IntStream.range;
import static java.lang.Math.min;

public class Test {

    static List<BigInteger> cumu(int n) {
        List<List<BigInteger>> cache = new ArrayList<>();
        cache.add(asList(BigInteger.ONE));

        for (int L = cache.size(); L < n + 1; L++) {
            List<BigInteger> r = new ArrayList<>();
            r.add(BigInteger.ZERO);
            for (int x = 1; x < L + 1; x++)
                r.add(r.get(r.size() - 1).add(cache.get(L - x).get(min(x, L - x))));
            cache.add(r);
        }
        return cache.get(n);
    }

    static List<BigInteger> row(int n) {
        List<BigInteger> r = cumu(n);
        return range(0, n).mapToObj(i -> r.get(i + 1).subtract(r.get(i)))
                .collect(toList());
    }

    public static void main(String[] args) {
        System.out.println("Rows:");
        for (int x = 1; x < 11; x++)
            System.out.printf("%2d: %s%n", x, row(x));

        System.out.println("\nSums:");
        for (int x : new int[]{23, 123, 1234}) {
            List<BigInteger> c = cumu(x);
            System.out.printf("%s %s%n", x, c.get(c.size() - 1));
        }
    }
}
```



```txt
Rows:
 1: [1]
 2: [1, 1]
 3: [1, 1, 1]
 4: [1, 2, 1, 1]
 5: [1, 2, 2, 1, 1]
 6: [1, 3, 3, 2, 1, 1]
 7: [1, 3, 4, 3, 2, 1, 1]
 8: [1, 4, 5, 5, 3, 2, 1, 1]
 9: [1, 4, 7, 6, 5, 3, 2, 1, 1]
10: [1, 5, 8, 9, 7, 5, 3, 2, 1, 1]

Sums:
23 1255
123 2552338241
1234 156978797223733228787865722354959930
```



## JavaScript

{{trans|Python}}

```JavaScript

(function () {
    var cache = [
        [1]
    ];
//this was never needed.
   /* function PyRange(start, end, step) {
        step = step || 1;
        if (!end) {
            end = start;
            start = 0;
        }
        var arr = [];
        for (var i = start; i < end; i += step) arr.push(i);
        return arr;
    }*/

    function cumu(n) {
        var /*ra = PyRange(cache.length, n + 1),*/ //Seems there is a better version for this
            r, l, x, Aa, Mi;
       // for (ll in ra) { too pythony
       for (l=cache.length;l<n+1;l++) {
            r = [0];
//            l = ra[ll];
//            ran = PyRange(1, l + 1);
//            for (xx in ran) {
            for(x=1;x<l+1;x++){
//                x = ran[xx];
                r.push(r[r.length - 1] + (Aa = cache[l - x < 0 ? cache.length - (l - x) : l - x])[(Mi = Math.min(x, l - x)) < 0 ? Aa.length - Mi : Mi]);
            }
            cache.push(r);
        }
        return cache[n];
    }

    function row(n) {
        var r = cumu(n),
//            rra = PyRange(n),
            leArray = [],
            i;
//        for (ii in rra) {
        for (i=0;i<n;i++) {
//            i = rra[ii];
            leArray.push(r[i + 1] - r[i]);
        }
        return leArray;
    }

    console.log("Rows:");
    for (iterator = 1; iterator < 12; iterator++) {
        console.log(row(iterator));
    }

    console.log("Sums")[23, 123, 1234, 12345].foreach(function (a) {
        var s = cumu(a);
        console.log(a, s[s.length - 1]);
    });
})()

```



## Julia


```julia

using Combinatorics, StatsBase

namesofline(n) = counts([x[1] for x in integer_partitions(n)])

function centerjustpyramid(n)
    maxwidth = length(string(namesofline(n)))
    for i in 1:n
        s = string(namesofline(i))
        println(" " ^ div(maxwidth - length(s), 2), s)
    end
end

centerjustpyramid(25)

const cachecountpartitions = Dict{BigInt,BigInt}()
function countpartitions(n::BigInt)
    if n < 0
        0
    elseif n < 2
        1
    elseif (np = get(cachecountpartitions, n, 0)) > 0
        np
    else
        np = 0
        sgn = 1
        for k = 1:n
            np += sgn * (countpartitions(n - (k*(3k-1)) >> 1) + countpartitions(n - (k*(3k+1)) >> 1))
            sgn = -sgn
        end
        cachecountpartitions[n] = np
    end
end

G(n) = countpartitions(BigInt(n))

for g in [23, 123, 1234, 12345]
    @time println("\nG($g) is $(G(g))")
end


```
 ### Output
```txt

                                                 [1]
                                                [1, 1]
                                              [1, 1, 1]
                                             [1, 2, 1, 1]
                                           [1, 2, 2, 1, 1]
                                          [1, 3, 3, 2, 1, 1]
                                        [1, 3, 4, 3, 2, 1, 1]
                                       [1, 4, 5, 5, 3, 2, 1, 1]
                                     [1, 4, 7, 6, 5, 3, 2, 1, 1]
                                    [1, 5, 8, 9, 7, 5, 3, 2, 1, 1]
                                 [1, 5, 10, 11, 10, 7, 5, 3, 2, 1, 1]
                               [1, 6, 12, 15, 13, 11, 7, 5, 3, 2, 1, 1]
                             [1, 6, 14, 18, 18, 14, 11, 7, 5, 3, 2, 1, 1]
                           [1, 7, 16, 23, 23, 20, 15, 11, 7, 5, 3, 2, 1, 1]
                         [1, 7, 19, 27, 30, 26, 21, 15, 11, 7, 5, 3, 2, 1, 1]
                       [1, 8, 21, 34, 37, 35, 28, 22, 15, 11, 7, 5, 3, 2, 1, 1]
                     [1, 8, 24, 39, 47, 44, 38, 29, 22, 15, 11, 7, 5, 3, 2, 1, 1]
                   [1, 9, 27, 47, 57, 58, 49, 40, 30, 22, 15, 11, 7, 5, 3, 2, 1, 1]
                 [1, 9, 30, 54, 70, 71, 65, 52, 41, 30, 22, 15, 11, 7, 5, 3, 2, 1, 1]
              [1, 10, 33, 64, 84, 90, 82, 70, 54, 42, 30, 22, 15, 11, 7, 5, 3, 2, 1, 1]
           [1, 10, 37, 72, 101, 110, 105, 89, 73, 55, 42, 30, 22, 15, 11, 7, 5, 3, 2, 1, 1]
        [1, 11, 40, 84, 119, 136, 131, 116, 94, 75, 56, 42, 30, 22, 15, 11, 7, 5, 3, 2, 1, 1]
      [1, 11, 44, 94, 141, 163, 164, 146, 123, 97, 76, 56, 42, 30, 22, 15, 11, 7, 5, 3, 2, 1, 1]
   [1, 12, 48, 108, 164, 199, 201, 186, 157, 128, 99, 77, 56, 42, 30, 22, 15, 11, 7, 5, 3, 2, 1, 1]
[1, 12, 52, 120, 192, 235, 248, 230, 201, 164, 131, 100, 77, 56, 42, 30, 22, 15, 11, 7, 5, 3, 2, 1, 1]

G(23) is 1255
  0.043878 seconds (82.76 k allocations: 3.730 MiB)

G(123) is 2552338241
  0.064343 seconds (435.68 k allocations: 7.199 MiB)

G(1234) is 156978797223733228787865722354959930
  6.439370 seconds (43.57 M allocations: 723.421 MiB, 30.61% gc time)

G(12345) is 69420357953926116819562977205209384460667673094671463620270321700806074195845953959951425306140971942519870679768681736
691.453611 seconds (4.32 G allocations: 71.973 GiB, 33.18% gc time)

```



## Kotlin

{{trans|Swift}}

```scala
import java.lang.Math.min
import java.math.BigInteger
import java.util.ArrayList
import java.util.Arrays.asList

fun namesOfGod(n: Int): List<BigInteger> {
    val cache = ArrayList<List<BigInteger>>()
    cache.add(asList(BigInteger.ONE))

    (cache.size..n).forEach { l ->
        val r = ArrayList<BigInteger>()
        r.add(BigInteger.ZERO)

        (1..l).forEach { x ->
            r.add(r[r.size - 1] + cache[l - x][min(x, l - x)])
        }
        cache.add(r)
    }
    return cache[n]
}

fun row(n: Int) = namesOfGod(n).let { r -> (0 until n).map { r[it + 1] - r[it] } }

fun main(args: Array<String>) {
    println("Rows:")
    (1..25).forEach {
        System.out.printf("%2d: %s%n", it, row(it))
    }

    println("\nSums:")
    intArrayOf(23, 123, 1234, 1234).forEach {
        val c = namesOfGod(it)
        System.out.printf("%s %s%n", it, c[c.size - 1])
    }
}
```



```txt

Rows:
 1: [1]
 2: [1, 1]
 3: [1, 1, 1]
 4: [1, 2, 1, 1]
 5: [1, 2, 2, 1, 1]
 6: [1, 3, 3, 2, 1, 1]
 7: [1, 3, 4, 3, 2, 1, 1]
 8: [1, 4, 5, 5, 3, 2, 1, 1]
 9: [1, 4, 7, 6, 5, 3, 2, 1, 1]
10: [1, 5, 8, 9, 7, 5, 3, 2, 1, 1]
11: [1, 5, 10, 11, 10, 7, 5, 3, 2, 1, 1]
12: [1, 6, 12, 15, 13, 11, 7, 5, 3, 2, 1, 1]
13: [1, 6, 14, 18, 18, 14, 11, 7, 5, 3, 2, 1, 1]
14: [1, 7, 16, 23, 23, 20, 15, 11, 7, 5, 3, 2, 1, 1]
15: [1, 7, 19, 27, 30, 26, 21, 15, 11, 7, 5, 3, 2, 1, 1]
16: [1, 8, 21, 34, 37, 35, 28, 22, 15, 11, 7, 5, 3, 2, 1, 1]
17: [1, 8, 24, 39, 47, 44, 38, 29, 22, 15, 11, 7, 5, 3, 2, 1, 1]
18: [1, 9, 27, 47, 57, 58, 49, 40, 30, 22, 15, 11, 7, 5, 3, 2, 1, 1]
19: [1, 9, 30, 54, 70, 71, 65, 52, 41, 30, 22, 15, 11, 7, 5, 3, 2, 1, 1]
20: [1, 10, 33, 64, 84, 90, 82, 70, 54, 42, 30, 22, 15, 11, 7, 5, 3, 2, 1, 1]
21: [1, 10, 37, 72, 101, 110, 105, 89, 73, 55, 42, 30, 22, 15, 11, 7, 5, 3, 2, 1, 1]
22: [1, 11, 40, 84, 119, 136, 131, 116, 94, 75, 56, 42, 30, 22, 15, 11, 7, 5, 3, 2, 1, 1]
23: [1, 11, 44, 94, 141, 163, 164, 146, 123, 97, 76, 56, 42, 30, 22, 15, 11, 7, 5, 3, 2, 1, 1]
24: [1, 12, 48, 108, 164, 199, 201, 186, 157, 128, 99, 77, 56, 42, 30, 22, 15, 11, 7, 5, 3, 2, 1, 1]
25: [1, 12, 52, 120, 192, 235, 248, 230, 201, 164, 131, 100, 77, 56, 42, 30, 22, 15, 11, 7, 5, 3, 2, 1, 1]

Sums:
23 1255
123 2552338241
1234 156978797223733228787865722354959930

```



## Lasso

This code is derived from the Python solution, as an illustration of the difference in array behaviour (indexes, syntax), and loop and query expression as alternative syntax to "for".

```Lasso
define cumu(n::integer) => {
	loop(-from=$cache->size,-to=#n+1) => {
		local(r = array(0), l = loop_count)
		loop(loop_count) => {
			protect => { #r->insert(#r->last + $cache->get(#l - loop_count)->get(math_min(loop_count+1, #l - loop_count))) }
		}
		#r->size > 1 ? $cache->insert(#r)
	}
	return $cache->get(#n)
}
define row(n::integer) => {
	// cache gets reset & rebuilt for each row, slower but more accurate
	var(cache = array(array(1)))
	local(r = cumu(#n+1))
	local(o = array)
	loop(#n) => {
		protect => { #o->insert(#r->get(loop_count+1) - #r->get(loop_count)) }
	}
	return #o
}
'rows:\r'
loop(25) => {^
	loop_count + ': '+ row(loop_count)->join(' ') + '\r'
^}

'sums:\r'
with x in array(23, 123, 1234) do => {^
	var(cache = array(array(1)))
	cumu(#x+1)->last
	'\r'
^}
```


### Output

```txt
rows:
1: 1
2: 1 1
3: 1 1 1
4: 1 2 1 1
5: 1 2 2 1 1
6: 1 3 3 2 1 1
7: 1 3 4 3 2 1 1
8: 1 4 5 5 3 2 1 1
9: 1 4 7 6 5 3 2 1 1
10: 1 5 8 9 7 5 3 2 1 1
11: 1 5 10 11 10 7 5 3 2 1 1
12: 1 6 12 15 13 11 7 5 3 2 1 1
13: 1 6 14 18 18 14 11 7 5 3 2 1 1
14: 1 7 16 23 23 20 15 11 7 5 3 2 1 1
15: 1 7 19 27 30 26 21 15 11 7 5 3 2 1 1
16: 1 8 21 34 37 35 28 22 15 11 7 5 3 2 1 1
17: 1 8 24 39 47 44 38 29 22 15 11 7 5 3 2 1 1
18: 1 9 27 47 57 58 49 40 30 22 15 11 7 5 3 2 1 1
19: 1 9 30 54 70 71 65 52 41 30 22 15 11 7 5 3 2 1 1
20: 1 10 33 64 84 90 82 70 54 42 30 22 15 11 7 5 3 2 1 1
21: 1 10 37 72 101 110 105 89 73 55 42 30 22 15 11 7 5 3 2 1 1
22: 1 11 40 84 119 136 131 116 94 75 56 42 30 22 15 11 7 5 3 2 1 1
23: 1 11 44 94 141 163 164 146 123 97 76 56 42 30 22 15 11 7 5 3 2 1 1
24: 1 12 48 108 164 199 201 186 157 128 99 77 56 42 30 22 15 11 7 5 3 2 1 1
25: 1 12 52 120 192 235 248 230 201 164 131 100 77 56 42 30 22 15 11 7 5 3 2 1 1

sums:
23: 1255
123: 2552338241
1234: 156978797223733228787865722354959930
12345: (ran long, timed out)
```



## Maple


```maple
TriangleLine(n) := map(rhs, Statistics :- Tally(map(x -> x[-1],  combinat:-partition(n)))):
Triangle := proc(m)
            local i;
            for i  from 1 to m do
                print(op(TriangleLine(i)));
            end do
            end proc:

```


### Output

```txt
Triangle(7);
                               1
                              1, 1
                            1, 1, 1
                           1, 2, 1, 1
                         1, 2, 2, 1, 1
                        1, 3, 3, 2, 1, 1
                      1, 3, 4, 3, 2, 1, 1

```


## Mathematica / ## Wolfram Language

```mathematica
Table[Last /@ Reverse@Tally[First /@ IntegerPartitions[n]], {n, 10}] // Grid
```

### Output

```txt
1
1	1
1	1	1
1	2	1	1
1	2	2	1	1
1	3	3	2	1	1
1	3	4	3	2	1	1
1	4	5	5	3	2	1	1
1	4	7	6	5	3	2	1	1
1	5	8	9	7	5	3	2	1	1
```


Here I use the bulit-in function PartitionsP to calculate <math>P(n)</math>.

```mathematica
PartitionsP /@ {23, 123, 1234, 12345}
```

### Output

```txt
{1255, 2552338241, 156978797223733228787865722354959930, 69420357953926116819562977205209384460667673094671463620270321700806074195845953959951425306140971942519870679768681736}
```



```mathematica
DiscretePlot[PartitionsP[n], {n, 1, 999}, PlotRange -> All]
```

[[File:9 billion names of God the integer Mathematica.png]]


## Nim

{{trans|Python}}

```nim
import bigints

var cache = @[@[1.initBigInt]]

proc cumu(n: int): seq[BigInt] =
  for m in cache.len .. n:
    var r = @[0.initBigInt]
    for x in 1..m:
      r.add r[r.high] + cache[m-x][min(x, m-x)]
    cache.add r
  result = cache[n]

proc row(n: int): seq[BigInt] =
  let r = cumu n
  result = @[]
  for i in 0 .. <n:
    result.add r[i+1] - r[i]

echo "rows:"
for x in 1..10:
  echo row x

echo "sums:"
for x in [23, 123, 1234, 12345]:
  let c = cumu(x)
  echo x, " ", c[c.high]
```

### Output

```txt
@[1]
@[1, 1]
@[1, 1, 1]
@[1, 2, 1, 1]
@[1, 2, 2, 1, 1]
@[1, 3, 3, 2, 1, 1]
@[1, 3, 4, 3, 2, 1, 1]
@[1, 4, 5, 5, 3, 2, 1, 1]
@[1, 4, 7, 6, 5, 3, 2, 1, 1]
@[1, 5, 8, 9, 7, 5, 3, 2, 1, 1]
sums:
23 1255
123 2552338241
1234 156978797223733228787865722354959930
^C
```


Faster version:
{{trans|C}}

```nim
import bigints

var p = @[1.initBigInt]

proc partitions(n): BigInt =
  p.add 0.initBigInt

  for k in 1..n:
    var d = n - k * (3 * k - 1) div 2
    if d < 0:
      break

    if (k and 1) != 0:
      p[n] += p[d]
    else:
      p[n] -= p[d]

    d -= k
    if d < 0:
      break

    if (k and 1) != 0:
      p[n] += p[d]
    else:
      p[n] -= p[d]

  result = p[p.high]

const ns = [23, 123, 1234, 12345]
for i in 1 .. max(ns):
  let p = partitions(i)
  if i in ns:
    echo i,": ",p
```

### Output

```txt
23: 1255
123: 2552338241
1234: 156978797223733228787865722354959930
12345: 69420357953926116819562977205209384460667673094671463620270321700806074195845953959951425306140971942519870679768681736
```



## OCaml


```ocaml

let get, sum_unto =
  let cache = ref [||]
  let rec get i j =
    if Array.length !cache < i then
      cache :=
        Array.init i begin fun i ->
          try !cache.(i) with Invalid_argument _ ->
          Array.make (i+1) (Num.num_of_int 0)
        end;
    if Num.(!cache.(i-1).(j-1) =/ num_of_int 0)
    then !cache.(i-1).(j-1) <- sum_unto (i-j) j;
    !cache.(i-1).(j-1)
  and sum_unto i j =
    let rec sum_unto sum i j =
      match (i,j) with
      |(0,0) -> (Num.num_of_int 1)
      |(_,0) -> sum
      |(i,j) when j > i -> sum_unto sum i i
      |(i,j) -> sum_unto Num.(sum +/ (get i j)) i (j-1)
    in
    sum_unto (Num.num_of_int 0) i j
  in
  get, sum_unto

let sum_of_row n = sum_unto n n

let euler_recurrence =
  let cache = ref [||] in
  let rec recurrence = function
    |n when n < 0 -> Num.num_of_int 0
    |0 -> Num.num_of_int 1
    |n ->
        if n >= Array.length !cache then
          cache :=
            Array.init (n+1) (fun i ->
              try !cache.(i) with Invalid_argument _ -> Num.num_of_int 0);
        if Num.(!cache.(n) =/ num_of_int 0)
        then begin
          let rec summing sum = function
            |0 -> sum
            |k ->
                let op = if k mod 2 = 0 then Num.sub_num else Num.add_num in
                let sum = op sum (recurrence (n - k * (3*k - 1) / 2)) in
                let sum = op sum (recurrence (n - k * (3*k + 1) / 2)) in
                summing sum (k-1)
          in
          !cache.(n) <- summing (Num.num_of_int 0) n
        end;
        !cache.(n)
  in
  recurrence

let print i_max =
  for i=1 to i_max do
    print_int (i+1); print_string ": ";
    for j=1 to i do
      print_string (Num.string_of_num (get i j));
      print_char ' ';
    done;
    print_newline ();
  done

let () =
  print 30;
  print_newline ();
  List.iter begin fun i ->
      Printf.printf "%i: %s ?= %s\n" i
        (Num.string_of_num (sum_of_row i))
        (Num.string_of_num (euler_recurrence i));
      flush stdout;
    end
  [23;123;1234;];
  List.iter begin fun i ->
      Printf.printf "%i: %s\n" i
        (Num.string_of_num (euler_recurrence i));
      flush stdout;
    end
  [23;123;1234;12345;123456]

```

### Output

```txt

2: 1
3: 1 1
4: 1 1 1
5: 1 2 1 1
6: 1 2 2 1 1
7: 1 3 3 2 1 1
8: 1 3 4 3 2 1 1
9: 1 4 5 5 3 2 1 1
10: 1 4 7 6 5 3 2 1 1
11: 1 5 8 9 7 5 3 2 1 1
12: 1 5 10 11 10 7 5 3 2 1 1
13: 1 6 12 15 13 11 7 5 3 2 1 1
14: 1 6 14 18 18 14 11 7 5 3 2 1 1
15: 1 7 16 23 23 20 15 11 7 5 3 2 1 1
16: 1 7 19 27 30 26 21 15 11 7 5 3 2 1 1
17: 1 8 21 34 37 35 28 22 15 11 7 5 3 2 1 1
18: 1 8 24 39 47 44 38 29 22 15 11 7 5 3 2 1 1
19: 1 9 27 47 57 58 49 40 30 22 15 11 7 5 3 2 1 1
20: 1 9 30 54 70 71 65 52 41 30 22 15 11 7 5 3 2 1 1
21: 1 10 33 64 84 90 82 70 54 42 30 22 15 11 7 5 3 2 1 1
22: 1 10 37 72 101 110 105 89 73 55 42 30 22 15 11 7 5 3 2 1 1
23: 1 11 40 84 119 136 131 116 94 75 56 42 30 22 15 11 7 5 3 2 1 1
24: 1 11 44 94 141 163 164 146 123 97 76 56 42 30 22 15 11 7 5 3 2 1 1
25: 1 12 48 108 164 199 201 186 157 128 99 77 56 42 30 22 15 11 7 5 3 2 1 1
26: 1 12 52 120 192 235 248 230 201 164 131 100 77 56 42 30 22 15 11 7 5 3 2 1 1
27: 1 13 56 136 221 282 300 288 252 212 169 133 101 77 56 42 30 22 15 11 7 5 3 2 1 1
28: 1 13 61 150 255 331 364 352 318 267 219 172 134 101 77 56 42 30 22 15 11 7 5 3 2 1 1
29: 1 14 65 169 291 391 436 434 393 340 278 224 174 135 101 77 56 42 30 22 15 11 7 5 3 2 1 1
30: 1 14 70 185 333 454 522 525 488 423 355 285 227 175 135 101 77 56 42 30 22 15 11 7 5 3 2 1 1
31: 1 15 75 206 377 532 618 638 598 530 445 366 290 229 176 135 101 77 56 42 30 22 15 11 7 5 3 2 1 1

23: 1255 ?= 1255
123: 2552338241 ?= 2552338241
1234: 156978797223733228787865722354959930 ?= 156978797223733228787865722354959930
23: 1255
123: 2552338241
1234: 156978797223733228787865722354959930
12345: 69420357953926116819562977205209384460667673094671463620270321700806074195845953959951425306140971942519870679768681736
123456: 30817659578536496678545317146533980855296613274507139217608776782063054452191537379312358383342446230621170608408020911309259407611257151683372221925128388387168451943800027128045369650890220060901494540459081545445020808726917371699102825508039173543836338081612528477859613355349851184591540231790254269948278726548570660145691076819912972162262902150886818986555127204165221706149989
./intnames  897.04s user 2.43s system 94% cpu 15:47.77 total

```



## PARI/GP



```parigp
row(n)=my(v=vector(n)); forpart(i=n,v[i[#i]]++); v;
show(n)=for(k=1,n,print(row(k)));
show(25)
apply(numbpart, [23,123,1234","12345"])
plot(x=1,999.9, numbpart(x\1))
```

### Output

```txt
[1]
[1, 1]
[1, 1, 1]
[1, 2, 1, 1]
[1, 2, 2, 1, 1]
[1, 3, 3, 2, 1, 1]
[1, 3, 4, 3, 2, 1, 1]
[1, 4, 5, 5, 3, 2, 1, 1]
[1, 4, 7, 6, 5, 3, 2, 1, 1]
[1, 5, 8, 9, 7, 5, 3, 2, 1, 1]
[1, 5, 10, 11, 10, 7, 5, 3, 2, 1, 1]
[1, 6, 12, 15, 13, 11, 7, 5, 3, 2, 1, 1]
[1, 6, 14, 18, 18, 14, 11, 7, 5, 3, 2, 1, 1]
[1, 7, 16, 23, 23, 20, 15, 11, 7, 5, 3, 2, 1, 1]
[1, 7, 19, 27, 30, 26, 21, 15, 11, 7, 5, 3, 2, 1, 1]
[1, 8, 21, 34, 37, 35, 28, 22, 15, 11, 7, 5, 3, 2, 1, 1]
[1, 8, 24, 39, 47, 44, 38, 29, 22, 15, 11, 7, 5, 3, 2, 1, 1]
[1, 9, 27, 47, 57, 58, 49, 40, 30, 22, 15, 11, 7, 5, 3, 2, 1, 1]
[1, 9, 30, 54, 70, 71, 65, 52, 41, 30, 22, 15, 11, 7, 5, 3, 2, 1, 1]
[1, 10, 33, 64, 84, 90, 82, 70, 54, 42, 30, 22, 15, 11, 7, 5, 3, 2, 1, 1]
[1, 10, 37, 72, 101, 110, 105, 89, 73, 55, 42, 30, 22, 15, 11, 7, 5, 3, 2, 1, 1]
[1, 11, 40, 84, 119, 136, 131, 116, 94, 75, 56, 42, 30, 22, 15, 11, 7, 5, 3, 2,1, 1]
[1, 11, 44, 94, 141, 163, 164, 146, 123, 97, 76, 56, 42, 30, 22, 15, 11, 7, 5, 3, 2, 1, 1]
[1, 12, 48, 108, 164, 199, 201, 186, 157, 128, 99, 77, 56, 42, 30, 22, 15, 11, 7, 5, 3, 2, 1, 1]
[1, 12, 52, 120, 192, 235, 248, 230, 201, 164, 131, 100, 77, 56, 42, 30, 22, 15, 11, 7, 5, 3, 2, 1, 1]

%1 = [1255, 2552338241, 156978797223733228787865722354959930, 69420357953926116819562977205209384460667673094671463620270321700806074195845953959951425306140971942519870679768681736]

2.31e+031 |''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''"
          |                                                              :
          |                                                              :
          |                                                              :
          |                                                              :
          |                                                             :|
          |                                                             :|
          |                                                             :|
          |                                                             :|
          |                                                             _|
          |                                                             :|
          |                                                             :|
          |                                                            : |
          |                                                            : |
          |                                                            : |
          |                                                            x |
          |                                                            : |
          |                                                           :  |
          |                                                           x  |
          |                                                              |
          |                                                         _"   |
        1 ________________________________________________________xx,,,,,,
          1                                                          999.9
```


Using <code>ploth</code> in place of <code>plot</code> yields a nice image which cannot be uploaded at present.


## Perl

*Library: ntheory*

```perl
use ntheory qw/:all/;

sub triangle_row {
  my($n,@row) = (shift);
  # Tally by first element of the unrestricted integer partitions.
  forpart { $row[ $_[0] - 1 ]++ } $n;
  @row;
}

printf "%2d: %s\n", $_, join(" ",triangle_row($_)) for 1..25;
print "\n";
say "P($_) = ", partitions($_) for (23, 123, 1234, 12345);
```

### Output
[rows are the same as below]

```txt
P(23) = 1255
P(123) = 2552338241
P(1234) = 156978797223733228787865722354959930
P(12345) = 69420357953926116819562977205209384460667673094671463620270321700806074195845953959951425306140971942519870679768681736
```


{{trans|Perl6}}

```perl

use strict;
use warnings;

# Where perl6 uses arbitrary precision integers everywhere
# that you don't tell it not to do so, perl5 will only use
# them where you *do* tell it do so.
use Math::BigInt;
use constant zero => Math::BigInt->bzero;
use constant one  => Math::BigInt->bone;

my @todo = [one];
my @sums = (zero);
sub nextrow {
   my $n = shift;
   for my $l (@todo .. $n) {
      $sums[$l] = zero;
      #print "$l\r" if $l < $n;
      my @r;
      for my $x (reverse 0 .. $l-1) {
         my $todo = $todo[$x];
         $sums[$x] += shift @$todo if @$todo;
         push @r, $sums[$x];
      }
      push @todo, \@r;
   }
   @{ $todo[$n] };
}

print "rows:\n";
for(1..25) {
   printf("%2d: ", $_);
   print join(' ', nextrow($_)), "\n";
}
print "\nsums:\n";
for (23, 123, 1234, 12345) {
   print $_, "." x (8 - length);
   my $i = 0;
   $i += $_ for nextrow($_);
   print $i, "\n";
}

```

### Output

```txt

rows:
 1: 1
 2: 1 1
 3: 1 1 1
 4: 1 2 1 1
 5: 1 2 2 1 1
 6: 1 3 3 2 1 1
 7: 1 3 4 3 2 1 1
 8: 1 4 5 5 3 2 1 1
 9: 1 4 7 6 5 3 2 1 1
10: 1 5 8 9 7 5 3 2 1 1
11: 1 5 10 11 10 7 5 3 2 1 1
12: 1 6 12 15 13 11 7 5 3 2 1 1
13: 1 6 14 18 18 14 11 7 5 3 2 1 1
14: 1 7 16 23 23 20 15 11 7 5 3 2 1 1
15: 1 7 19 27 30 26 21 15 11 7 5 3 2 1 1
16: 1 8 21 34 37 35 28 22 15 11 7 5 3 2 1 1
17: 1 8 24 39 47 44 38 29 22 15 11 7 5 3 2 1 1
18: 1 9 27 47 57 58 49 40 30 22 15 11 7 5 3 2 1 1
19: 1 9 30 54 70 71 65 52 41 30 22 15 11 7 5 3 2 1 1
20: 1 10 33 64 84 90 82 70 54 42 30 22 15 11 7 5 3 2 1 1
21: 1 10 37 72 101 110 105 89 73 55 42 30 22 15 11 7 5 3 2 1 1
22: 1 11 40 84 119 136 131 116 94 75 56 42 30 22 15 11 7 5 3 2 1 1
23: 1 11 44 94 141 163 164 146 123 97 76 56 42 30 22 15 11 7 5 3 2 1 1
24: 1 12 48 108 164 199 201 186 157 128 99 77 56 42 30 22 15 11 7 5 3 2 1 1
25: 1 12 52 120 192 235 248 230 201 164 131 100 77 56 42 30 22 15 11 7 5 3 2 1 1

sums:
23......1243
123.....2552338241
1234....156978797223733228787865722354959930
^C

```

Note: I didn't wait long enough to see what the next result was, and stopped the program.


## Perl 6

To save a bunch of memory, this algorithm throws away all the numbers that it knows it's not going to use again, on the assumption that the function will only be called with increasing values of $n.  (It could easily be made to recalculate if it notices a regression.)


```perl6
my @todo = $[1];
my @sums = 0;
sub nextrow($n) {
    for +@todo .. $n -> $l {
        @sums[$l] = 0;
        print $l,"\r" if $l < $n;
        my $r = [];
        for reverse ^$l -> $x {
            my @x := @todo[$x];
            if @x {
                $r.push: @sums[$x] += @x.shift;
            }
            else {
                $r.push: @sums[$x];
            }
        }
        @todo.push($r);
    }
    @todo[$n];
}

say "rows:";
say .fmt('%2d'), ": ", nextrow($_)[] for 1..10;


say "\nsums:";
for 23, 123, 1234, 12345 {
    say $_, "\t", [+] nextrow($_)[];
}
```

### Output

```txt
rows:
 1: 1
 2: 1 1
 3: 1 1 1
 4: 1 2 1 1
 5: 1 2 2 1 1
 6: 1 3 3 2 1 1
 7: 1 3 4 3 2 1 1
 8: 1 4 5 5 3 2 1 1
 9: 1 4 7 6 5 3 2 1 1
10: 1 5 8 9 7 5 3 2 1 1

sums:
23	1255
123	2552338241
1234	156978797223733228787865722354959930
12345	69420357953926116819562977205209384460667673094671463620270321700806074195845953959951425306140971942519870679768681736
```



## Phix


```Phix
-- demo\rosetta\9billionnames.exw

sequence cache = {{1}}
function cumu(integer n)
sequence r
    for l=length(cache) to n do
        r = {0}
        for x=1 to l do
            r = append(r,r[-1]+cache[l-x+1][min(x,l-x)+1])
        end for
        cache = append(cache,r)
    end for
    return cache[n]
end function

function row(integer n)
sequence r = cumu(n+1)
sequence res = repeat(0,n)
    for i=1 to n do
        res[i] = r[i+1]-r[i]
    end for
    return res
end function

for i=1 to 25 do
    puts(1,repeat(' ',50-2*i))
    sequence r = row(i)
    for j=1 to i do
        printf(1,"%4d",r[j])
    end for
    puts(1,"\n")
end for
```

### Output

```txt

                                                   1
                                                 1   1
                                               1   1   1
                                             1   2   1   1
                                           1   2   2   1   1
                                         1   3   3   2   1   1
                                       1   3   4   3   2   1   1
                                     1   4   5   5   3   2   1   1
                                   1   4   7   6   5   3   2   1   1
                                 1   5   8   9   7   5   3   2   1   1
                               1   5  10  11  10   7   5   3   2   1   1
                             1   6  12  15  13  11   7   5   3   2   1   1
                           1   6  14  18  18  14  11   7   5   3   2   1   1
                         1   7  16  23  23  20  15  11   7   5   3   2   1   1
                       1   7  19  27  30  26  21  15  11   7   5   3   2   1   1
                     1   8  21  34  37  35  28  22  15  11   7   5   3   2   1   1
                   1   8  24  39  47  44  38  29  22  15  11   7   5   3   2   1   1
                 1   9  27  47  57  58  49  40  30  22  15  11   7   5   3   2   1   1
               1   9  30  54  70  71  65  52  41  30  22  15  11   7   5   3   2   1   1
             1  10  33  64  84  90  82  70  54  42  30  22  15  11   7   5   3   2   1   1
           1  10  37  72 101 110 105  89  73  55  42  30  22  15  11   7   5   3   2   1   1
         1  11  40  84 119 136 131 116  94  75  56  42  30  22  15  11   7   5   3   2   1   1
       1  11  44  94 141 163 164 146 123  97  76  56  42  30  22  15  11   7   5   3   2   1   1
     1  12  48 108 164 199 201 186 157 128  99  77  56  42  30  22  15  11   7   5   3   2   1   1
   1  12  52 120 192 235 248 230 201 164 131 100  77  56  42  30  22  15  11   7   5   3   2   1   1

```


###  Part 2

{{trans|C}}
*Library: mpfr*

```Phix
include mpfr.e

sequence p

procedure calc(integer n)
    n += 1
    for k=1 to n-1 do
        integer d = n - k * (3 * k - 1) / 2;
        if d<1 then exit end if
        if and_bits(k,1) then mpz_add(p[n],p[n],p[d])
                         else mpz_sub(p[n],p[n],p[d]) end if
        d -= k;
        if d<1 then exit end if
        if and_bits(k,1) then mpz_add(p[n],p[n],p[d])
                         else mpz_sub(p[n],p[n],p[d]) end if
    end for
end procedure

constant cx = {23, 123, 1234, 12345}
puts(1,"sums:\n")
integer at = 1
p = mpz_inits(cx[$]+1)
mpz_set_si(p[1],1)
for i=1 to cx[$] do
    calc(i)
    if i=cx[at] then
        printf(1,"%2d:%s\n",{i,mpz_get_str(p[i+1])})
        at += 1
    end if
end for
```

{{Out}}

```txt

sums:
23:1255
123:2552338241
1234:156978797223733228787865722354959930
12345:69420357953926116819562977205209384460667673094671463620270321700806074195845953959951425306140971942519870679768681736

```


###  Third and last, a simple plot
*Library: pGUI*

```Phix
include pGUI.e
IupOpen()
IupControlsOpen()
Ihandle plot = IupPlot("MENUITEMPROPERTIES=Yes, SIZE=640x320")
IupSetAttribute(plot, "TITLE", "9 Billion Names");
IupSetAttribute(plot, "TITLEFONTSIZE", "10");
IupSetAttribute(plot, "TITLEFONTSTYLE", "ITALIC");
IupSetAttribute(plot, "GRIDLINESTYLE", "DOTTED");
IupSetAttribute(plot, "GRID", "YES");
IupSetAttribute(plot, "AXS_XLABEL", "x");
IupSetAttribute(plot, "AXS_YLABEL", "G(x)");
IupSetAttribute(plot, "AXS_XFONTSTYLE", "ITALIC");
IupSetAttribute(plot, "AXS_YFONTSTYLE", "ITALIC");
IupSetAttribute(plot, "AXS_XSCALE", "LOG10");
IupSetAttribute(plot, "AXS_YSCALE", "LOG10");
IupSetAttribute(plot, "AXS_YTICKSIZEAUTO", "NO");
IupSetAttribute(plot, "AXS_YTICKMAJORSIZE", "8");
IupSetAttribute(plot, "AXS_YTICKMINORSIZE", "0");
IupPlotBegin(plot)
for x=1 to 999 do
    IupPlotAdd(plot, x, sum(row(x))) -- (row() from part 1)
end for
{} = IupPlotEnd(plot)
Ihandle dlg = IupDialog(plot)
IupCloseOnEscape(dlg)
IupSetAttribute(dlg, "TITLE", "9 Billion Names")
IupMap(dlg)
IupShowXY(dlg,IUP_CENTER,IUP_CENTER)
IupMainLoop()
IupClose()
```



## PicoLisp

{{trans|Python}}

```PicoLisp
(de row (N)
   (let C '((1))
      (do N
         (push 'C (grow C)) )
      (mapcon
         '((L)
            (when (cdr L)
               (cons (- (cadr L) (car L))) ) )
         (car C) ) ) )

(de grow (Lst)
   (let (L (length Lst)  S 0)
      (cons
         0
         (mapcar
            '((I X)
               (inc 'S
                  (get I (inc (min X (- L X)))) ) )
            Lst
            (range 1 L) ) ) ) )

(de sumr (N)
   (let
      (K 1
         S 1
         O (cons 1 (need N 0))
         D
         (make
            (while
               (<
                  (* K (dec (* 3 K)))
                  (* 2 N) )
               (link (list (dec (* 2 K)) S))
               (link (list K S))
               (inc 'K)
               (setq S (- S)) ) ) )
      (for (Y O (cdr Y) (cdr Y))
         (let Z Y
            (for L D
               (inc
                  (setq Z (cdr (nth Z (car L))))
                  (* (car Y) (cadr L)) ) ) ) )
      (last O) ) )

(for I 25
   (println (row I)) )

(bench
   (for I '(23 123 1234 12345)
      (println (sumr I)) ) )

(bye)
```


### Output

```txt
(1)
(1 1)
(1 1 1)
(1 2 1 1)
(1 2 2 1 1)
(1 3 3 2 1 1)
(1 3 4 3 2 1 1)
(1 4 5 5 3 2 1 1)
(1 4 7 6 5 3 2 1 1)
(1 5 8 9 7 5 3 2 1 1)
(1 5 10 11 10 7 5 3 2 1 1)
(1 6 12 15 13 11 7 5 3 2 1 1)
(1 6 14 18 18 14 11 7 5 3 2 1 1)
(1 7 16 23 23 20 15 11 7 5 3 2 1 1)
(1 7 19 27 30 26 21 15 11 7 5 3 2 1 1)
(1 8 21 34 37 35 28 22 15 11 7 5 3 2 1 1)
(1 8 24 39 47 44 38 29 22 15 11 7 5 3 2 1 1)
(1 9 27 47 57 58 49 40 30 22 15 11 7 5 3 2 1 1)
(1 9 30 54 70 71 65 52 41 30 22 15 11 7 5 3 2 1 1)
(1 10 33 64 84 90 82 70 54 42 30 22 15 11 7 5 3 2 1 1)
(1 10 37 72 101 110 105 89 73 55 42 30 22 15 11 7 5 3 2 1 1)
(1 11 40 84 119 136 131 116 94 75 56 42 30 22 15 11 7 5 3 2 1 1)
(1 11 44 94 141 163 164 146 123 97 76 56 42 30 22 15 11 7 5 3 2 1 1)
(1 12 48 108 164 199 201 186 157 128 99 77 56 42 30 22 15 11 7 5 3 2 1 1)
(1 12 52 120 192 235 248 230 201 164 131 100 77 56 42 30 22 15 11 7 5 3 2 1 1)
1255
2552338241
156978797223733228787865722354959930
69420357953926116819562977205209384460667673094671463620270321700806074195845953959951425306140971942519870679768681736
0.626 sec
```



## PureBasic


```purebasic

Define nMax.i=25, n.i, k.i
Dim pfx.s(1)

Procedure.s Sigma(sx.s, sums.s)
  Define i.i, v1.i, v2.i, r.i
  Define s.s, sa.s
  sums=ReverseString(sums) : s=ReverseString(sx)
  For i=1 To Len(s)*Bool(Len(s)>Len(sums))+Len(sums)*Bool(Len(sums)>=Len(s))
    v1=Val(Mid(s,i,1))
    v2=Val(Mid(sums,i,1))
    r+v1+v2
    sa+Str(r%10)
    r/10
  Next i
  If r : sa+Str(r%10) : EndIf
  ProcedureReturn ReverseString(sa)
EndProcedure

Procedure.i Adr(row.i,col.i)
  ProcedureReturn ((row-1)*row/2+col)*Bool(row>0 And col>0)
EndProcedure

Procedure Triangle(row.i,Array pfx.s(1))
  Define n.i,k.i
  Define zs.s
  nMax=row
  ReDim pfx(Adr(nMax,nMax))
  For n=1 To nMax
    For k=1 To n
      If k>n    : pfx(Adr(n,k))="0"    : Continue : EndIf
      If n=k    : pfx(Adr(n,k))="1"    : Continue : EndIf
      If k<=n/2
        zs=""
        zs=Sigma(pfx(Adr(n-k,k)),zs)
        zs=Sigma(pfx(Adr(n-1,k-1)),zs)
        pfx(Adr(n,k))=zs
      Else
        pfx(Adr(n,k))=pfx(Adr(n-1,k-1))
      EndIf
    Next k
  Next n
EndProcedure

Procedure.s sum(row.i, Array pfx.s(1))
  Define s.s
  Triangle(row, pfx())
  For n=1 To row
    s=Sigma(pfx(Adr(row,n)),s)
  Next n
  ProcedureReturn RSet(Str(row),5,Chr(32))+" : "+s
EndProcedure

OpenConsole()

Triangle(nMax, pfx())
For n=1 To nMax
  Print(Space(((nMax*4-1)-(n*4-1))/2))
  For k=1 To n
    Print(RSet(pfx(Adr(n,k)),3,Chr(32))+Space(1))
  Next k
  PrintN("")
Next n
PrintN("")
PrintN(sum(23,pfx()))
PrintN(sum(123,pfx()))
PrintN(sum(1234,pfx()))
PrintN(sum(12345,pfx()))
Input()

```

### Output

```txt

                                                  1
                                                1   1
                                              1   1   1
                                            1   2   1   1
                                          1   2   2   1   1
                                        1   3   3   2   1   1
                                      1   3   4   3   2   1   1
                                    1   4   5   5   3   2   1   1
                                  1   4   7   6   5   3   2   1   1
                                1   5   8   9   7   5   3   2   1   1
                              1   5  10  11  10   7   5   3   2   1   1
                            1   6  12  15  13  11   7   5   3   2   1   1
                          1   6  14  18  18  14  11   7   5   3   2   1   1
                        1   7  16  23  23  20  15  11   7   5   3   2   1   1
                      1   7  19  27  30  26  21  15  11   7   5   3   2   1   1
                    1   8  21  34  37  35  28  22  15  11   7   5   3   2   1   1
                  1   8  24  39  47  44  38  29  22  15  11   7   5   3   2   1   1
                1   9  27  47  57  58  49  40  30  22  15  11   7   5   3   2   1   1
              1   9  30  54  70  71  65  52  41  30  22  15  11   7   5   3   2   1   1
            1  10  33  64  84  90  82  70  54  42  30  22  15  11   7   5   3   2   1   1
          1  10  37  72 101 110 105  89  73  55  42  30  22  15  11   7   5   3   2   1   1
        1  11  40  84 119 136 131 116  94  75  56  42  30  22  15  11   7   5   3   2   1   1
      1  11  44  94 141 163 164 146 123  97  76  56  42  30  22  15  11   7   5   3   2   1   1
    1  12  48 108 164 199 201 186 157 128  99  77  56  42  30  22  15  11   7   5   3   2   1   1
  1  12  52 120 192 235 248 230 201 164 131 100  77  56  42  30  22  15  11   7   5   3   2   1   1

   23 : 1255
  123 : 2552338241
 1234 : 156978797223733228787865722354959930
12345 : 69420357953926116819562977205209384460667673094671463620270321700806074195845953959951425306140971942519870679768681736

```



## Python


```python
cache = [[1]]
def cumu(n):
    for l in range(len(cache), n+1):
        r = [0]
        for x in range(1, l+1):
            r.append(r[-1] + cache[l-x][min(x, l-x)])
        cache.append(r)
    return cache[n]

def row(n):
    r = cumu(n)
    return [r[i+1] - r[i] for i in range(n)]

print "rows:"
for x in range(1, 11): print "%2d:"%x, row(x)


print "\nsums:"
for x in [23, 123, 1234, 12345]: print x, cumu(x)[-1]
```

### Output (I didn't actually wait long enough to see what the sum for 12345 is)

```txt

rows:
 1: [1]
 2: [1, 1]
 3: [1, 1, 1]
 4: [1, 2, 1, 1]
 5: [1, 2, 2, 1, 1]
 6: [1, 3, 3, 2, 1, 1]
 7: [1, 3, 4, 3, 2, 1, 1]
 8: [1, 4, 5, 5, 3, 2, 1, 1]
 9: [1, 4, 7, 6, 5, 3, 2, 1, 1]
10: [1, 5, 8, 9, 7, 5, 3, 2, 1, 1]

sums:
23 1255
123 2552338241
1234 156978797223733228787865722354959930
^C

```

To calculate partition functions only:

```python
def partitions(N):
    diffs,k,s = [],1,1
    while k * (3*k-1) < 2*N:
        diffs.extend([(2*k - 1, s), (k, s)])
	k,s = k+1,-s

    out = [1] + [0]*N
    for p in range(0, N+1):
        x = out[p]
	for (o,s) in diffs:
           p += o
           if p > N: break
           out[p] += x*s

    return out

p = partitions(12345)
for x in [23,123,1234,12345]: print x, p[x]
```


This version uses only a fraction of the memory and of the running time, compared to the first one that has to generate all the rows:
{{trans|C}}

```python
def partitions(n):
    partitions.p.append(0)

    for k in xrange(1, n + 1):
        d = n - k * (3 * k - 1) // 2
        if d < 0:
            break

        if k & 1:
            partitions.p[n] += partitions.p[d]
        else:
            partitions.p[n] -= partitions.p[d]

        d -= k
        if d < 0:
            break

        if k & 1:
            partitions.p[n] += partitions.p[d]
        else:
            partitions.p[n] -= partitions.p[d]

    return partitions.p[-1]

partitions.p = [1]

def main():
    ns = set([23, 123, 1234, 12345])
    max_ns = max(ns)

    for i in xrange(1, max_ns + 1):
        if i > max_ns:
            break
        p = partitions(i)
        if i in ns:
            print "%6d: %s" % (i, p)

main()
```

### Output

```txt
    23: 1255
   123: 2552338241
  1234: 156978797223733228787865722354959930
 12345: 69420357953926116819562977205209384460667673094671463620270321700806074195845953959951425306140971942519870679768681736
```



## Racket


```racket
#lang racket

(define (cdr-empty ls) (if (empty? ls) empty (cdr ls)))

(define (names-of n)
  (define (names-of-tail ans raws-rest n)
    (if (zero? n)
        ans
        (names-of-tail (cons 1 (append (map +
                                            (take ans (length raws-rest))
                                            (map car raws-rest))
                                       (drop ans (length raws-rest))))
                       (filter (compose not empty?)
                               (map cdr-empty (cons ans raws-rest)))
                       (sub1 n))))
  (names-of-tail '() '() n))

(define (G n) (foldl + 0 (names-of n)))

(module+ main
  (build-list 25 (compose names-of add1))
  (newline)
  (map G '(23 123 1234)))

```


### Output

```txt

'((1)
  (1 1)
  (1 1 1)
  (1 2 1 1)
  (1 2 2 1 1)
  (1 3 3 2 1 1)
  (1 3 4 3 2 1 1)
  (1 4 5 5 3 2 1 1)
  (1 4 7 6 5 3 2 1 1)
  (1 5 8 9 7 5 3 2 1 1)
  (1 5 10 11 10 7 5 3 2 1 1)
  (1 6 12 15 13 11 7 5 3 2 1 1)
  (1 6 14 18 18 14 11 7 5 3 2 1 1)
  (1 7 16 23 23 20 15 11 7 5 3 2 1 1)
  (1 7 19 27 30 26 21 15 11 7 5 3 2 1 1)
  (1 8 21 34 37 35 28 22 15 11 7 5 3 2 1 1)
  (1 8 24 39 47 44 38 29 22 15 11 7 5 3 2 1 1)
  (1 9 27 47 57 58 49 40 30 22 15 11 7 5 3 2 1 1)
  (1 9 30 54 70 71 65 52 41 30 22 15 11 7 5 3 2 1 1)
  (1 10 33 64 84 90 82 70 54 42 30 22 15 11 7 5 3 2 1 1)
  (1 10 37 72 101 110 105 89 73 55 42 30 22 15 11 7 5 3 2 1 1)
  (1 11 40 84 119 136 131 116 94 75 56 42 30 22 15 11 7 5 3 2 1 1)
  (1 11 44 94 141 163 164 146 123 97 76 56 42 30 22 15 11 7 5 3 2 1 1)
  (1 12 48 108 164 199 201 186 157 128 99 77 56 42 30 22 15 11 7 5 3 2 1 1)
  (1 12 52 120 192 235 248 230 201 164 131 100 77 56 42 30 22 15 11 7 5 3 2 1 1))

'(1255 2552338241 156978797223733228787865722354959930)

```



## REXX

This REXX version displays a nicely "balanced" numbers triangle as per this task's requirement.

If the number of rows is entered as a signed positive integer, only the number of partitions is shown,

(that is, the sum of the numbers on the last line of the number triangle).

If the number of rows is entered as a signed integer, the triangle isn't shown.

Memoization is used to quickly obtain information of previously calculated numbers in the left-hand side of

the triangle and also previous calculated partitions.

The right half of the triangle isn't calculated but rather the value is taken from a previous row and column.

Also, the left two columns of the triangle are computed directly   [either   **1**   or   **row%2**   (integer divide)]

as well as the rightmost three columns   (either   **1**   or   **2**).

The formula used is:
<big>
:::::: <math> P_n  =  \sum_{k=1}^n (-1)^{k+1} \Big( A_{} + B_{} \Big) </math>
::::::::::::::::: <math> A_{}  =  \Big( P_{n-k(3k-1)/2} \Big) </math>
::::::::::::::::: <math> B_{}  =  \Big( P_{n-k(3k+1)/2} \Big) </math>
</big>
which is derived from Euler's generating function.

```rexx
/*REXX program  generates and displays a  number triangle  for partitions of a number.  */
numeric digits 400                               /*be able to handle larger numbers.    */
parse arg N .;  if N==''  then N=25              /*N  specified?  Then use the default. */
@.=0;     @.0=1;          aN=abs(N)
if N==N+0  then say  '         G('aN"):"  G(N)   /*just do this for well formed numbers.*/
                say  'partitions('aN"):"  partitions(aN)           /*do it the easy way.*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
G: procedure;  parse arg nn;      !.=0;     mx=1;       aN=abs(nn);      build=nn>0
   !.4.2=2;    do j=1  for aN%2;  !.j.j=1;  end  /*j*/        /*generate some shortcuts.*/

            do     t=1  for 1+build;  #.=1           /*generate triangle once or twice. */
              do   r=1  for aN;       #.2=r%2        /*#.2  is a shortcut calculation.  */
                do c=3  to  r-2;      #.c=gen#(r,c);    end  /*c*/
              L=length(mx);     p=0;  __=            /*__  will be a row of the triangle*/
                  do cc=1  for r                     /*only sum the last row of numbers.*/
                  p=p+#.cc                           /*add the last row of the triangle.*/
                  if \build  then iterate            /*should we skip building triangle?*/
                  mx=max(mx, #.cc)                   /*used to build the symmetric #s.  */
                  __=__ right(#.cc, L)               /*construct a row of the triangle. */
                  end   /*cc*/
              if t==1  then iterate                  /*Is this 1st time through? No show*/
              say  center(strip(__), 2+(aN-1)*(length(mx)+1))
              end       /*r*/                        /* [↑]  center row of the triangle.*/
            end         /*t*/
  return p                                           /*return with the generated number.*/
/*──────────────────────────────────────────────────────────────────────────────────────*/
gen#: procedure expose !.;   parse arg x,y             /*obtain the  X and Y  arguments.*/
      if !.x.y\==0  then  return !.x.y                 /*was number generated before ?  */
      if y>x%2  then do;  nx=x+1-2*(y-x%2)-(x//2==0);     ny=nx%2;       !.x.y=!.nx.ny
                          return !.x.y                 /*return the calculated number.  */
                     end                               /* [↑]  right half of triangle.  */
      $=1                                              /* [↓]   left   "   "     "      */
                          do q=2  for  y-1;   xy=x-y;   if q>xy  then iterate
                          if q==2  then $=$+xy%2
                                   else if q==xy-1  then $=$+1
                                                    else $=$+gen#(xy,q)       /*recurse.*/
                          end   /*q*/
      !.x.y=$; return $                                /*use memoization; return with #.*/
/*──────────────────────────────────────────────────────────────────────────────────────*/
partitions: procedure expose @.; parse arg n; if @.n\==0 then return @.n  /* ◄────────┐ */
            $=0                                          /*Already known?  Return ►───┘ */
                  do k=1  for n;  _=n-(k*3-1)*k%2;   if _<0  then leave
                  if @._==0  then x=partitions(_)        /* [◄]  recursive call.*/
                             else x=@._                  /*value already known. */
                  _=_-k;   if _<0  then  y=0             /*recursive call ►────┐*/
                                   else  if @._==0  then y=partitions(_)  /*◄──┘*/
                                                    else y=@._
                  if k//2  then $=$+x+y                  /*use this method if K is odd. */
                           else $=$-x-y                  /* "    "     "    " "  " even.*/
                  end   /*k*/                            /* [↑]  Euler's recursive func.*/
            @.n=$;              return $                 /*use memoization;   return #. */
```

**output**   when using the default input   (of 25 rows):

```txt

                                                1
                                              1   1
                                            1   1   1
                                          1   2   1   1
                                        1   2   2   1   1
                                      1   3   3   2   1   1
                                    1   3   4   3   2   1   1
                                  1   4   5   5   3   2   1   1
                                1   4   7   6   5   3   2   1   1
                              1   5   8   9   7   5   3   2   1   1
                            1   5  10  11  10   7   5   3   2   1   1
                          1   6  12  15  13  11   7   5   3   2   1   1
                        1   6  14  18  18  14  11   7   5   3   2   1   1
                      1   7  16  23  23  20  15  11   7   5   3   2   1   1
                    1   7  19  27  30  26  21  15  11   7   5   3   2   1   1
                  1   8  21  34  37  35  28  22  15  11   7   5   3   2   1   1
                1   8  24  39  47  44  38  29  22  15  11   7   5   3   2   1   1
              1   9  27  47  57  58  49  40  30  22  15  11   7   5   3   2   1   1
            1   9  30  54  70  71  65  52  41  30  22  15  11   7   5   3   2   1   1
          1  10  33  64  84  90  82  70  54  42  30  22  15  11   7   5   3   2   1   1
        1  10  37  72 101 110 105  89  73  55  42  30  22  15  11   7   5   3   2   1   1
      1  11  40  84 119 136 131 116  94  75  56  42  30  22  15  11   7   5   3   2   1   1
    1  11  44  94 141 163 164 146 123  97  76  56  42  30  22  15  11   7   5   3   2   1   1
  1  12  48 108 164 199 201 186 157 128  99  77  56  42  30  22  15  11   7   5   3   2   1   1
1  12  52 120 192 235 248 230 201 164 131 100  77  56  42  30  22  15  11   7   5   3   2   1   1
         G(25): 1958
partitions(25): 1958

```

**output**   when using the input:   <tt> -23 </tt>

```txt

         G(23): 1255
partitions(23): 1255

```

**output**   when using the input:   <tt> -123 </tt>

```txt

         G(123): 2552338241
partitions(123): 2552338241

```

**output**   when using the input:   <tt> -1234 </tt>

```txt

         G(1234): 156978797223733228787865722354959930
partitions(1234): 156978797223733228787865722354959930

```

**output**   when using the input:   <tt> -12345 </tt>

```txt

         G(12345): 69420357953926116819562977205209384460667673094671463620270321700806074195845953959951425306140971942519870679768681736
partitions(12345): 69420357953926116819562977205209384460667673094671463620270321700806074195845953959951425306140971942519870679768681736

```

**output**   when using the input:   <tt> +123456 </tt>

```txt

partitions(123456): 30817659578536496678545317146533980855296613274507139217608776782063054452191537379312358383342446230621170608408020911309259407611257151683372221925128388387168451943800027128045369650890220060901494540459081545445020808726917371699102825508039173543836338081612528477859613355349851184591540231790254269948278726548570660145691076819912972162262902150886818986555127204165221706149989

```

(For the extra credit part)   to view a horizontal histogram (plot) for the values for the number of partitions of   1 ──► 999   here at:

:::::: [9 billion names of God the integer (REXX) histogram](/tasks/9 billion names of God the integer (REXX) histogram).


## Ruby


### Naive Solution


```ruby

# Generate IPF triangle
# Nigel_Galloway: May 1st., 2013.
def g(n,g)
  return 1 unless 1 < g and g < n-1
  (2..g).inject(1){|res,q| res + (q > n-g ? 0 : g(n-g,q))}
end

(1..25).each {|n|
  puts (1..n).map {|g| "%4s" % g(n,g)}.join
}

```

### Output

```txt

   1
   1   1
   1   1   1
   1   2   1   1
   1   2   2   1   1
   1   3   3   2   1   1
   1   3   4   3   2   1   1
   1   4   5   5   3   2   1   1
   1   4   7   6   5   3   2   1   1
   1   5   8   9   7   5   3   2   1   1
   1   5  10  11  10   7   5   3   2   1   1
   1   6  12  15  13  11   7   5   3   2   1   1
   1   6  14  18  18  14  11   7   5   3   2   1   1
   1   7  16  23  23  20  15  11   7   5   3   2   1   1
   1   7  19  27  30  26  21  15  11   7   5   3   2   1   1
   1   8  21  34  37  35  28  22  15  11   7   5   3   2   1   1
   1   8  24  39  47  44  38  29  22  15  11   7   5   3   2   1   1
   1   9  27  47  57  58  49  40  30  22  15  11   7   5   3   2   1   1
   1   9  30  54  70  71  65  52  41  30  22  15  11   7   5   3   2   1   1
   1  10  33  64  84  90  82  70  54  42  30  22  15  11   7   5   3   2   1   1
   1  10  37  72 101 110 105  89  73  55  42  30  22  15  11   7   5   3   2   1   1
   1  11  40  84 119 136 131 116  94  75  56  42  30  22  15  11   7   5   3   2   1   1
   1  11  44  94 141 163 164 146 123  97  76  56  42  30  22  15  11   7   5   3   2   1   1
   1  12  48 108 164 199 201 186 157 128  99  77  56  42  30  22  15  11   7   5   3   2   1   1
   1  12  52 120 192 235 248 230 201 164 131 100  77  56  42  30  22  15  11   7   5   3   2   1   1

```



### Full Solution


```ruby

# Find large values of IPF
# Nigel_Galloway: May 1st., 2013.
N = 12345
@ng = []
@ipn1 = []
@ipn2 = []
def g(n,g)
  t = n-g-2
  return 1 if n<4 or t<0
  return @ng[g-2][n-4] unless n/2<g
  return @ipn1[t]
end
@ng[0] = []
(4..N).each {|q| @ng[0][q-4] = 1 + g(q-2,2)}
@ipn1[0] = @ng[0][0]
@ipn2[0] = @ng[0][N-4]
(1...(N/2-1)).each {|n|
  @ng[n] = []
  (n*2+4..N).each {|q| @ng[n][q-4] = g(q-1,n+1) + g(q-n-2,n+2)}
  @ipn1[n] = @ng[n][n*2]
  @ipn2[n] = @ng[n][N-4]
  @ng[n-1] = nil
}
@ipn2.pop if N.even?

puts "G(23) = #{@ipn1[21]}"
puts "G(123) = #{@ipn1[121]}"
puts "G(1234) = #{@ipn1[1232]}"
n = 3 + @ipn1.inject(:+) + @ipn2.inject(:+)
puts "G(12345) = #{n}"

```

### Output

```txt

G(23) = 1255
G(123) = 2552338241
G(1234) = 156978797223733228787865722354959930
G(12345) = 69420357953926116819562977205209384460667673094671463620270321700806074195845953959951425306140971942519870679768681736

```



## Rust

{{trans|Python}}

```rust
extern crate num;

use std::cmp;
use num::bigint::BigUint;

fn cumu(n: usize, cache: &mut Vec<Vec<BigUint>>) {
    for l in cache.len()..n+1 {
        let mut r = vec![BigUint::from(0u32)];
        for x in 1..l+1 {
            let prev = r[r.len() - 1].clone();
            r.push(prev + cache[l-x][cmp::min(x, l-x)].clone());
        }
        cache.push(r);
    }
}

fn row(n: usize, cache: &mut Vec<Vec<BigUint>>) -> Vec<BigUint> {
    cumu(n, cache);
    let r = &cache[n];
    let mut v: Vec<BigUint> = Vec::new();

    for i in 0..n {
        v.push(&r[i+1] - &r[i]);
    }
    v
}

fn main() {
    let mut cache = vec![vec![BigUint::from(1u32)]];

    println!("rows:");
    for x in 1..26 {
        let v: Vec<String> = row(x, &mut cache).iter().map(|e| e.to_string()).collect();
        let s: String = v.join(" ");
        println!("{}: {}", x, s);
    }

    println!("sums:");
    for x in vec![23, 123, 1234, 12345] {
        cumu(x, &mut cache);
        let v = &cache[x];
        let s = v[v.len() - 1].to_string();
        println!("{}: {}", x, s);
    }
}
```

### Output

```txt
rows:
1: 1
2: 1 1
3: 1 1 1
4: 1 2 1 1
5: 1 2 2 1 1
6: 1 3 3 2 1 1
7: 1 3 4 3 2 1 1
8: 1 4 5 5 3 2 1 1
9: 1 4 7 6 5 3 2 1 1
10: 1 5 8 9 7 5 3 2 1 1
11: 1 5 10 11 10 7 5 3 2 1 1
12: 1 6 12 15 13 11 7 5 3 2 1 1
13: 1 6 14 18 18 14 11 7 5 3 2 1 1
14: 1 7 16 23 23 20 15 11 7 5 3 2 1 1
15: 1 7 19 27 30 26 21 15 11 7 5 3 2 1 1
16: 1 8 21 34 37 35 28 22 15 11 7 5 3 2 1 1
17: 1 8 24 39 47 44 38 29 22 15 11 7 5 3 2 1 1
18: 1 9 27 47 57 58 49 40 30 22 15 11 7 5 3 2 1 1
19: 1 9 30 54 70 71 65 52 41 30 22 15 11 7 5 3 2 1 1
20: 1 10 33 64 84 90 82 70 54 42 30 22 15 11 7 5 3 2 1 1
21: 1 10 37 72 101 110 105 89 73 55 42 30 22 15 11 7 5 3 2 1 1
22: 1 11 40 84 119 136 131 116 94 75 56 42 30 22 15 11 7 5 3 2 1 1
23: 1 11 44 94 141 163 164 146 123 97 76 56 42 30 22 15 11 7 5 3 2 1 1
24: 1 12 48 108 164 199 201 186 157 128 99 77 56 42 30 22 15 11 7 5 3 2 1 1
25: 1 12 52 120 192 235 248 230 201 164 131 100 77 56 42 30 22 15 11 7 5 3 2 1 1
sums:
23: 1255
123: 2552338241
1234: 156978797223733228787865722354959930
12345: 69420357953926116819562977205209384460667673094671463620270321700806074195845953959951425306140971942519870679768681736

```



## Scala


### Naive Solution


```scala

object Main {

  // This is a special class for memoization
  case class Memo[A,B](f: A => B) extends (A => B) {
	  private val cache = Map.empty[A, B]
	  def apply(x: A) = cache getOrElseUpdate (x, f(x))
  }

  // Naive, but memoized solution
  lazy val namesStartingMemo : Memo[Tuple2[Int, Int], BigInt] = Memo {
    case (1, 1) => 1
    case (a, n) =>
	    if (a > n/2) namesStartingMemo(a - 1, n - 1)
	    else if (n < a) 0
	    else if (n == a) 1
	    else (1 to a).map(i => namesStartingMemo(i, n - a)).sum

  }

  def partitions(n: Int) = (1 to n).map(namesStartingMemo(_, n)).sum

  // main method
  def main(args: Array[String]): Unit = {
    for (i <- 1 to 25) {
    	for (j <- 1 to i) {
	      print(namesStartingMemo(j, i));
	      print(' ');
	    }
    	println()
    }
    println(partitions(23))
    println(partitions(123))
    println(partitions(1234))
    println(partitions(12345))
  }
}

```

### Output

```txt

1
1 1
1 1 1
1 2 1 1
1 2 2 1 1
1 3 3 2 1 1
1 3 4 3 2 1 1
1 4 5 5 3 2 1 1
1 4 7 6 5 3 2 1 1
1 5 8 9 7 5 3 2 1 1
1 5 10 11 10 7 5 3 2 1 1
1 6 12 15 13 11 7 5 3 2 1 1
1 6 14 18 18 14 11 7 5 3 2 1 1
1 7 16 23 23 20 15 11 7 5 3 2 1 1
1 7 19 27 30 26 21 15 11 7 5 3 2 1 1
1 8 21 34 37 35 28 22 15 11 7 5 3 2 1 1
1 8 24 39 47 44 38 29 22 15 11 7 5 3 2 1 1
1 9 27 47 57 58 49 40 30 22 15 11 7 5 3 2 1 1
1 9 30 54 70 71 65 52 41 30 22 15 11 7 5 3 2 1 1
1 10 33 64 84 90 82 70 54 42 30 22 15 11 7 5 3 2 1 1
1 10 37 72 101 110 105 89 73 55 42 30 22 15 11 7 5 3 2 1 1
1 11 40 84 119 136 131 116 94 75 56 42 30 22 15 11 7 5 3 2 1 1
1 11 44 94 141 163 164 146 123 97 76 56 42 30 22 15 11 7 5 3 2 1 1
1 12 48 108 164 199 201 186 157 128 99 77 56 42 30 22 15 11 7 5 3 2 1 1
1 12 52 120 192 235 248 230 201 164 131 100 77 56 42 30 22 15 11 7 5 3 2 1 1
1255
2552338241
156978797223733228787865722354959930
Exception in thread "main" java.lang.StackOverflowError
	at scala.collection.mutable.HashTable$class.findEntry(HashTable.scala:130)
	at scala.collection.mutable.HashMap.findEntry(HashMap.scala:39)
	at scala.collection.mutable.HashMap.get(HashMap.scala:69)
	at scala.collection.mutable.MapLike$class.getOrElseUpdate(MapLike.scala:187)
	at scala.collection.mutable.AbstractMap.getOrElseUpdate(Map.scala:91)
	at Main$Memo.apply(Main.scala:14)
        ...

```

<small>(As you see, partitions(12345) fails with StackOverflowError)</small>


### Full Solution


```Scala
val cache = new Array[BigInt](15000)
cache(0) = 1
val cacheNaive = scala.collection.mutable.Map[Tuple2[Int, Int], BigInt]()

def p(n: Int, k: Int): BigInt = cacheNaive.getOrElseUpdate((n, k), (n, k) match {
    case (n, 1) => 1
    case (n, k) if n < k => 0
    case (n, k) if n == k => 1
    case (n, k) =>
        if (k > n/2) p(n - 1, k - 1)
        else p(n - 1, k - 1) + p(n - k, k)
})

def partitions(n: Int) = (1 to n).map(p(n, _)).sum

def updateCache(n: Int, d: Int, k: Int) =
    if ((k & 1) == 1) cache(n) = cache(n) + cache(d)
    else cache(n) = cache(n) - cache(d)

def quickPartitions(n: Int): BigInt = {
    cache(n) = 0
    for (k <- 1 to n) {
        val d = n - k * (3 * k - 1) / 2
        if (d >= 0) {
            updateCache(n, d, k)

            val e = d - k
            if (e >= 0) {
                updateCache(n, e, k)
            }
        }
    }
    cache(n)
}

for (i <- 1 to 23) {
    for (j <- 1 to i) {
        print(f"${p(i, j)}%4d")
    }
    println
}
println(partitions(23))

for (i <- 1 until cache.length) {
    quickPartitions(i)
}
println(quickPartitions(123))
println(quickPartitions(1234))
println(quickPartitions(12345))
```

### Output

```txt
   1
   1   1
   1   1   1
   1   2   1   1
   1   2   2   1   1
   1   3   3   2   1   1
   1   3   4   3   2   1   1
   1   4   5   5   3   2   1   1
   1   4   7   6   5   3   2   1   1
   1   5   8   9   7   5   3   2   1   1
   1   5  10  11  10   7   5   3   2   1   1
   1   6  12  15  13  11   7   5   3   2   1   1
   1   6  14  18  18  14  11   7   5   3   2   1   1
   1   7  16  23  23  20  15  11   7   5   3   2   1   1
   1   7  19  27  30  26  21  15  11   7   5   3   2   1   1
   1   8  21  34  37  35  28  22  15  11   7   5   3   2   1   1
   1   8  24  39  47  44  38  29  22  15  11   7   5   3   2   1   1
   1   9  27  47  57  58  49  40  30  22  15  11   7   5   3   2   1   1
   1   9  30  54  70  71  65  52  41  30  22  15  11   7   5   3   2   1   1
   1  10  33  64  84  90  82  70  54  42  30  22  15  11   7   5   3   2   1   1
   1  10  37  72 101 110 105  89  73  55  42  30  22  15  11   7   5   3   2   1   1
   1  11  40  84 119 136 131 116  94  75  56  42  30  22  15  11   7   5   3   2   1   1
   1  11  44  94 141 163 164 146 123  97  76  56  42  30  22  15  11   7   5   3   2   1   1
1255
2552338241
156978797223733228787865722354959930
69420357953926116819562977205209384460667673094671463620270321700806074195845953959951425306140971942519870679768681736
```



## scheme


```scheme
(define (f m n)
  (define (sigma g x y)
    (define (sum i)
      (if (< i 0) 0 (+ (f x (- y i) ) (sum (- i 1)))))
    (sum y))
  (cond ((eq? m n) 1)
        ((eq? n 1) 1)
        ((eq? n 0) 0)
        ((< m n) (f m m))
        ((< (/ m 2) n) (sigma f (- m n) (- m n)))
        (else (sigma f (- m n) n))))
(define (line m)
  (define (connect i)
    (if (> i m) '() (cons (f m i) (connect (+ i 1)))))
  (connect 1))
(define (print x)
  (define (print-loop i)
    (cond ((< i x) (begin (display (line i)) (display "\n") (print-loop (+ i 1)) ))))
  (print-loop 1))
(print 25)
```


### Output

```txt

(1)
(1 1)
(1 1 1)
(1 2 1 1)
(1 2 2 1 1)
(1 3 3 2 1 1)
(1 3 4 3 2 1 1)
(1 4 5 5 3 2 1 1)
(1 4 7 6 5 3 2 1 1)
(1 5 8 9 7 5 3 2 1 1)
(1 5 10 11 10 7 5 3 2 1 1)
(1 6 12 15 13 11 7 5 3 2 1 1)
(1 6 14 18 18 14 11 7 5 3 2 1 1)
(1 7 16 23 23 20 15 11 7 5 3 2 1 1)
(1 7 19 27 30 26 21 15 11 7 5 3 2 1 1)
(1 8 21 34 37 35 28 22 15 11 7 5 3 2 1 1)
(1 8 24 39 47 44 38 29 22 15 11 7 5 3 2 1 1)
(1 9 27 47 57 58 49 40 30 22 15 11 7 5 3 2 1 1)
(1 9 30 54 70 71 65 52 41 30 22 15 11 7 5 3 2 1 1)
(1 10 33 64 84 90 82 70 54 42 30 22 15 11 7 5 3 2 1 1)
(1 10 37 72 101 110 105 89 73 55 42 30 22 15 11 7 5 3 2 1 1)
(1 11 40 84 119 136 131 116 94 75 56 42 30 22 15 11 7 5 3 2 1 1)
(1 11 44 94 141 163 164 146 123 97 76 56 42 30 22 15 11 7 5 3 2 1 1)
(1 12 48 108 164 199 201 186 157 128 99 77 56 42 30 22 15 11 7 5 3 2 1 1)

```



## Sidef


```ruby
var cache = [[1]]

func cumu (n) {
    for l (cache.len .. n) {
        var r = [0]
        for i (1..l) {
            r << (r[-1] + cache[l-i][min(i, l-i)])
        }
        cache << r
    }
    cache[n]
}

func row (n) {
    var r = cumu(n)
    n.of {|i| r[i+1] - r[i] }
}

say "rows:"
for i (1..15) {
    "%2s: %s\n".printf(i, row(i))
}

say "\nsums:"

for i in [23, 123, 1234, 12345] {
    "%2s : %4s\n".printf(i, cumu(i)[-1])
}
```


### Output

```txt

rows:
 1: [1]
 2: [1, 1]
 3: [1, 1, 1]
 4: [1, 2, 1, 1]
 5: [1, 2, 2, 1, 1]
 6: [1, 3, 3, 2, 1, 1]
 7: [1, 3, 4, 3, 2, 1, 1]
 8: [1, 4, 5, 5, 3, 2, 1, 1]
 9: [1, 4, 7, 6, 5, 3, 2, 1, 1]
10: [1, 5, 8, 9, 7, 5, 3, 2, 1, 1]
11: [1, 5, 10, 11, 10, 7, 5, 3, 2, 1, 1]
12: [1, 6, 12, 15, 13, 11, 7, 5, 3, 2, 1, 1]
13: [1, 6, 14, 18, 18, 14, 11, 7, 5, 3, 2, 1, 1]
14: [1, 7, 16, 23, 23, 20, 15, 11, 7, 5, 3, 2, 1, 1]
15: [1, 7, 19, 27, 30, 26, 21, 15, 11, 7, 5, 3, 2, 1, 1]

sums:
23 : 1255
123 : 2552338241
1234 : 156978797223733228787865722354959930
^C

```



## SPL


```spl
'print triangle
> n, 1..25
  k = 50-n*2
  #.output(#.str("","<"+k+"<"),#.rs)
  > k, 1..n
    i = p(n,k)
    s = #.str(i,">3<")
    ? k<n, s += " "+#.rs
    #.output(s)
  <
<
p(n,k)=
  ? k=0 | k>n, <= 0
  ? k=n, <= 1
  <= p(n-1,k-1)+p(n-k,k)
.

'calculate partition function
#.output()
#.output("G(23) =    ",g(23))
#.output("G(123) =   ",g(123))
#.output("G(1234) =  ",g(1234))
#.output("G(12345) = ",g(12345))
g(n)=
  p[1] = 1
  > i, 2..n+1
    j = 2
    k,p[i] = 0
    > j>1
      k += 1
      j = i-#.lower((3*k*k+k)/2)
      ? j!<1, p[i] -= (-1)^k*p[j]
      j = i-#.lower((3*k*k-k)/2)
      ? j!<1, p[i] -= (-1)^k*p[j]
    <
  <
  <= p[n+1]
.
```

### Output

```txt

                                                 1
                                               1   1
                                             1   1   1
                                           1   2   1   1
                                         1   2   2   1   1
                                       1   3   3   2   1   1
                                     1   3   4   3   2   1   1
                                   1   4   5   5   3   2   1   1
                                 1   4   7   6   5   3   2   1   1
                               1   5   8   9   7   5   3   2   1   1
                             1   5  10  11  10   7   5   3   2   1   1
                           1   6  12  15  13  11   7   5   3   2   1   1
                         1   6  14  18  18  14  11   7   5   3   2   1   1
                       1   7  16  23  23  20  15  11   7   5   3   2   1   1
                     1   7  19  27  30  26  21  15  11   7   5   3   2   1   1
                   1   8  21  34  37  35  28  22  15  11   7   5   3   2   1   1
                 1   8  24  39  47  44  38  29  22  15  11   7   5   3   2   1   1
               1   9  27  47  57  58  49  40  30  22  15  11   7   5   3   2   1   1
             1   9  30  54  70  71  65  52  41  30  22  15  11   7   5   3   2   1   1
           1  10  33  64  84  90  82  70  54  42  30  22  15  11   7   5   3   2   1   1
         1  10  37  72  101 110 105 89  73  55  42  30  22  15  11   7   5   3   2   1   1
       1  11  40  84  119 136 131 116 94  75  56  42  30  22  15  11   7   5   3   2   1   1
     1  11  44  94  141 163 164 146 123 97  76  56  42  30  22  15  11   7   5   3   2   1   1
   1  12  48  108 164 199 201 186 157 128 99  77  56  42  30  22  15  11   7   5   3   2   1   1
 1  12  52  120 192 235 248 230 201 164 131 100 77  56  42  30  22  15  11   7   5   3   2   1   1

G(23) =    1255
G(123) =   2552338241
G(1234) =  156978797223733228787865722354959930
G(12345) = 69420357953926116819562977205209384460667673094671463620270321700806074195845953959951425306140971942519870679768681736

```



## Stata


```stata
mata
function part(n) {
	a = J(n,n,.)
	for (i=1;i<=n;i++) a[i,1] = a[i,i] = 1
	for (i=3;i<=n;i++) {
		for (j=2;j<i;j++) a[i,j] = sum(a[i-j,1..min((j,i-j))])
	}
	return(a)
}
end
```


The result is shown for n=10 to keep it small. Due to computations being done in floating point, the result is exact up to n=299, and suffers rounding for larger values of n. Compare the array with [OEIS A008284](http://oeis.org/A008284) and row sums with [OEIS A000041](http://oeis.org/A000041).

**Output**


```txt
: a = part(10)
: a
         1    2    3    4    5    6    7    8    9   10
     +---------------------------------------------------+
   1 |   1    .    .    .    .    .    .    .    .    .  |
   2 |   1    1    .    .    .    .    .    .    .    .  |
   3 |   1    1    1    .    .    .    .    .    .    .  |
   4 |   1    2    1    1    .    .    .    .    .    .  |
   5 |   1    2    2    1    1    .    .    .    .    .  |
   6 |   1    3    3    2    1    1    .    .    .    .  |
   7 |   1    3    4    3    2    1    1    .    .    .  |
   8 |   1    4    5    5    3    2    1    1    .    .  |
   9 |   1    4    7    6    5    3    2    1    1    .  |
  10 |   1    5    8    9    7    5    3    2    1    1  |
     +---------------------------------------------------+

: rowsum(a)'
        1    2    3    4    5    6    7    8    9   10
    +---------------------------------------------------+
  1 |   1    2    3    5    7   11   15   22   30   42  |
    +---------------------------------------------------+
```



## Swift

{{trans|Python}}

```Swift
var cache = [[1]]
func namesOfGod(n:Int) -> [Int] {
    for l in cache.count...n {
        var r = [0]
        for x in 1...l {
            r.append(r[r.count - 1] + cache[l - x][min(x, l-x)])
        }
        cache.append(r)
    }
    return cache[n]
}

func row(n:Int) -> [Int] {
    let r = namesOfGod(n)
    var returnArray = [Int]()
    for i in 0...n - 1 {
        returnArray.append(r[i + 1] - r[i])
    }
    return returnArray
}

println("rows:")
for x in 1...25 {
    println("\(x): \(row(x))")
}

println("\nsums: ")

for x in [23, 123, 1234, 12345] {
    cache = [[1]]
    var array = namesOfGod(x)
    var numInt = array[array.count - 1]
    println("\(x): \(numInt)")
}
```

### Output

```txt

rows:
1: [1]
2: [1, 1]
3: [1, 1, 1]
4: [1, 2, 1, 1]
5: [1, 2, 2, 1, 1]
6: [1, 3, 3, 2, 1, 1]
7: [1, 3, 4, 3, 2, 1, 1]
8: [1, 4, 5, 5, 3, 2, 1, 1]
9: [1, 4, 7, 6, 5, 3, 2, 1, 1]
10: [1, 5, 8, 9, 7, 5, 3, 2, 1, 1]
11: [1, 5, 10, 11, 10, 7, 5, 3, 2, 1, 1]
12: [1, 6, 12, 15, 13, 11, 7, 5, 3, 2, 1, 1]
13: [1, 6, 14, 18, 18, 14, 11, 7, 5, 3, 2, 1, 1]
14: [1, 7, 16, 23, 23, 20, 15, 11, 7, 5, 3, 2, 1, 1]
15: [1, 7, 19, 27, 30, 26, 21, 15, 11, 7, 5, 3, 2, 1, 1]
16: [1, 8, 21, 34, 37, 35, 28, 22, 15, 11, 7, 5, 3, 2, 1, 1]
17: [1, 8, 24, 39, 47, 44, 38, 29, 22, 15, 11, 7, 5, 3, 2, 1, 1]
18: [1, 9, 27, 47, 57, 58, 49, 40, 30, 22, 15, 11, 7, 5, 3, 2, 1, 1]
19: [1, 9, 30, 54, 70, 71, 65, 52, 41, 30, 22, 15, 11, 7, 5, 3, 2, 1, 1]
20: [1, 10, 33, 64, 84, 90, 82, 70, 54, 42, 30, 22, 15, 11, 7, 5, 3, 2, 1, 1]
21: [1, 10, 37, 72, 101, 110, 105, 89, 73, 55, 42, 30, 22, 15, 11, 7, 5, 3, 2, 1, 1]
22: [1, 11, 40, 84, 119, 136, 131, 116, 94, 75, 56, 42, 30, 22, 15, 11, 7, 5, 3, 2, 1, 1]
23: [1, 11, 44, 94, 141, 163, 164, 146, 123, 97, 76, 56, 42, 30, 22, 15, 11, 7, 5, 3, 2, 1, 1]
24: [1, 12, 48, 108, 164, 199, 201, 186, 157, 128, 99, 77, 56, 42, 30, 22, 15, 11, 7, 5, 3, 2, 1, 1]
25: [1, 12, 52, 120, 192, 235, 248, 230, 201, 164, 131, 100, 77, 56, 42, 30, 22, 15, 11, 7, 5, 3, 2, 1, 1]

sums:
    23: 1255
   123: 2552338241
  1234: 156978797223733228787865722354959930
 12345: 69420357953926116819562977205209384460667673094671463620270321700806074195845953959951425306140971942519870679768681736

```



## Tcl

{{trans|Python}}

```tcl
set cache 1
proc cumu {n} {
    global cache
    for {set l [llength $cache]} {$l <= $n} {incr l} {
	set r 0
	for {set x 1; set y [expr {$l-1}]} {$y >= 0} {incr x; incr y -1} {
	    lappend r [expr {
		[lindex $r end] + [lindex $cache $y [expr {min($x, $y)}]]
	    }]
	}
	lappend cache $r
    }
    return [lindex $cache $n]
}
proc row {n} {
    set r [cumu $n]
    for {set i 0; set j 1} {$j < [llength $r]} {incr i; incr j} {
	lappend result [expr {[lindex $r $j] - [lindex $r $i]}]
    }
    return $result
}

puts "rows:"
foreach x {1 2 3 4 5 6 7 8 9 10} {
    puts "${x}: \[[join [row $x] {, }]\]"
}
puts "\nsums:"
foreach x {23 123 1234 12345} {
    puts "${x}: [lindex [cumu $x] end]"
}
```

### Output

```txt

rows:
1: [1]
2: [1, 1]
3: [1, 1, 1]
4: [1, 2, 1, 1]
5: [1, 2, 2, 1, 1]
6: [1, 3, 3, 2, 1, 1]
7: [1, 3, 4, 3, 2, 1, 1]
8: [1, 4, 5, 5, 3, 2, 1, 1]
9: [1, 4, 7, 6, 5, 3, 2, 1, 1]
10: [1, 5, 8, 9, 7, 5, 3, 2, 1, 1]

sums:
23: 1255
123: 2552338241
1234: 156978797223733228787865722354959930
^C

```

<small>(I killed the run when it started to take a significant proportion of my system's memory.)</small>


## VBA


```vb
Public Sub nine_billion_names()
    Dim p(25, 25) As Long
    p(1, 1) = 1
    For i = 2 To 25
        For j = 1 To i
            p(i, j) = p(i - 1, j - 1) + p(i - j, j)
        Next j
    Next i
    For i = 1 To 25
        Debug.Print String$(50 - 2 * i, " ");
        For j = 1 To i
            Debug.Print String$(4 - Len(CStr(p(i, j))), " ") & p(i, j);
        Next j
        Debug.Print
    Next i
End Sub
```
### Output

```txt
                                                   1
                                                 1   1
                                               1   1   1
                                             1   2   1   1
                                           1   2   2   1   1
                                         1   3   3   2   1   1
                                       1   3   4   3   2   1   1
                                     1   4   5   5   3   2   1   1
                                   1   4   7   6   5   3   2   1   1
                                 1   5   8   9   7   5   3   2   1   1
                               1   5  10  11  10   7   5   3   2   1   1
                             1   6  12  15  13  11   7   5   3   2   1   1
                           1   6  14  18  18  14  11   7   5   3   2   1   1
                         1   7  16  23  23  20  15  11   7   5   3   2   1   1
                       1   7  19  27  30  26  21  15  11   7   5   3   2   1   1
                     1   8  21  34  37  35  28  22  15  11   7   5   3   2   1   1
                   1   8  24  39  47  44  38  29  22  15  11   7   5   3   2   1   1
                 1   9  27  47  57  58  49  40  30  22  15  11   7   5   3   2   1   1
               1   9  30  54  70  71  65  52  41  30  22  15  11   7   5   3   2   1   1
```



## Yabasic

{{trans|VBA}}

```Yabasic
clear screen

Sub nine_billion_names(rows)
    local p(rows, rows), i, j, column

    p(1, 1) = 1

    For i = 2 To rows
        For j = 1 To i
            p(i, j) = p(i - 1, j - 1) + p(i - j, j)
        Next j
    Next i
    For i = 1 To rows
        column = rows * 2 - 2 * i - 2
        For j = 1 To i
            Print at(column + j * 4 + (1 - len(str$(p(i, j)))), i), p(i, j)
        Next j
    Next i
End Sub

nine_billion_names(20)
```



## zkl

{{trans|C}}
Takes its time getting to 100,000 but it does. Uses the GMP big int library. Does the big int math in place to avoid garbage creation.

```zkl
var [const] BN=Import.lib("zklBigNum");

const N=0d100_000;
p:=List.createLong(N+1,BN.fp(0),True);  // (0,0,...) all different

fcn calc(n,p){
   p[n].set(0);  // reset array for each run
   foreach k in ([1..n]){
      d:=n - k *(3*k - 1)/2;
      do(2){
         if (d<0) break(2);
	 if (k.isOdd) p[n].add(p[d]);
	 else         p[n].sub(p[d]);
	 d-=k;
      }
   }
}
```


```zkl
idx:=T(23, 123, 1234, 12345, 20000, 30000, 40000, 50000, N);
p[0].set(1);

foreach i in (idx){
   (1).pump(i,Void,calc.fp1(p));	// for n in [1..i] do calc(n,p)
   "%2d:\t%d".fmt(i,p[i]).println();
}
```

The .fp/.fp1 methods create a closure, fixing the first or second parameter.
### Output

```txt

23:	1255
123:	2552338241
1234:	156978797223733228787865722354959930
12345:	69420357953926116819562977205209384460667673094671463620270321700806074195845953959951425306140971942519870679768681736
...
100000:	27493510569775696512677516320986352688173429315980054758203125984302147328114964173055050741660736621590157844774296248940493063070200461792764493033510116079342457190155718943509725312466108452006369558934464248716828789832182345009262853831404597021307130674510624419227311238999702284408609370935531629697851569569892196108480158600569421098519

```

