+++
title = "Display a linear combination"
description = ""
date = 2019-09-09T23:37:53Z
aliases = []
[extra]
id = 19648
[taxonomies]
categories = ["task"]
tags = []
+++

## Task

Display a finite [[wp:linear combination|linear combination]] in an infinite vector basis <big><math>(e_1, e_2,\ldots)</math></big>.

Write a function that, when given a finite list of scalars <big><math>(\alpha^1,\alpha^2,\ldots)</math></big>,
creates a string representing the linear combination <big><math>\sum_i\alpha^i e_i</math></big> in an explicit format often used in mathematics, that is:

:<big><math>\alpha^{i_1}e_{i_1}\pm|\alpha^{i_2}|e_{i_2}\pm|\alpha^{i_3}|e_{i_3}\pm\ldots</math></big>

where <big><math>\alpha^{i_k}\neq 0</math></big>




The output must comply to the following rules:
*   don't show null terms, unless the whole combination is null.
:::::::  '''e(1)'''     is fine,     '''e(1) + 0*e(3)'''     or     '''e(1) + 0'''     is wrong.
*   don't show scalars when they are equal to one or minus one.
:::::::  '''e(3)'''     is fine,     '''1*e(3)'''     is wrong.
*   don't prefix by a minus sign if it follows a preceding term.   Instead you use subtraction.
:::::::  '''e(4) - e(5)'''     is fine,     '''e(4) + -e(5)'''     is wrong.



Show here output for the following lists of scalars:

```txt

 1)    1,  2,  3
 2)    0,  1,  2,  3
 3)    1,  0,  3,  4
 4)    1,  2,  0
 5)    0,  0,  0
 6)    0
 7)    1,  1,  1
 8)   -1, -1, -1
 9)   -1, -2,  0, -3
10)   -1

```






## 11l

```11l
F linear(x)
   V a = enumerate(x).filter2((i, v) -> v != 0).map2((i, v) -> ‘#.e(#.)’.format(I v == -1 {‘-’} E I v == 1 {‘’} E String(v)‘*’, i + 1))
   R (I !a.empty {a} E [String(‘0’)]).join(‘ + ’).replace(‘ + -’, ‘ - ’)

L(x) [[1, 2, 3], [0, 1, 2, 3], [1, 0, 3, 4], [1, 2, 0], [0, 0, 0], [0], [1, 1, 1], [-1, -1, -1], [-1, -2, 0, 3], [-1]]
   print(linear(x))
```

```txt

e(1) + 2*e(2) + 3*e(3)
e(2) + 2*e(3) + 3*e(4)
e(1) + 3*e(3) + 4*e(4)
e(1) + 2*e(2)
0
0
e(1) + e(2) + e(3)
-e(1) - e(2) - e(3)
-e(1) - 2*e(2) + 3*e(4)
-e(1)

```



## C

Accepts vector coefficients from the command line, prints usage syntax if invoked with no arguments. This implementation can handle floating point values but displays integer values as integers. All test case results shown with invocation. A multiplication sign is not shown between a coefficient and the unit vector when a vector is written out by hand ( i.e. human readable) and is thus not shown here as well.

```C


#include<stdlib.h>
#include<stdio.h>
#include<math.h> /*Optional, but better if included as fabs, labs and abs functions are being used. */

int main(int argC, char* argV[])
{

	int i,zeroCount= 0,firstNonZero = -1;
	double* vector;

	if(argC == 1){
		printf("Usage : %s <Vector component coefficients seperated by single space>",argV[0]);
	}

	else{

		printf("Vector for [");
		for(i=1;i<argC;i++){
			printf("%s,",argV[i]);
		}
		printf("\b] -> ");


		vector = (double*)malloc((argC-1)*sizeof(double));

		for(i=1;i<=argC;i++){
			vector[i-1] = atof(argV[i]);
			if(vector[i-1]==0.0)
				zeroCount++;
			if(vector[i-1]!=0.0 && firstNonZero==-1)
				firstNonZero = i-1;
		}

		if(zeroCount == argC){
			printf("0");
		}

		else{
			for(i=0;i<argC;i++){
				if(i==firstNonZero && vector[i]==1)
					printf("e%d ",i+1);
				else if(i==firstNonZero && vector[i]==-1)
					printf("- e%d ",i+1);
				else if(i==firstNonZero && vector[i]<0 && fabs(vector[i])-abs(vector[i])>0.0)
					printf("- %lf e%d ",fabs(vector[i]),i+1);
				else if(i==firstNonZero && vector[i]<0 && fabs(vector[i])-abs(vector[i])==0.0)
					printf("- %ld e%d ",labs(vector[i]),i+1);
				else if(i==firstNonZero && vector[i]>0 && fabs(vector[i])-abs(vector[i])>0.0)
					printf("%lf e%d ",vector[i],i+1);
				else if(i==firstNonZero && vector[i]>0 && fabs(vector[i])-abs(vector[i])==0.0)
					printf("%ld e%d ",vector[i],i+1);
				else if(fabs(vector[i])==1.0 && i!=0)
					printf("%c e%d ",(vector[i]==-1)?'-':'+',i+1);
				else if(i!=0 && vector[i]!=0 && fabs(vector[i])-abs(vector[i])>0.0)
					printf("%c %lf e%d ",(vector[i]<0)?'-':'+',fabs(vector[i]),i+1);
				else if(i!=0 && vector[i]!=0 && fabs(vector[i])-abs(vector[i])==0.0)
					printf("%c %ld e%d ",(vector[i]<0)?'-':'+',labs(vector[i]),i+1);
			}
		}
	}

	free(vector);

	return 0;
}

```

```txt

C:\rossetaCode>vectorDisplay.exe 1 2 3
Vector for [1,2,3] -> e1 + 2 e2 + 3 e3
C:\rossetaCode>vectorDisplay.exe 0 0 0
Vector for [0,0,0] -> 0
C:\rossetaCode>vectorDisplay.exe 0 1 2 3
Vector for [0,1,2,3] -> e2 + 2 e3 + 3 e4
C:\rossetaCode>vectorDisplay.exe 1 0 3 4
Vector for [1,0,3,4] -> e1 + 3 e3 + 4 e4
C:\rossetaCode>vectorDisplay.exe 1 2 0
Vector for [1,2,0] -> e1 + 2 e2
C:\rossetaCode>vectorDisplay.exe 0 0 0
Vector for [0,0,0] -> 0
C:\rossetaCode>vectorDisplay.exe 0
Vector for [0] -> 0
C:\rossetaCode>vectorDisplay.exe 1 1 1
Vector for [1,1,1] -> e1 + e2 + e3
C:\rossetaCode>vectorDisplay.exe -1 -1 -1
Vector for [-1,-1,-1] -> - e1 - e2 - e3
C:\rossetaCode>vectorDisplay.exe -1 -2 0 -3
Vector for [-1,-2,0,-3] -> - e1 - 2 e2 - 3 e4
C:\rossetaCode>vectorDisplay.exe -1
Vector for [-1] -> - e1

```



## C++

```cpp
#include <iomanip>
#include <iostream>
#include <sstream>
#include <vector>

template<typename T>
std::ostream& operator<<(std::ostream& os, const std::vector<T>& v) {
    auto it = v.cbegin();
    auto end = v.cend();

    os << '[';
    if (it != end) {
        os << *it;
        it = std::next(it);
    }
    while (it != end) {
        os << ", " << *it;
        it = std::next(it);
    }
    return os << ']';
}

std::ostream& operator<<(std::ostream& os, const std::string& s) {
    return os << s.c_str();
}

std::string linearCombo(const std::vector<int>& c) {
    std::stringstream ss;
    for (size_t i = 0; i < c.size(); i++) {
        int n = c[i];
        if (n < 0) {
            if (ss.tellp() == 0) {
                ss << '-';
            } else {
                ss << " - ";
            }
        } else if (n > 0) {
            if (ss.tellp() != 0) {
                ss << " + ";
            }
        } else {
            continue;
        }

        int av = abs(n);
        if (av != 1) {
            ss << av << '*';
        }
        ss << "e(" << i + 1 << ')';
    }
    if (ss.tellp() == 0) {
        return "0";
    }
    return ss.str();
}

int main() {
    using namespace std;

    vector<vector<int>> combos{
        {1, 2, 3},
        {0, 1, 2, 3},
        {1, 0, 3, 4},
        {1, 2, 0},
        {0, 0, 0},
        {0},
        {1, 1, 1},
        {-1, -1, -1},
        {-1, -2, 0, -3},
        {-1},
    };

    for (auto& c : combos) {
        stringstream ss;
        ss << c;
        cout << setw(15) << ss.str() << " -> ";
        cout << linearCombo(c) << '\n';
    }

    return 0;
}
```

```txt
      [1, 2, 3] -> e(1) + 2*e(2) + 3*e(3)
   [0, 1, 2, 3] -> e(2) + 2*e(3) + 3*e(4)
   [1, 0, 3, 4] -> e(1) + 3*e(3) + 4*e(4)
      [1, 2, 0] -> e(1) + 2*e(2)
      [0, 0, 0] -> 0
            [0] -> 0
      [1, 1, 1] -> e(1) + e(2) + e(3)
   [-1, -1, -1] -> -e(1) - e(2) - e(3)
[-1, -2, 0, -3] -> -e(1) - 2*e(2) - 3*e(4)
           [-1] -> -e(1)
```



## C#

```c#
using System;
using System.Collections.Generic;
using System.Text;

namespace DisplayLinearCombination {
    class Program {
        static string LinearCombo(List<int> c) {
            StringBuilder sb = new StringBuilder();
            for (int i = 0; i < c.Count; i++) {
                int n = c[i];
                if (n < 0) {
                    if (sb.Length == 0) {
                        sb.Append('-');
                    } else {
                        sb.Append(" - ");
                    }
                } else if (n > 0) {
                    if (sb.Length != 0) {
                        sb.Append(" + ");
                    }
                } else {
                    continue;
                }

                int av = Math.Abs(n);
                if (av != 1) {
                    sb.AppendFormat("{0}*", av);
                }
                sb.AppendFormat("e({0})", i + 1);
            }
            if (sb.Length == 0) {
                sb.Append('0');
            }
            return sb.ToString();
        }

        static void Main(string[] args) {
            List<List<int>> combos = new List<List<int>>{
                new List<int> { 1, 2, 3},
                new List<int> { 0, 1, 2, 3},
                new List<int> { 1, 0, 3, 4},
                new List<int> { 1, 2, 0},
                new List<int> { 0, 0, 0},
                new List<int> { 0},
                new List<int> { 1, 1, 1},
                new List<int> { -1, -1, -1},
                new List<int> { -1, -2, 0, -3},
                new List<int> { -1},
            };

            foreach (List<int> c in combos) {
                var arr = "[" + string.Join(", ", c) + "]";
                Console.WriteLine("{0,15} -> {1}", arr, LinearCombo(c));
            }
        }
    }
}
```

```txt
      [1, 2, 3] -> e(1) + 2*e(2) + 3*e(3)
   [0, 1, 2, 3] -> e(2) + 2*e(3) + 3*e(4)
   [1, 0, 3, 4] -> e(1) + 3*e(3) + 4*e(4)
      [1, 2, 0] -> e(1) + 2*e(2)
      [0, 0, 0] -> 0
            [0] -> 0
      [1, 1, 1] -> e(1) + e(2) + e(3)
   [-1, -1, -1] -> -e(1) - e(2) - e(3)
[-1, -2, 0, -3] -> -e(1) - 2*e(2) - 3*e(4)
           [-1] -> -e(1)
```



## D

```D
import std.array;
import std.conv;
import std.format;
import std.math;
import std.stdio;

string linearCombo(int[] c) {
    auto sb = appender!string;
    foreach (i, n; c) {
        if (n==0) continue;
        string op;
        if (n < 0) {
            if (sb.data.empty) {
                op = "-";
            } else {
                op = " - ";
            }
        } else if (n > 0) {
            if (!sb.data.empty) {
                op = " + ";
            }
        }
        auto av = abs(n);
        string coeff;
        if (av != 1) {
            coeff = to!string(av) ~ "*";
        }
        sb.formattedWrite("%s%se(%d)", op, coeff, i+1);
    }
    if (sb.data.empty) {
        return "0";
    }
    return sb.data;
}

void main() {
    auto combos = [
        [1, 2, 3],
        [0, 1, 2, 3],
        [1, 0, 3, 4],
        [1, 2, 0],
        [0, 0, 0],
        [0],
        [1, 1, 1],
        [-1, -1, -1],
        [-1, -2, 0, -3],
        [-1],
    ];
    foreach (c; combos) {
        auto arr = c.format!"%s";
        writefln("%-15s  ->  %s", arr, linearCombo(c));
    }
}
```

```txt
[1, 2, 3]        ->  e(1) + 2*e(2) + 3*e(3)
[0, 1, 2, 3]     ->  e(2) + 2*e(3) + 3*e(4)
[1, 0, 3, 4]     ->  e(1) + 3*e(3) + 4*e(4)
[1, 2, 0]        ->  e(1) + 2*e(2)
[0, 0, 0]        ->  0
[0]              ->  0
[1, 1, 1]        ->  e(1) + e(2) + e(3)
[-1, -1, -1]     ->  -e(1) - e(2) - e(3)
[-1, -2, 0, -3]  ->  -e(1) - 2*e(2) - 3*e(4)
[-1]             ->  -e(1)
```



## EchoLisp


```scheme

;; build an html string from list of coeffs

(define (linear->html coeffs)
    (define plus #f)
    (or*
    (for/fold (html "") ((a coeffs) (i (in-naturals 1)))
      (unless (zero? a)
 		(set! plus (if plus "+" "")))
      (string-append html
	 (cond
	  ((= a 1)  (format "%a e<sub>%d</sub> " plus i))
	  ((= a -1) (format "- e<sub>%d</sub> " i))
	  ((> a 0)  (format "%a %d*e<sub>%d</sub> " plus a i))
	  ((< a 0)  (format "- %d*e<sub>%d</sub> " (abs a) i))
	  (else ""))))
     "0"))

(define linears '((1 2 3)
   (0 1 2 3)
   (1 0 3 4)
   (1 2 0)
   (0 0 0)
   (0)
   (1 1 1)
   (-1 -1 -1)
   (-1 -2 0 -3)
   (-1)))

(define (task linears)
    (html-print ;; send string to stdout
    (for/string ((linear linears))
      (format "%a -> <span style='color:blue'>%a</span>
" linear (linear->html linear)))))

```

(1 2 3) -> <span style='color:blue'> e<sub>1</sub> + 2*e<sub>2</sub> + 3*e<sub>3</sub> </span>
(0 1 2 3) -> <span style='color:blue'> e<sub>2</sub> + 2*e<sub>3</sub> + 3*e<sub>4</sub> </span>
(1 0 3 4) -> <span style='color:blue'> e<sub>1</sub> + 3*e<sub>3</sub> + 4*e<sub>4</sub> </span>
(1 2 0) -> <span style='color:blue'> e<sub>1</sub> + 2*e<sub>2</sub> </span>
(0 0 0) -> <span style='color:blue'>0</span>
(0) -> <span style='color:blue'>0</span>
(1 1 1) -> <span style='color:blue'> e<sub>1</sub> + e<sub>2</sub> + e<sub>3</sub> </span>
(-1 -1 -1) -> <span style='color:blue'>- e<sub>1</sub> - e<sub>2</sub> - e<sub>3</sub> </span>
(-1 -2 0 -3) -> <span style='color:blue'>- e<sub>1</sub> - 2*e<sub>2</sub> - 3*e<sub>4</sub> </span>
(-1) -> <span style='color:blue'>- e<sub>1</sub> </span>



## Elixir

```elixir
defmodule Linear_combination do
  def display(coeff) do
    Enum.with_index(coeff)
    |> Enum.map_join(fn {n,i} ->
         {m,s} = if n<0, do: {-n,"-"}, else: {n,"+"}
         case {m,i} do
           {0,_} -> ""
           {1,i} -> "#{s}e(#{i+1})"
           {n,i} -> "#{s}#{n}*e(#{i+1})"
         end
       end)
    |> String.trim_leading("+")
    |> case do
         ""  -> IO.puts "0"
         str -> IO.puts str
       end
  end
end

coeffs =
  [ [1, 2, 3],
    [0, 1, 2, 3],
    [1, 0, 3, 4],
    [1, 2, 0],
    [0, 0, 0],
    [0],
    [1, 1, 1],
    [-1, -1, -1],
    [-1, -2, 0, -3],
    [-1]
  ]
Enum.each(coeffs, &Linear_combination.display(&1))
```


```txt

e(1)+2*e(2)+3*e(3)
e(2)+2*e(3)+3*e(4)
e(1)+3*e(3)+4*e(4)
e(1)+2*e(2)
0
0
e(1)+e(2)+e(3)
-e(1)-e(2)-e(3)
-e(1)-2*e(2)-3*e(4)
-e(1)

```


=={{header|F_Sharp|F#}}==

### The function


```fsharp

// Display a linear combination. Nigel Galloway: March 28th., 2018
let fN g =
  let rec fG n g=match g with
                 |0::g    ->                        fG (n+1) g
                 |1::g    -> printf "+e(%d)" n;     fG (n+1) g
                 |(-1)::g -> printf "-e(%d)" n;     fG (n+1) g
                 |i::g    -> printf "%+de(%d)" i n; fG (n+1) g
                 |_       -> printfn ""
  let rec fN n g=match g with
                 |0::g    ->                        fN (n+1) g
                 |1::g    -> printf "e(%d)" n;      fG (n+1) g
                 |(-1)::g -> printf "-e(%d)" n;     fG (n+1) g
                 |i::g    -> printf "%de(%d)" i n;  fG (n+1) g
                 |_       -> printfn "0"
  fN 1 g

```



### The Task


```fsharp

fN [1;2;3]

```

```txt

e(1)+2e(2)+3e(3)

```


```fsharp

fN [0;1;2;3]

```

```txt

e(2)+2e(3)+3e(4)

```


```fsharp

fN[1;0;3;4]

```

```txt

e(1)+3e(3)+4e(4)

```


```fsharp

fN[1;2;0]

```

```txt

e(1)+2e(2)

```


```fsharp

fN[0;0;0]

```

```txt

0

```


```fsharp

fN[0]

```

```txt

0

```


```fsharp

fN[1;1;1]

```

```txt

e(1)+e(2)+e(3)

```


```fsharp

fN[-1;-1;-1]

```

```txt

-e(1)-e(2)-e(3)

```


```fsharp

fN[-1;-2;0;-3]

```

```txt

-e(1)-2e(2)-3e(4)

```


```fsharp

fN[1]

```

```txt

e(1)

```



## Factor


```factor
USING: formatting kernel match math pair-rocket regexp sequences ;

MATCH-VARS: ?a ?b ;

: choose-term ( coeff i -- str )
    1 + { } 2sequence {
        {  0  _ } => [       ""                 ]
        {  1 ?a } => [ ?a    "e(%d)"    sprintf ]
        { -1 ?a } => [ ?a    "-e(%d)"   sprintf ]
        { ?a ?b } => [ ?a ?b "%d*e(%d)" sprintf ]
    } match-cond ;

: linear-combo ( seq -- str )
    [ choose-term ] map-index harvest " + " join
    R/ \+ -/ "- " re-replace [ "0" ] when-empty ;

{ { 1 2 3 } { 0 1 2 3 } { 1 0 3 4 } { 1 2 0 } { 0 0 0 } { 0 }
  { 1 1 1 } { -1 -1 -1 } { -1 -2 0 -3 } { -1 } }
[ dup linear-combo "%-14u  ->  %s\n" printf ] each
```

```txt

{ 1 2 3 }       ->  e(1) + 2*e(2) + 3*e(3)
{ 0 1 2 3 }     ->  e(2) + 2*e(3) + 3*e(4)
{ 1 0 3 4 }     ->  e(1) + 3*e(3) + 4*e(4)
{ 1 2 0 }       ->  e(1) + 2*e(2)
{ 0 0 0 }       ->  0
{ 0 }           ->  0
{ 1 1 1 }       ->  e(1) + e(2) + e(3)
{ -1 -1 -1 }    ->  -e(1) - e(2) - e(3)
{ -1 -2 0 -3 }  ->  -e(1) - 2*e(2) - 3*e(4)
{ -1 }          ->  -e(1)

```



## Go

```go
package main

import (
    "fmt"
    "strings"
)

func linearCombo(c []int) string {
    var sb strings.Builder
    for i, n := range c {
        if n == 0 {
            continue
        }
        var op string
        switch {
        case n < 0 && sb.Len() == 0:
            op = "-"
        case n < 0:
            op = " - "
        case n > 0 && sb.Len() == 0:
            op = ""
        default:
            op = " + "
        }
        av := n
        if av < 0 {
            av = -av
        }
        coeff := fmt.Sprintf("%d*", av)
        if av == 1 {
            coeff = ""
        }
        sb.WriteString(fmt.Sprintf("%s%se(%d)", op, coeff, i+1))
    }
    if sb.Len() == 0 {
        return "0"
    } else {
        return sb.String()
    }
}

func main() {
    combos := [][]int{
        {1, 2, 3},
        {0, 1, 2, 3},
        {1, 0, 3, 4},
        {1, 2, 0},
        {0, 0, 0},
        {0},
        {1, 1, 1},
        {-1, -1, -1},
        {-1, -2, 0, -3},
        {-1},
    }
    for _, c := range combos {
        t := strings.Replace(fmt.Sprint(c), " ", ", ", -1)
        fmt.Printf("%-15s  ->  %s\n", t, linearCombo(c))
    }
}
```


```txt

[1, 2, 3]        ->  e(1) + 2*e(2) + 3*e(3)
[0, 1, 2, 3]     ->  e(2) + 2*e(3) + 3*e(4)
[1, 0, 3, 4]     ->  e(1) + 3*e(3) + 4*e(4)
[1, 2, 0]        ->  e(1) + 2*e(2)
[0, 0, 0]        ->  0
[0]              ->  0
[1, 1, 1]        ->  e(1) + e(2) + e(3)
[-1, -1, -1]     ->  -e(1) - e(2) - e(3)
[-1, -2, 0, -3]  ->  -e(1) - 2*e(2) - 3*e(4)
[-1]             ->  -e(1)

```



## J


Implementation:


```J
fourbanger=:3 :0
  e=. ('e(',')',~])@":&.> 1+i.#y
  firstpos=. 0< {.y-.0
  if. */0=y do. '0' else. firstpos}.;y gluedto e end.
)

gluedto=:4 :0 each
  pfx=. '+-' {~ x<0
  select. |x
    case. 0 do. ''
    case. 1 do. pfx,y
    case.   do. pfx,(":|x),'*',y
  end.
)
```


Example use:


```J
   fourbanger 1 2 3
e(1)+2*e(2)+3*e(3)
   fourbanger 0 1 2 3
e(2)+2*e(3)+3*e(4)
   fourbanger 1 0 3 4
e(1)+3*e(3)+4*e(4)
   fourbanger 0 0 0
0
   fourbanger 0
0
   fourbanger 1 1 1
e(1)+e(2)+e(3)
   fourbanger _1 _1 _1
-e(1)-e(2)-e(3)
   fourbanger _1 _2 0 _3
-e(1)-2*e(2)-3*e(4)
   fourbanger _1
-e(1)
```



## Java

```Java
import java.util.Arrays;

public class LinearCombination {
    private static String linearCombo(int[] c) {
        StringBuilder sb = new StringBuilder();
        for (int i = 0; i < c.length; ++i) {
            if (c[i] == 0) continue;
            String op;
            if (c[i] < 0 && sb.length() == 0) {
                op = "-";
            } else if (c[i] < 0) {
                op = " - ";
            } else if (c[i] > 0 && sb.length() == 0) {
                op = "";
            } else {
                op = " + ";
            }
            int av = Math.abs(c[i]);
            String coeff = av == 1 ? "" : "" + av + "*";
            sb.append(op).append(coeff).append("e(").append(i + 1).append(')');
        }
        if (sb.length() == 0) {
            return "0";
        }
        return sb.toString();
    }

    public static void main(String[] args) {
        int[][] combos = new int[][]{
            new int[]{1, 2, 3},
            new int[]{0, 1, 2, 3},
            new int[]{1, 0, 3, 4},
            new int[]{1, 2, 0},
            new int[]{0, 0, 0},
            new int[]{0},
            new int[]{1, 1, 1},
            new int[]{-1, -1, -1},
            new int[]{-1, -2, 0, -3},
            new int[]{-1},
        };
        for (int[] c : combos) {
            System.out.printf("%-15s  ->  %s\n", Arrays.toString(c), linearCombo(c));
        }
    }
}
```

```txt
[1, 2, 3]        ->  e(1) + 2*e(2) + 3*e(3)
[0, 1, 2, 3]     ->  e(2) + 2*e(3) + 3*e(4)
[1, 0, 3, 4]     ->  e(1) + 3*e(3) + 4*e(4)
[1, 2, 0]        ->  e(1) + 2*e(2)
[0, 0, 0]        ->  0
[0]              ->  0
[1, 1, 1]        ->  e(1) + e(2) + e(3)
[-1, -1, -1]     ->  -e(1) - e(2) - e(3)
[-1, -2, 0, -3]  ->  -e(1) - 2*e(2) - 3*e(4)
[-1]             ->  -e(1)
```



## Julia


```julia
# v0.6

linearcombination(coef::Array) = join(collect("$c * e($i)" for (i, c) in enumerate(coef) if c != 0), " + ")

for c in [[1, 2, 3], [0, 1, 2, 3], [1, 0, 3, 4], [1, 2, 0], [0, 0, 0], [0], [1, 1, 1],
    [-1, -1, -1], [-1, -2, 0, -3], [-1]]
    @printf("%20s -> %s\n", c, linearcombination(c))
end
```


```txt
           [1, 2, 3] -> 1 * e(1) + 2 * e(2) + 3 * e(3)
        [0, 1, 2, 3] -> 1 * e(2) + 2 * e(3) + 3 * e(4)
        [1, 0, 3, 4] -> 1 * e(1) + 3 * e(3) + 4 * e(4)
           [1, 2, 0] -> 1 * e(1) + 2 * e(2)
           [0, 0, 0] ->
                 [0] ->
           [1, 1, 1] -> 1 * e(1) + 1 * e(2) + 1 * e(3)
        [-1, -1, -1] -> -1 * e(1) + -1 * e(2) + -1 * e(3)
     [-1, -2, 0, -3] -> -1 * e(1) + -2 * e(2) + -3 * e(4)
                [-1] -> -1 * e(1)
```



## Kotlin


```scala
// version 1.1.2

fun linearCombo(c: IntArray): String {
    val sb = StringBuilder()
    for ((i, n) in c.withIndex()) {
        if (n == 0) continue
        val op = when {
            n < 0 && sb.isEmpty() -> "-"
            n < 0                 -> " - "
            n > 0 && sb.isEmpty() -> ""
            else                  -> " + "
        }
        val av = Math.abs(n)
        val coeff = if (av == 1) "" else "$av*"
        sb.append("$op${coeff}e(${i + 1})")
    }
    return if(sb.isEmpty()) "0" else sb.toString()
}

fun main(args: Array<String>) {
    val combos = arrayOf(
        intArrayOf(1, 2, 3),
        intArrayOf(0, 1, 2, 3),
        intArrayOf(1, 0, 3, 4),
        intArrayOf(1, 2, 0),
        intArrayOf(0, 0, 0),
        intArrayOf(0),
        intArrayOf(1, 1, 1),
        intArrayOf(-1, -1, -1),
        intArrayOf(-1, -2, 0, -3),
        intArrayOf(-1)
    )
    for (c in combos) {
        println("${c.contentToString().padEnd(15)}  ->  ${linearCombo(c)}")
    }
}
```


```txt

[1, 2, 3]        ->  e(1) + 2*e(2) + 3*e(3)
[0, 1, 2, 3]     ->  e(2) + 2*e(3) + 3*e(4)
[1, 0, 3, 4]     ->  e(1) + 3*e(3) + 4*e(4)
[1, 2, 0]        ->  e(1) + 2*e(2)
[0, 0, 0]        ->  0
[0]              ->  0
[1, 1, 1]        ->  e(1) + e(2) + e(3)
[-1, -1, -1]     ->  -e(1) - e(2) - e(3)
[-1, -2, 0, -3]  ->  -e(1) - 2*e(2) - 3*e(4)
[-1]             ->  -e(1)

```


=={{header|Modula-2}}==

```modula2
MODULE Linear;
FROM FormatString IMPORT FormatString;
FROM Terminal IMPORT WriteString,WriteLn,ReadChar;

PROCEDURE WriteInt(n : INTEGER);
VAR buf : ARRAY[0..15] OF CHAR;
BEGIN
    FormatString("%i", buf, n);
    WriteString(buf)
END WriteInt;

PROCEDURE WriteLinear(c : ARRAY OF INTEGER);
VAR
    buf : ARRAY[0..15] OF CHAR;
    i,j : CARDINAL;
    b : BOOLEAN;
BEGIN
    b := TRUE;
    j := 0;

    FOR i:=0 TO HIGH(c) DO
        IF c[i]=0 THEN CONTINUE END;

        IF c[i]<0 THEN
            IF b THEN WriteString("-")
            ELSE      WriteString(" - ") END;
        ELSIF c[i]>0 THEN
            IF NOT b THEN WriteString(" + ") END;
        END;

        IF c[i] > 1 THEN
            WriteInt(c[i]);
            WriteString("*")
        ELSIF c[i] < -1 THEN
            WriteInt(-c[i]);
            WriteString("*")
        END;

        FormatString("e(%i)", buf, i+1);
        WriteString(buf);

        b := FALSE;
        INC(j)
    END;

    IF j=0 THEN WriteString("0") END;
    WriteLn
END WriteLinear;

TYPE
    Array1 = ARRAY[0..0] OF INTEGER;
    Array3 = ARRAY[0..2] OF INTEGER;
    Array4 = ARRAY[0..3] OF INTEGER;
BEGIN
    WriteLinear(Array3{1,2,3});
    WriteLinear(Array4{0,1,2,3});
    WriteLinear(Array4{1,0,3,4});
    WriteLinear(Array3{1,2,0});
    WriteLinear(Array3{0,0,0});
    WriteLinear(Array1{0});
    WriteLinear(Array3{1,1,1});
    WriteLinear(Array3{-1,-1,-1});
    WriteLinear(Array4{-1,-2,0,-3});
    WriteLinear(Array1{-1});

    ReadChar
END Linear.
```



## Perl


```perl
sub linear_combination {
    my(@coef) = @$_;
    my $e;
    for my $c (1..+@coef) { $e .= "$coef[$c-1]*e($c) + " if $coef[$c-1] }
    $e =~ s/ \+ $//;
    $e =~ s/1\*//g;
    $e =~ s/\+ -/- /g;
    $e // 0;
}

print linear_combination($_), "\n" for
  [1, 2, 3], [0, 1, 2, 3], [1, 0, 3, 4], [1, 2, 0], [0, 0, 0], [0], [1, 1, 1], [-1, -1, -1], [-1, -2, 0, -3], [-1 ]
```

```txt
e(1) + 2*e(2) + 3*e(3)
e(2) + 2*e(3) + 3*e(4)
e(1) + 3*e(3) + 4*e(4)
e(1) + 2*e(2)
0
0
e(1) + e(2) + e(3)
-e(1) - e(2) - e(3)
-e(1) - 2*e(2) - 3*e(4)
-e(1)
```



## Perl 6


```perl6
sub linear-combination(@coeff) {
    (@coeff Z=> map { "e($_)" }, 1 .. *)
    .grep(+*.key)
    .map({ .key ~ '*' ~ .value })
    .join(' + ')
    .subst('+ -', '- ', :g)
    .subst(/<|w>1\*/, '', :g)
        || '0'
}

say linear-combination($_) for
[1, 2, 3],
[0, 1, 2, 3],
[1, 0, 3, 4],
[1, 2, 0],
[0, 0, 0],
[0],
[1, 1, 1],
[-1, -1, -1],
[-1, -2, 0, -3],
[-1 ]
;
```

```txt
e(1) + 2*e(2) + 3*e(3)
e(2) + 2*e(3) + 3*e(4)
e(1) + 3*e(3) + 4*e(4)
e(1) + 2*e(2)
0
0
e(1) + e(2) + e(3)
-e(1) - e(2) - e(3)
-e(1) - 2*e(2) - 3*e(4)
-e(1)
```



## Phix

```Phix
function linear_combination(sequence f)
    string res = ""
    for e=1 to length(f) do
        integer fe = f[e]
        if fe!=0 then
            if fe=1 then
                if length(res) then res &= "+" end if
            elsif fe=-1 then
                res &= "-"
            elsif fe>0 and length(res) then
                res &= sprintf("+%d*",fe)
            else
                res &= sprintf("%d*",fe)
            end if
            res &= sprintf("e(%d)",e)
        end if
    end for
    if res="" then res = "0" end if
    return res
end function

constant tests = {{1,2,3},
                  {0,1,2,3},
                  {1,0,3,4},
                  {1,2,0},
                  {0,0,0},
                  {0},
                  {1,1,1},
                  {-1,-1,-1},
                  {-1,-2,0,-3},
                  {-1}}
for i=1 to length(tests) do
    sequence ti = tests[i]
    printf(1,"%12s -> %s\n",{sprint(ti), linear_combination(ti)})
end for
```

```txt

     {1,2,3} -> e(1)+2*e(2)+3*e(3)
   {0,1,2,3} -> e(2)+2*e(3)+3*e(4)
   {1,0,3,4} -> e(1)+3*e(3)+4*e(4)
     {1,2,0} -> e(1)+2*e(2)
     {0,0,0} -> 0
         {0} -> 0
     {1,1,1} -> e(1)+e(2)+e(3)
  {-1,-1,-1} -> -e(1)-e(2)-e(3)
{-1,-2,0,-3} -> -e(1)-2*e(2)-3*e(4)
        {-1} -> -e(1)

```



## Python


```python

def linear(x):
    return ' + '.join(['{}e({})'.format('-' if v == -1 else '' if v == 1 else str(v) + '*', i + 1)
        for i, v in enumerate(x) if v] or ['0']).replace(' + -', ' - ')

list(map(lambda x: print(linear(x)), [[1, 2, 3], [0, 1, 2, 3], [1, 0, 3, 4], [1, 2, 0],
        [0, 0, 0], [0], [1, 1, 1], [-1, -1, -1], [-1, -2, 0, 3], [-1]]))

```

```txt

e(1) + 2*e(2) + 3*e(3)
e(2) + 2*e(3) + 3*e(4)
e(1) + 3*e(3) + 4*e(4)
e(1) + 2*e(2)
0
0
e(1) + e(2) + e(3)
-e(1) - e(2) - e(3)
-e(1) - 2*e(2) + 3*e(4)
-e(1)

```



## Racket


```racket
#lang racket/base
(require racket/match racket/string)

(define (linear-combination->string es)
  (let inr ((es es) (i 1) (rv ""))
    (match* (es rv)
      [((list) "") "0"]
      [((list) rv) rv]
      [((list (? zero?) t ...) rv)
       (inr t (add1 i) rv)]
      [((list n t ...) rv)
       (define ±n
         (match* (n rv)
           ;; zero is handled above
           [(1 "") ""]
           [(1 _) "+"]
           [(-1 _) "-"]
           [((? positive? n) (not "")) (format "+~a*" n)]
           [(n _) (format "~a*" n)]))
       (inr t (add1 i) (string-append rv ±n "e("(number->string i)")"))])))

(for-each
 (compose displayln linear-combination->string)
 '((1 2 3)
   (0 1 2 3)
   (1 0 3 4)
   (1 2 0)
   (0 0 0)
   (0)
   (1 1 1)
   (-1 -1 -1)
   (-1 -2 0 -3)
   (-1)))

```


```txt
e(1)+2*e(2)+3*e(3)
e(2)+2*e(3)+3*e(4)
e(1)+3*e(3)+4*e(4)
e(1)+2*e(2)
0
0
e(1)+e(2)+e(3)
-e(1)-e(2)-e(3)
-e(1)-2*e(2)-3*e(4)
-e(1)
```



## REXX


```rexx
/*REXX program displays a  finite liner combination  in an  infinite vector basis.      */
@.=.;                             @.1  =   '  1,  2,  3     '
                                  @.2  =   '  0,  1,  2,  3 '
                                  @.3  =   '  1,  0,  3,  4 '
                                  @.4  =   '  1,  2,  0     '
                                  @.5  =   '  0,  0,  0     '
                                  @.6  =      0
                                  @.7  =   '  1,  1,  1     '
                                  @.8  =   ' -1, -1, -1     '
                                  @.9  =   ' -1, -2,  0, -3 '
                                  @.10 =     -1
  do j=1  while  @.j\==.;             n= 0       /*process each vector; zero element cnt*/
  y= space( translate(@.j, ,',') )               /*elide commas and superfluous blanks. */
  $=                                             /*nullify  output  (liner combination).*/
      do k=1  for words(y);      #= word(y, k)   /* ◄───── process each of the elements.*/
      if #=0  then iterate;      a= abs(# / 1)   /*if the value is zero, then ignore it.*/
      s= '+ ' ;    if #<0   then s= "- "         /*define the sign:  plus(+) or minus(-)*/
      n= n + 1;    if n==1  then s= strip(s)     /*if the 1st element used, remove plus.*/
      if a\==1              then s= s || a'*'    /*if multiplier is unity, then ignore #*/
      $= $  s'e('k")"                            /*construct a liner combination element*/
      end   /*k*/

  $= strip( strip($), 'L', "+")                  /*strip leading plus sign (1st element)*/
  if $==''  then $= 0                            /*handle special case of no elements.  */
  say right( space(@.j), 20)  ' ──► '   strip($) /*align the output for presentation.   */
  end       /*j*/                                /*stick a fork in it,  we're all done. */
```

```txt

             1, 2, 3  ──►  e(1) + 2*e(2) + 3*e(3)
          0, 1, 2, 3  ──►  e(2) + 2*e(3) + 3*e(4)
          1, 0, 3, 4  ──►  e(1) + 3*e(3) + 4*e(4)
             1, 2, 0  ──►  e(1) + 2*e(2)
             0, 0, 0  ──►  0
                   0  ──►  0
             1, 1, 1  ──►  e(1) + e(2) + e(3)
          -1, -1, -1  ──►  -e(1) - e(2) - e(3)
       -1, -2, 0, -3  ──►  -e(1) - 2*e(2) - 3*e(4)
                  -1  ──►  -e(1)

```



## Ring


```ring

# Project : Display a linear combination

scalars = [[1,  2,  3], [0,  1,  2,  3], [1,  0,  3,  4], [1,  2,  0], [0,  0,  0], [0], [1,  1,  1], [-1, -1, -1], [-1, -2,  0, -3], [-1]]
for n=1 to len(scalars)
    str = ""
    for m=1 to len(scalars[n])
        scalar = scalars[n] [m]
        if scalar != "0"
           if scalar = 1
              str = str + "+e" + m
           elseif  scalar = -1
              str = str + "" + "-e" + m
           else
              if scalar > 0
                 str = str + char(43) + scalar + "*e" + m
              else
                 str = str + "" + scalar + "*e" + m
              ok
           ok
        ok
    next
    if str = ""
       str = "0"
    ok
    if left(str, 1) = "+"
       str = right(str, len(str)-1)
    ok
    see str + nl
next

```

Output:

```txt

e1+2*e2+3*e3
e2+2*e3+3*e4
e1+3*e3+4*e4
e1+2*e2
0
0
e1+e2+e3
-e1-e2-e3
-e1-2*e2-3*e4
-e1

```



## Scala


```Scala
object LinearCombination extends App {
    val combos = Seq(Seq(1, 2, 3), Seq(0, 1, 2, 3),
      Seq(1, 0, 3, 4), Seq(1, 2, 0), Seq(0, 0, 0), Seq(0),
      Seq(1, 1, 1), Seq(-1, -1, -1), Seq(-1, -2, 0, -3), Seq(-1))

  private def linearCombo(c: Seq[Int]): String = {
    val sb = new StringBuilder
    for {i <- c.indices
         term = c(i)
         if term != 0} {
      val av = math.abs(term)
      def op = if (term < 0 && sb.isEmpty) "-"
      else if (term < 0) " - "
      else if (term > 0 && sb.isEmpty) "" else " + "

      sb.append(op).append(if (av == 1) "" else s"$av*").append("e(").append(i + 1).append(')')
    }
    if (sb.isEmpty) "0" else sb.toString
  }
    for (c <- combos) {
      println(f"${c.mkString("[", ", ", "]")}%-15s  ->  ${linearCombo(c)}%s")
    }
}
```


## Sidef

```ruby
func linear_combination(coeffs) {
    var res = ""
    for e,f in (coeffs.kv) {
        given(f) {
            when (1) {
                res += "+e(#{e+1})"
            }
            when (-1) {
                res += "-e(#{e+1})"
            }
            case (.> 0) {
                res += "+#{f}*e(#{e+1})"
            }
            case (.< 0) {
                res += "#{f}*e(#{e+1})"
            }
        }
    }
    res -= /^\+/
    res || 0
}

var tests = [
    %n{1 2 3},
    %n{0 1 2 3},
    %n{1 0 3 4},
    %n{1 2 0},
    %n{0 0 0},
    %n{0},
    %n{1 1 1},
    %n{-1 -1 -1},
    %n{-1 -2 0 -3},
    %n{-1},
]

tests.each { |t|
    printf("%10s -> %-10s\n", t.join(' '), linear_combination(t))
}
```

```txt
     1 2 3 -> e(1)+2*e(2)+3*e(3)
   0 1 2 3 -> e(2)+2*e(3)+3*e(4)
   1 0 3 4 -> e(1)+3*e(3)+4*e(4)
     1 2 0 -> e(1)+2*e(2)
     0 0 0 -> 0
         0 -> 0
     1 1 1 -> e(1)+e(2)+e(3)
  -1 -1 -1 -> -e(1)-e(2)-e(3)
-1 -2 0 -3 -> -e(1)-2*e(2)-3*e(4)
        -1 -> -e(1)
```



## Tcl

This solution strives for legibility rather than golf.


```Tcl
proc lincom {factors} {
    set exp 0
    set res ""
    foreach f $factors {
        incr exp
        if {$f == 0} {
            continue
        } elseif {$f == 1} {
            append res "+e($exp)"
        } elseif {$f == -1} {
            append res "-e($exp)"
        } elseif {$f > 0} {
            append res "+$f*e($exp)"
        } else {
            append res "$f*e($exp)"
        }
    }
    if {$res eq ""} {set res 0}
    regsub {^\+} $res {} res
    return $res
}

foreach test {
    {1 2 3}
    {0 1 2 3}
    {1 0 3 4}
    {1 2 0}
    {0 0 0}
    {0}
    {1 1 1}
    {-1 -1 -1}
    {-1 -2 0 -3}
    {-1}
} {
    puts [format "%10s -> %-10s" $test [lincom $test]]
}
```


```txt
     1 2 3 -> e(1)+2*e(2)+3*e(3)
   0 1 2 3 -> e(2)+2*e(3)+3*e(4)
   1 0 3 4 -> e(1)+3*e(3)+4*e(4)
     1 2 0 -> e(1)+2*e(2)
     0 0 0 -> 0
         0 -> 0
     1 1 1 -> e(1)+e(2)+e(3)
  -1 -1 -1 -> -e(1)-e(2)-e(3)
-1 -2 0 -3 -> -e(1)-2*e(2)-3*e(4)
        -1 -> -e(1)
```



## Visual Basic .NET

```vbnet
Imports System.Text

Module Module1

    Function LinearCombo(c As List(Of Integer)) As String
        Dim sb As New StringBuilder
        For i = 0 To c.Count - 1
            Dim n = c(i)
            If n < 0 Then
                If sb.Length = 0 Then
                    sb.Append("-")
                Else
                    sb.Append(" - ")
                End If
            ElseIf n > 0 Then
                If sb.Length <> 0 Then
                    sb.Append(" + ")
                End If
            Else
                Continue For
            End If

            Dim av = Math.Abs(n)
            If av <> 1 Then
                sb.AppendFormat("{0}*", av)
            End If
            sb.AppendFormat("e({0})", i + 1)
        Next
        If sb.Length = 0 Then
            sb.Append("0")
        End If
        Return sb.ToString()
    End Function

    Sub Main()
        Dim combos = New List(Of List(Of Integer)) From {
            New List(Of Integer) From {1, 2, 3},
            New List(Of Integer) From {0, 1, 2, 3},
            New List(Of Integer) From {1, 0, 3, 4},
            New List(Of Integer) From {1, 2, 0},
            New List(Of Integer) From {0, 0, 0},
            New List(Of Integer) From {0},
            New List(Of Integer) From {1, 1, 1},
            New List(Of Integer) From {-1, -1, -1},
            New List(Of Integer) From {-1, -2, 0, -3},
            New List(Of Integer) From {-1}
        }

        For Each c In combos
            Dim arr = "[" + String.Join(", ", c) + "]"
            Console.WriteLine("{0,15} -> {1}", arr, LinearCombo(c))
        Next
    End Sub

End Module
```

```txt
      [1, 2, 3] -> e(1) + 2*e(2) + 3*e(3)
   [0, 1, 2, 3] -> e(2) + 2*e(3) + 3*e(4)
   [1, 0, 3, 4] -> e(1) + 3*e(3) + 4*e(4)
      [1, 2, 0] -> e(1) + 2*e(2)
      [0, 0, 0] -> 0
            [0] -> 0
      [1, 1, 1] -> e(1) + e(2) + e(3)
   [-1, -1, -1] -> -e(1) - e(2) - e(3)
[-1, -2, 0, -3] -> -e(1) - 2*e(2) - 3*e(4)
           [-1] -> -e(1)
```



## zkl

```zkl
fcn linearCombination(coeffs){
   [1..].zipWith(fcn(n,c){ if(c==0) "" else "%s*e(%s)".fmt(c,n) },coeffs)
      .filter().concat("+").replace("+-","-").replace("1*","")
   or 0
}
```


```zkl
T(T(1,2,3),T(0,1,2,3),T(1,0,3,4),T(1,2,0),T(0,0,0),T(0),T(1,1,1),T(-1,-1,-1),
  T(-1,-2,0,-3),T(-1),T)
.pump(Console.println,linearCombination);
```

```txt

e(1)+2*e(2)+3*e(3)
e(2)+2*e(3)+3*e(4)
e(1)+3*e(3)+4*e(4)
e(1)+2*e(2)
0
0
e(1)+e(2)+e(3)
-e(1)-e(2)-e(3)
-e(1)-2*e(2)-3*e(4)
-e(1)
0

```

