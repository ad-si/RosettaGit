+++
title = "Apply a digital filter (direct form II transposed)"
description = ""
date = 2019-09-10T10:25:18Z
aliases = []
[extra]
id = 21230
[taxonomies]
categories = []
tags = []
+++

[[Category:Digital signal processing]]
{{task}}
Digital filters are used to apply a mathematical operation to a sampled signal. One of the common formulations is the "direct form II transposed" which can represent both infinite impulse response (IIR) and finite impulse response (FIR) filters, as well as being more numerically stable than other forms. <ref>[https://ccrma.stanford.edu/~jos/fp/Transposed_Direct_Forms.html]</ref>

;Task:

Filter a signal using an order 3 low-pass Butterworth filter. The coefficients for the filter are a=[1.00000000, -2.77555756e-16, 3.33333333e-01, -1.85037171e-17] and b = [0.16666667, 0.5, 0.5, 0.16666667]

The signal that needs filtering is the following vector: [-0.917843918645, 0.141984778794, 1.20536903482, 0.190286794412, -0.662370894973, -1.00700480494, -0.404707073677 ,0.800482325044, 0.743500089861, 1.01090520172, 0.741527555207, 0.277841675195, 0.400833448236, -0.2085993586, -0.172842103641, -0.134316096293, 0.0259303398477, 0.490105989562, 0.549391221511, 0.9047198589]


## C

Given the number of values a coefficient or signal vector can have and the number of digits, this implementation reads data from a file and prints it to the console if no output file is specified or writes to the specified output file. Usage printed on incorrect invocation.

```c
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

#define MAX_LEN 1000

typedef struct{
	float* values;
	int size;
}vector;

vector extractVector(char* str){
	vector coeff;
	int i=0,count = 1;
	char* token;

	while(str[i]!=00){
		if(str[i++]==' ')
			count++;
	}

	coeff.values = (float*)malloc(count*sizeof(float));
	coeff.size = count;

	token = strtok(str," ");

	i = 0;

	while(token!=NULL){
		coeff.values[i++] = atof(token);
		token = strtok(NULL," ");
	}

	return coeff;
}

vector processSignalFile(char* fileName){
	int i,j;
	float sum;
	char str[MAX_LEN];
	vector coeff1,coeff2,signal,filteredSignal;

	FILE* fp = fopen(fileName,"r");

	fgets(str,MAX_LEN,fp);
	coeff1 = extractVector(str);

	fgets(str,MAX_LEN,fp);
	coeff2 = extractVector(str);

	fgets(str,MAX_LEN,fp);
	signal = extractVector(str);

        fclose(fp);

	filteredSignal.values = (float*)calloc(signal.size,sizeof(float));
	filteredSignal.size = signal.size;

	for(i=0;i<signal.size;i++){
		sum = 0;

		for(j=0;j<coeff2.size;j++){
			if(i-j>=0)
				sum += coeff2.values[j]*signal.values[i-j];
		}

		for(j=0;j<coeff1.size;j++){
			if(i-j>=0)
				sum -= coeff1.values[j]*filteredSignal.values[i-j];
		}

		sum /= coeff1.values[0];
		filteredSignal.values[i] = sum;
	}

	return filteredSignal;
}

void printVector(vector v, char* outputFile){
	int i;

	if(outputFile==NULL){
		printf("[");
		for(i=0;i<v.size;i++)
			printf("%.12f, ",v.values[i]);
		printf("\b\b]");
	}

	else{
		FILE* fp = fopen(outputFile,"w");
		for(i=0;i<v.size-1;i++)
			fprintf(fp,"%.12f, ",v.values[i]);
		fprintf(fp,"%.12f",v.values[i]);
		fclose(fp);
	}

}

int main(int argC,char* argV[])
{
	char *str;
	if(argC<2||argC>3)
		printf("Usage : %s <name of signal data file and optional output file.>",argV[0]);
	else{
		if(argC!=2){
			str = (char*)malloc((strlen(argV[2]) + strlen(str) + 1)*sizeof(char));
			strcpy(str,"written to ");
		}
		printf("Filtered signal %s",(argC==2)?"is:\n":strcat(str,argV[2]));
		printVector(processSignalFile(argV[1]),argV[2]);
	}
	return 0;
}

```

Input file, 3 lines containing first ( a ) and second ( b ) coefficient followed by the signal, all values should be separated by a single space:

```txt

1.00000000 -2.77555756e-16 3.33333333e-01 -1.85037171e-17
0.16666667 0.5 0.5 0.16666667
-0.917843918645 0.141984778794 1.20536903482 0.190286794412 -0.662370894973 -1.00700480494 -0.404707073677 0.800482325044 0.743500089861 1.01090520172 0.741527555207 0.277841675195 0.400833448236 -0.2085993586 -0.172842103641 -0.134316096293 0.0259303398477 0.490105989562 0.549391221511 0.9047198589

```

Invocation and output for writing to file :

```txt

C:\rosettaCode>filterSignal.exe signalData.txt signalOut1.txt
Filtered signal written to signalOut1.txt

```

Output file :

```txt

-0.152973994613, -0.435257852077, -0.136043429375, 0.697503268719, 0.656444668770, -0.435482472181, -1.089239478111, -0.537676513195, 0.517050027847, 1.052249789238, 0.961854279041, 0.695690035820, 0.424356281757, 0.196262255311, -0.027835110202, -0.211721926928, -0.174745559692, 0.069258414209, 0.385445863008, 0.651770770550

```



## C++


This uses the C++11 method of initializing vectors. In g++, use the -std=c++0x compiler switch.


```cpp
#include <vector>
#include <iostream>
using namespace std;

void Filter(const vector<float> &b, const vector<float> &a, const vector<float> &in, vector<float> &out)
{

	out.resize(0);
	out.resize(in.size());

	for(int i=0; i < in.size(); i++)
	{
		float tmp = 0.;
		int j=0;
		out[i] = 0.f;
		for(j=0; j < b.size(); j++)
		{
			if(i - j < 0) continue;
			tmp += b[j] * in[i-j];
		}

		for(j=1; j < a.size(); j++)
		{
			if(i - j < 0) continue;
			tmp -= a[j]*out[i-j];
		}

		tmp /= a[0];
		out[i] = tmp;
	}
}

int main()
{
	vector<float> sig = {-0.917843918645,0.141984778794,1.20536903482,0.190286794412,-0.662370894973,-1.00700480494,\
		-0.404707073677,0.800482325044,0.743500089861,1.01090520172,0.741527555207,\
		0.277841675195,0.400833448236,-0.2085993586,-0.172842103641,-0.134316096293,\
		0.0259303398477,0.490105989562,0.549391221511,0.9047198589};

	//Constants for a Butterworth filter (order 3, low pass)
	vector<float> a = {1.00000000, -2.77555756e-16, 3.33333333e-01, -1.85037171e-17};
	vector<float> b = {0.16666667, 0.5, 0.5, 0.16666667};

	vector<float> result;
	Filter(b, a, sig, result);

	for(size_t i=0;i<result.size();i++)
		cout << result[i] << ",";
	cout << endl;

	return 0;
}
```


{{out}}

```txt
-0.152974,-0.435258,-0.136043,0.697503,0.656445,-0.435483,-1.08924,-0.537677,0.51705,1.05225,0.961854,0.69569,0.424356,0.196262,-0.0278351,-0.211722,-0.174746,0.0692584,0.385446,0.651771,
```


## C#
{{trans|Java}}

```c#
using System;

namespace ApplyDigitalFilter {
    class Program {
        private static double[] Filter(double[] a, double[] b, double[] signal) {
            double[] result = new double[signal.Length];
            for (int i = 0; i < signal.Length; ++i) {
                double tmp = 0.0;
                for (int j = 0; j < b.Length; ++j) {
                    if (i - j < 0) continue;
                    tmp += b[j] * signal[i - j];
                }
                for (int j = 1; j < a.Length; ++j) {
                    if (i - j < 0) continue;
                    tmp -= a[j] * result[i - j];
                }
                tmp /= a[0];
                result[i] = tmp;
            }
            return result;
        }

        static void Main(string[] args) {
            double[] a = new double[] { 1.00000000, -2.77555756e-16, 3.33333333e-01, -1.85037171e-17 };
            double[] b = new double[] { 0.16666667, 0.5, 0.5, 0.16666667 };

            double[] signal = new double[] {
                -0.917843918645, 0.141984778794, 1.20536903482, 0.190286794412,
                -0.662370894973, -1.00700480494, -0.404707073677, 0.800482325044,
                0.743500089861, 1.01090520172, 0.741527555207, 0.277841675195,
                0.400833448236, -0.2085993586, -0.172842103641, -0.134316096293,
                0.0259303398477, 0.490105989562, 0.549391221511, 0.9047198589
            };

            double[] result = Filter(a, b, signal);
            for (int i = 0; i < result.Length; ++i) {
                Console.Write("{0,11:F8}", result[i]);
                Console.Write((i + 1) % 5 != 0 ? ", " : "\n");
            }
        }
    }
}
```

{{out}}

```txt
-0.15297399, -0.43525783, -0.13604340,  0.69750333,  0.65644469
-0.43548245, -1.08923946, -0.53767655,  0.51704999,  1.05224975
 0.96185430,  0.69569009,  0.42435630,  0.19626223, -0.02783512
-0.21172192, -0.17474556,  0.06925841,  0.38544587,  0.65177084
```


## Common Lisp

{{trans|zkl}}

```lisp
(defparameter a #(1.00000000L0 -2.77555756L-16 3.33333333L-01 -1.85037171L-17))
(defparameter b #(0.16666667L0 0.50000000L0 0.50000000L0 0.16666667L0))
(defparameter s #(-0.917843918645 0.141984778794  1.20536903482   0.190286794412 -0.662370894973
                  -1.00700480494 -0.404707073677  0.800482325044  0.743500089861  1.01090520172
                   0.741527555207 0.277841675195  0.400833448236 -0.2085993586   -0.172842103641
                  -0.134316096293 0.0259303398477 0.490105989562  0.549391221511  0.9047198589))

(loop with out = (make-array (length s) :initial-element 0.0D0)
  for i below (length s)
  do (setf (svref out i)
           (/ (- (loop for j below (length b)
                   when (>= i j) sum (* (svref b j) (svref s (- i j))))
                 (loop for j below (length a)
                   when (>= i j) sum (* (svref a j) (svref out (- i j)))))
              (svref a 0)))
  (format t "~%~16,8F" (svref out i)))
```

{{out}}

```txt
     -0.15297399
     -0.43525784
     -0.13604341
      0.69750331
      0.65644468
     -0.43548247
     -1.08923949
     -0.53767657
      0.51705000
      1.05224976
      0.96185428
      0.69569007
      0.42435630
      0.19626225
     -0.02783512
     -0.21172192
     -0.17474557
      0.06925841
      0.38544587
      0.65177083

```



## D

{{trans|Kotlin}}

```D
import std.stdio;

alias T = real;
alias AT = T[];

AT filter(const AT a, const AT b, const AT signal) {
    AT result = new T[signal.length];

    foreach (int i; 0..signal.length) {
        T tmp = 0.0;
        foreach (int j; 0..b.length) {
            if (i-j<0) continue;
            tmp += b[j] * signal[i-j];
        }
        foreach (int j; 1..a.length) {
            if (i-j<0) continue;
            tmp -= a[j] * result[i-j];
        }
        tmp /= a[0];
        result[i] = tmp;
    }

    return result;
}

void main() {
    AT a = [1.00000000, -2.77555756e-16, 3.33333333e-01, -1.85037171e-17];
    AT b = [0.16666667, 0.5, 0.5, 0.16666667];

    AT signal = [
        -0.917843918645, 0.141984778794, 1.20536903482, 0.190286794412,
        -0.662370894973, -1.00700480494, -0.404707073677, 0.800482325044,
        0.743500089861, 1.01090520172, 0.741527555207, 0.277841675195,
        0.400833448236, -0.2085993586, -0.172842103641, -0.134316096293,
        0.0259303398477, 0.490105989562, 0.549391221511, 0.9047198589
    ];

    AT result = filter(a,b,signal);
    foreach (i; 0..result.length) {
        writef("% .8f", result[i]);
        if ((i+1)%5 != 0) {
            write(", ");
        } else {
            writeln;
        }
    }
}
```


{{out}}

```txt
-0.15297399, -0.43525783, -0.13604340,  0.69750333,  0.65644469
-0.43548245, -1.08923946, -0.53767655,  0.51704999,  1.05224975
 0.96185430,  0.69569009,  0.42435630,  0.19626223, -0.02783512
-0.21172192, -0.17474556,  0.06925840,  0.38544587,  0.65177084
```



## Go


```go
package main

import "fmt"

type filter struct {
    b, a []float64
}

func (f filter) filter(in []float64) []float64 {
    out := make([]float64, len(in))
    s := 1. / f.a[0]
    for i := range in {
        tmp := 0.
        b := f.b
        if i+1 < len(b) {
            b = b[:i+1]
        }
        for j, bj := range b {
            tmp += bj * in[i-j]
        }
        a := f.a[1:]
        if i < len(a) {
            a = a[:i]
        }
        for j, aj := range a {
            tmp -= aj * out[i-j-1]
        }
        out[i] = tmp * s
    }
    return out
}

//Constants for a Butterworth filter (order 3, low pass)
var bwf = filter{
    a: []float64{1.00000000, -2.77555756e-16, 3.33333333e-01, -1.85037171e-17},
    b: []float64{0.16666667, 0.5, 0.5, 0.16666667},
}

var sig = []float64{
    -0.917843918645, 0.141984778794, 1.20536903482, 0.190286794412,
    -0.662370894973, -1.00700480494, -0.404707073677, 0.800482325044,
    0.743500089861, 1.01090520172, 0.741527555207, 0.277841675195,
    0.400833448236, -0.2085993586, -0.172842103641, -0.134316096293,
    0.0259303398477, 0.490105989562, 0.549391221511, 0.9047198589,
}

func main() {
    for _, v := range bwf.filter(sig) {
        fmt.Printf("%9.6f\n", v)
    }
}
```

{{out}}

```txt

-0.152974
-0.435258
-0.136043
 0.697503
 0.656445
-0.435482
-1.089239
-0.537677
 0.517050
 1.052250
 0.961854
 0.695690
 0.424356
 0.196262
-0.027835
-0.211722
-0.174746
 0.069258
 0.385446
 0.651771

```



## Java

{{trans|Kotlin}}

```Java
public class DigitalFilter {
    private static double[] filter(double[] a, double[] b, double[] signal) {
        double[] result = new double[signal.length];
        for (int i = 0; i < signal.length; ++i) {
            double tmp = 0.0;
            for (int j = 0; j < b.length; ++j) {
                if (i - j < 0) continue;
                tmp += b[j] * signal[i - j];
            }
            for (int j = 1; j < a.length; ++j) {
                if (i - j < 0) continue;
                tmp -= a[j] * result[i - j];
            }
            tmp /= a[0];
            result[i] = tmp;
        }
        return result;
    }

    public static void main(String[] args) {
        double[] a = new double[]{1.00000000, -2.77555756e-16, 3.33333333e-01, -1.85037171e-17};
        double[] b = new double[]{0.16666667, 0.5, 0.5, 0.16666667};

        double[] signal = new double[]{
            -0.917843918645, 0.141984778794, 1.20536903482, 0.190286794412,
            -0.662370894973, -1.00700480494, -0.404707073677, 0.800482325044,
            0.743500089861, 1.01090520172, 0.741527555207, 0.277841675195,
            0.400833448236, -0.2085993586, -0.172842103641, -0.134316096293,
            0.0259303398477, 0.490105989562, 0.549391221511, 0.9047198589
        };

        double[] result = filter(a, b, signal);
        for (int i = 0; i < result.length; ++i) {
            System.out.printf("% .8f", result[i]);
            System.out.print((i + 1) % 5 != 0 ? ", " : "\n");
        }
    }
}
```

{{out}}

```txt
-0.15297399, -0.43525783, -0.13604340,  0.69750333,  0.65644469
-0.43548245, -1.08923946, -0.53767655,  0.51704999,  1.05224975
 0.96185430,  0.69569009,  0.42435630,  0.19626223, -0.02783512
-0.21172192, -0.17474556,  0.06925841,  0.38544587,  0.65177084
```



## Julia

{{trans|zkl}}

```julia
function DF2TFilter(a::Vector, b::Vector, sig::Vector)
    rst = zeros(sig)
    for i in eachindex(sig)
        tmp =  sum(b[j] * sig[i-j+1] for j in 1:min(i, length(b)))
        tmp -= sum(a[j] * rst[i-j+1] for j in 1:min(i, length(a)))
        rst[i] = tmp / a[1]
    end
    return rst
end

acoef = [1.00000000, -2.77555756e-16, 3.33333333e-01, -1.85037171e-17]
bcoef = [0.16666667, 0.5, 0.5, 0.16666667]
signal = [-0.917843918645,  0.141984778794, 1.20536903482, 0.190286794412,
          -0.662370894973, -1.00700480494, -0.404707073677,  0.800482325044,
           0.743500089861,  1.01090520172,  0.741527555207,  0.277841675195,
           0.400833448236, -0.2085993586,  -0.172842103641, -0.134316096293,
           0.0259303398477, 0.490105989562, 0.549391221511,  0.9047198589]
@show DF2TFilter(acoef, bcoef, signal)
```

{{output}}
```txt
DF2TFilter(acoef, bcoef, signal) = [-0.152974, -0.435258, -0.136043, 0.697503, 0.656445, -0.435482, -1.08924, -0.537677, 0.51705, 1.05225, 0.961854, 0.69569, 0.424356, 0.196262, -0.0278351, -0.211722, -0.174746, 0.0692584, 0.385446, 0.651771]
```



## Kotlin

{{trans|C++}}

```scala
// version 1.1.3

fun filter(a: DoubleArray, b: DoubleArray, signal: DoubleArray): DoubleArray {
    val result = DoubleArray(signal.size)
    for (i in 0 until signal.size) {
        var tmp = 0.0
        for (j in 0 until b.size) {
            if (i - j < 0) continue
            tmp += b[j] * signal[i - j]
        }
        for (j in 1 until a.size) {
            if (i - j < 0) continue
            tmp -= a[j] * result[i - j]
        }
        tmp /= a[0]
        result[i] = tmp
    }
    return result
}

fun main(args: Array<String>) {
    val a = doubleArrayOf(1.00000000, -2.77555756e-16, 3.33333333e-01, -1.85037171e-17)
    val b = doubleArrayOf(0.16666667, 0.5, 0.5, 0.16666667)

    val signal = doubleArrayOf(
        -0.917843918645, 0.141984778794, 1.20536903482, 0.190286794412,
        -0.662370894973, -1.00700480494, -0.404707073677, 0.800482325044,
        0.743500089861, 1.01090520172, 0.741527555207, 0.277841675195,
        0.400833448236, -0.2085993586, -0.172842103641, -0.134316096293,
        0.0259303398477, 0.490105989562, 0.549391221511, 0.9047198589
    )

    val result = filter(a, b, signal)
    for (i in 0 until result.size) {
        print("% .8f".format(result[i]))
        print(if ((i + 1) % 5 != 0) ", " else "\n")
    }
}
```


{{out}}

```txt

-0.15297399, -0.43525783, -0.13604340,  0.69750333,  0.65644469
-0.43548245, -1.08923946, -0.53767655,  0.51704999,  1.05224975
 0.96185430,  0.69569009,  0.42435630,  0.19626223, -0.02783512
-0.21172192, -0.17474556,  0.06925841,  0.38544587,  0.65177084

```



## MATLAB

MATLAB is commonly used for filter design and implementation. To implement this filter, and display the original signal and the filtered result:

```MATLAB

signal = [-0.917843918645, 0.141984778794, 1.20536903482, 0.190286794412, -0.662370894973, -1.00700480494, -0.404707073677 ,0.800482325044, 0.743500089861, 1.01090520172, 0.741527555207, 0.277841675195, 0.400833448236, -0.2085993586, -0.172842103641, -0.134316096293, 0.0259303398477, 0.490105989562, 0.549391221511, 0.9047198589];
a = [1.00000000, -2.77555756e-16, 3.33333333e-01, -1.85037171e-17];
b = [0.16666667, 0.5, 0.5, 0.16666667];

out = filter(b,a,signal)

figure
subplot(1,2,1)
stem(0:19, signal)
xlabel('n')
title('Original Signal')

subplot(1,2,2)
stem(0:19, out)
xlabel('n')
title('Filtered Signal')

```


{{out}}

```txt

out =

  Columns 1 through 10

   -0.1530   -0.4353   -0.1360    0.6975    0.6564   -0.4355   -1.0892   -0.5377    0.5170    1.0522

  Columns 11 through 20

    0.9619    0.6957    0.4244    0.1963   -0.0278   -0.2117   -0.1747    0.0693    0.3854    0.6518

```



## Objeck

{{trans|Java}}

```objeck
class DigitalFilter {
  function : Main(args : String[]) ~ Nil {
    a := [1.00000000, -2.77555756e-16, 3.33333333e-01, -1.85037171e-17];
    b := [0.16666667, 0.5, 0.5, 0.16666667];
    signal := [-0.917843918645, 0.141984778794, 1.20536903482, 0.190286794412,
      -0.662370894973, -1.00700480494, -0.404707073677, 0.800482325044,
      0.743500089861, 1.01090520172, 0.741527555207, 0.277841675195,
      0.400833448236, -0.2085993586, -0.172842103641, -0.134316096293,
      0.0259303398477, 0.490105989562, 0.549391221511, 0.9047198589];

    result := Filter(a, b, signal);
    each(i : result) {
          System.IO.Console->Print(result[i])->Print(((i + 1) % 5 <> 0) ? ",\t" : "\n");
        };
  }

  function : Filter(a : Float[], b : Float[], signal : Float[]) ~ Float[] {
    result := Float->New[signal->Size()];

    each(i : signal) {
      tmp := 0.0;

      each(j : b) {
          if(i-j >= 0) {
            tmp += b[j] * signal[i - j];
          };
      };

      each(j : a) {
        if(i-j >= 0) {
              tmp -= a[j] * result[i - j];
          };
      };

      tmp /= a[0];
      result[i] := tmp;
    };

    return result;
  }
}
```


{{output}}

```txt

-0.152974,      -0.435258,      -0.136043,      0.697503,       0.656445
-0.435482,      -1.08924,       -0.537677,      0.51705,        1.05225
0.961854,       0.69569,        0.424356,       0.196262,       -0.0278351
-0.211722,      -0.174746,      0.0692584,      0.385446,       0.651771

```



## ooRexx


```oorexx
/* REXX */
a=.array~of(1.00000000, -2.77555756e-16, 3.33333333e-01, -1.85037171e-17)
b=.array~of(0.16666667, 0.5, 0.5, 0.16666667)
s=.array~of(-0.917843918645, 0.141984778794, 1.20536903482, 0.190286794412,,
            -0.662370894973, -1.00700480494, -0.404707073677 ,0.800482325044,,
             0.743500089861, 1.01090520172, 0.741527555207, 0.277841675195,,
             0.400833448236, -0.2085993586, -0.172842103641, -0.134316096293,,
             0.0259303398477, 0.490105989562, 0.549391221511, 0.9047198589)

ret=.array~new(s~items)~~fill(0) /* create array and fill with zeroes */

Call filter a,b,s,ret
Do i=1 To ret~items
  Say format(i,2) format(ret[i],2,12)
  End
Exit

::Routine filter
Use Arg a,b,s,ret
Do i=1 To s~items
  temp=0
  Do j=1 To b~items
    if i-j>=0 Then
      temp=temp+b[j]*s[i-j+1]
    End
  Do j=1 To a~items
    if i-j>=0 Then Do
      u=i-j+1
      temp=temp-a[j]*ret[u]
      End
    End
  ret[i]=temp/a[1]
  End
Return

::OPTIONS digits 24      /* Numeric Digits 24, everywhere */

```

{{out|output}}

```txt
 1 -0.152973989500
 2 -0.435257829050
 3 -0.136043396988
 4  0.697503326548
 5  0.656444692469
 6 -0.435482453256
 7 -1.089239461153
 8 -0.537676549563
 9  0.517049992313
10  1.052249747155
11  0.961854300374
12  0.695690094010
13  0.424356295096
14  0.196262231822
15 -0.027835124463
16 -0.211721915450
17 -0.174745562223
18  0.069258408901
19  0.385445874307
20  0.651770838819
```



## Perl

{{trans|Perl 6}}

```perl
use strict;
use List::AllUtils 'natatime';

sub TDF_II_filter {
    our(@signal,@a,@b);
    local(*signal,*a,*b) = (shift, shift, shift);
    my @out = (0) x $#signal;
    for my $i (0..@signal-1) {
        my $this;
        map { $this += $b[$_] * $signal[$i-$_] if $i-$_ >= 0 } 0..@b;
        map { $this -= $a[$_] *    $out[$i-$_] if $i-$_ >= 0 } 0..@a;
        $out[$i] = $this / $a[0];
    }
    @out
}

my @signal = (
    -0.917843918645,  0.141984778794, 1.20536903482,   0.190286794412,
    -0.662370894973, -1.00700480494, -0.404707073677,  0.800482325044,
     0.743500089861,  1.01090520172,  0.741527555207,  0.277841675195,
     0.400833448236, -0.2085993586,  -0.172842103641, -0.134316096293,
     0.0259303398477, 0.490105989562, 0.549391221511,  0.9047198589
);
my @a = ( 1.00000000, -2.77555756e-16, 3.33333333e-01, -1.85037171e-17 );
my @b = ( 0.16666667,  0.5,            0.5,             0.16666667     );

my @filtered = TDF_II_filter(\@signal, \@a, \@b);
my $iter = natatime 5, @filtered;
while( my @values = $iter->() ) {
    printf(' %10.6f' x 5 . "\n", @values);
}

```

{{out}}

```txt
  -0.152974  -0.435258  -0.136043   0.697503   0.656445
  -0.435482  -1.089239  -0.537677   0.517050   1.052250
   0.961854   0.695690   0.424356   0.196262  -0.027835
  -0.211722  -0.174746   0.069258   0.385446   0.651771
```



## Perl 6

{{works with|Rakudo|2016.11}}
{{trans|zkl}}


```perl6
sub TDF-II-filter ( @signal, @a, @b ) {
    my @out = 0 xx @signal;
    for ^@signal -> $i {
        my $this;
        $this += @b[$_] * @signal[$i-$_] if $i-$_ >= 0 for ^@b;
        $this -= @a[$_] *    @out[$i-$_] if $i-$_ >= 0 for ^@a;
        @out[$i] = $this / @a[0];
    }
    @out
}

my @signal = [
    -0.917843918645,  0.141984778794, 1.20536903482,   0.190286794412,
    -0.662370894973, -1.00700480494, -0.404707073677,  0.800482325044,
     0.743500089861,  1.01090520172,  0.741527555207,  0.277841675195,
     0.400833448236, -0.2085993586,  -0.172842103641, -0.134316096293,
     0.0259303398477, 0.490105989562, 0.549391221511,  0.9047198589
];
my @a = [ 1.00000000, -2.77555756e-16, 3.33333333e-01, -1.85037171e-17 ];
my @b = [ 0.16666667,  0.5,            0.5,             0.16666667     ];

say TDF-II-filter(@signal, @a, @b)».fmt("% 0.8f")
    Z~ flat (', ' xx 4, ",\n") xx *;
```

{{out}}

```txt
(-0.15297399,  -0.43525783,  -0.13604340,   0.69750333,   0.65644469,
 -0.43548245,  -1.08923946,  -0.53767655,   0.51704999,   1.05224975,
  0.96185430,   0.69569009,   0.42435630,   0.19626223,  -0.02783512,
 -0.21172192,  -0.17474556,   0.06925841,   0.38544587,   0.65177084,
)
```



## Phix

{{trans|Julia}}
Note however that the a[j]* starts from index 2, unlike Julia/C/Perl6/Rust/Sidef/zkl,
but the same as C++/C#/D/Java/Kotlin - and it does not seem to make any difference...

```Phix
function direct_form_II_transposed_filter(sequence a, b, signal)
    sequence result = repeat(0,length(signal))
    for i=1 to length(in) do
        atom tmp = 0
        for j=1 to min(i,length(b)) do tmp += b[j]*signal[i-j+1] end for
        for j=2 to min(i,length(a)) do tmp -= a[j]*result[i-j+1] end for
        result[i] = tmp/a[1]
    end for
    return result
end function

constant acoef = {1.00000000, -2.77555756e-16, 3.33333333e-01, -1.85037171e-17},
         bcoef = {0.16666667, 0.5, 0.5, 0.16666667}
         signal = {-0.917843918645,0.141984778794,1.20536903482,0.190286794412,-0.662370894973,
                   -1.00700480494,-0.404707073677,0.800482325044,0.743500089861,1.01090520172,
                    0.741527555207,0.277841675195,0.400833448236,-0.2085993586,-0.172842103641,
                   -0.134316096293,0.0259303398477,0.490105989562,0.549391221511,0.9047198589},

pp(direct_form_II_transposed_filter(acoef, bcoef, signal),{pp_FltFmt,"%9.6f",pp_Maxlen,110})
```

{{out}}

```txt

{-0.152974,-0.435258,-0.136043, 0.697503, 0.656445,-0.435482,-1.089239,-0.537677, 0.517050, 1.052250,
  0.961854, 0.695690, 0.424356, 0.196262,-0.027835,-0.211722,-0.174746, 0.069258, 0.385446, 0.651771}

```



## Python



```python
#!/bin/python
from __future__ import print_function
from scipy import signal
import matplotlib.pyplot as plt

if __name__=="__main__":
	sig = [-0.917843918645,0.141984778794,1.20536903482,0.190286794412,-0.662370894973,-1.00700480494,
		-0.404707073677,0.800482325044,0.743500089861,1.01090520172,0.741527555207,
		0.277841675195,0.400833448236,-0.2085993586,-0.172842103641,-0.134316096293,
		0.0259303398477,0.490105989562,0.549391221511,0.9047198589]

	#Create an order 3 lowpass butterworth filter
	#Generated using b, a = signal.butter(3, 0.5)
	a = [1.00000000, -2.77555756e-16, 3.33333333e-01, -1.85037171e-17]
	b = [0.16666667, 0.5, 0.5, 0.16666667]

	#Apply the filter to signal
	filt = signal.lfilter(b, a, sig)
	print (filt)

	plt.plot(sig, 'b')
	plt.plot(filt, 'r--')
	plt.show()
```


{{out}}

```txt
[-0.15297399 -0.43525783 -0.1360434   0.69750333  0.65644469 -0.43548245
 -1.08923946 -0.53767655  0.51704999  1.05224975  0.9618543   0.69569009
  0.4243563   0.19626223 -0.02783512 -0.21172192 -0.17474556  0.06925841
  0.38544587  0.65177084]
```



## REXX


### version 1

{{trans|Julia}}

```REXX
/*REXX pgm filters a signal with a order3 lowpass Butterworth, direct form II transposed*/
numeric digits 24                                                  /*use 20 decimal digs*/
@a= '1           -2.77555756e-16  3.33333333e-1  -1.85037171e-17'  /*filter coefficients*/
@b=  0.16666667   0.5             0.5             0.16666667       /*  "          "     */
@s= '-0.917843918645  0.141984778794   1.20536903482    0.190286794412  -0.662370894973' ,
    '-1.00700480494  -0.404707073677   0.800482325044   0.743500089861   1.01090520172 ' ,
    ' 0.741527555207  0.277841675195   0.400833448236  -0.2085993586    -0.172842103641' ,
    '-0.134316096293  0.0259303398477  0.490105989562   0.549391221511   0.9047198589  '
$.=0;            N= words(@s);     w= length(n)                    /* [↑]  signal vector*/
     do i=1  for N;                #=0           /*process each of the vector elements. */
       do j=1  for words(@b); if i-j >= 0  then #= # + word(@b, j) * word(@s, i-j+1);  end
       do k=1  for words(@a); _= i -k +1;  if i-k >= 0  then #= # - word(@a, k) * $._; end
     $.i= # / word(@a ,1);         call tell
     end   /*i*/                                 /* [↑]  only show using ½ the dec. digs*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
tell: numeric digits digits()%2;  say right(i, w)   " "   left('', $.i>=0)$.i /1;   return
```

{{out|output}}

```txt

 1   -0.1529739895
 2   -0.43525782905
 3   -0.136043396988
 4    0.697503326548
 5    0.656444692469
 6   -0.435482453256
 7   -1.08923946115
 8   -0.537676549563
 9    0.517049992313
10    1.05224974716
11    0.961854300374
12    0.69569009401
13    0.424356295096
14    0.196262231822
15   -0.0278351244634
16   -0.21172191545
17   -0.174745562223
18    0.0692584089012
19    0.385445874307
20    0.651770838819

```



### version 2

{{trans|Julia}}

```REXX
/* REXX */
Numeric Digits 24
acoef = '1.00000000, -2.77555756e-16, 3.33333333e-01, -1.85037171e-17'
bcoef = '0.16666667, 0.5, 0.5, 0.16666667'
signal = '-0.917843918645, 0.141984778794, 1.20536903482, 0.190286794412,',
         '-0.662370894973, -1.00700480494, -0.404707073677 ,0.800482325044,',
         ' 0.743500089861, 1.01090520172, 0.741527555207, 0.277841675195,',
         ' 0.400833448236, -0.2085993586, -0.172842103641, -0.134316096293,',
         ' 0.0259303398477, 0.490105989562, 0.549391221511, 0.9047198589'

Do i=1 By 1 While acoef>'';  Parse Var acoef a.i . ',' acoef;   End; a.0=i-1
Do i=1 By 1 While bcoef>'';  Parse Var bcoef b.i . ',' bcoef;   End; b.0=i-1
Do i=1 By 1 While signal>''; Parse Var signal s.i . ',' signal; End; s.0=i-1

ret.=0
Do i=1 To s.0
  temp=0.0
  Do j=1 To b.0
    if i-j>=0 Then Do
      u=i-j+1
      temp=temp+b.j*s.u
      End
    End
  Do j=1 To a.0
    if i-j>=0 Then Do
      u=i-j+1
      temp=temp-a.j*ret.u
      End
    End
  ret.i=temp/a.1
  Say format(i,2) format(ret.i,2,12)
  End
```

{{out|output}}

```txt
 1 -0.152973989500
 2 -0.435257829050
 3 -0.136043396988
 4  0.697503326548
 5  0.656444692469
 6 -0.435482453256
 7 -1.089239461153
 8 -0.537676549563
 9  0.517049992313
10  1.052249747155
11  0.961854300374
12  0.695690094010
13  0.424356295096
14  0.196262231822
15 -0.027835124463
16 -0.211721915450
17 -0.174745562223
18  0.069258408901
19  0.385445874307
20  0.651770838819

```



## Rust

{{trans|Java}}

```Rust
use std::cmp::Ordering;

struct IIRFilter<'f>(&'f [f32], &'f [f32]);

impl<'f> IIRFilter<'f> {
    pub fn with_coefficients(a: &'f [f32], b: &'f [f32]) -> IIRFilter<'f> {
        IIRFilter(a, b)
    }

    // Performs the calculation as an iterator chain.
    pub fn apply<I: Iterator<Item = &'f f32> + 'f>(
        &self,
        samples: I,
    ) -> impl Iterator<Item = f32> + 'f {
        // Name some things for readability
        let a_coeff = self.0;
        let b_coeff = self.1;

        let mut prev_results = Vec::<f32>::new();
        let mut prev_samples = Vec::<f32>::new();

        // The actual calculation, done one number at a time
        samples.enumerate() // (i, sample[i])
            .map(move |(i, sample)| { // for each sample, apply this function
                prev_samples.push(*sample);
                prev_results.push(0f32); // the initial version of the previous result

                let sum_b: f32 = b_coeff.iter() // for each coefficient in b
                    .enumerate() // (j, b_coeff[j])
                    .map(|(j, c)| { // calculate the weight of the coefficient
                        if i >= j {
                            (*c) * prev_samples[i-j]
                        } else {
                            0f32
                        }
                    })
                    .sum(); // add them all together

                let sum_a: f32 = a_coeff.iter() // for each coefficient in a
                    .enumerate() // (j, a_coeff[j])
                    .map(|(j, c)| { // calculate the weight of the coefficient
                        if i >= j {
                            (*c) * prev_results[i-j]
                        } else {
                            0f32
                        }
                    })
                    .sum(); // add them all together

                // perform the final calculation
                let result = (sum_b - sum_a) / a_coeff[0];

                // update the previous result for the next iteration
                prev_results[i] = result;

                // return the current result in this iteration
                result
            }
        )
    }
}

fn main() {
    let a: &[f32] = &[1.00000000, -2.77555756e-16, 3.33333333e-01, -1.85037171e-17];
    let b: &[f32] = &[0.16666667, 0.5, 0.5, 0.16666667];

    let samples: Vec<f32> = vec![
        -0.917843918645,
        0.141984778794,
        1.20536903482,
        0.190286794412,
        -0.662370894973,
        -1.00700480494,
        -0.404707073677,
        0.800482325044,
        0.743500089861,
        1.01090520172,
        0.741527555207,
        0.277841675195,
        0.400833448236,
        -0.2085993586,
        -0.172842103641,
        -0.134316096293,
        0.0259303398477,
        0.490105989562,
        0.549391221511,
        0.9047198589,
    ];

    for (i, result) in IIRFilter::with_coefficients(a, b)
        .apply(samples.iter())
        .enumerate()
    {
        print!("{:.8}", result);
        if (i + 1) % 5 != 0 {
            print!(", ");
        } else {
            println!();
        }
    }
    println!();
}
```

{{out|output}}

```txt

-0.15297399, -0.43525785, -0.13604343, 0.69750333, 0.65644467
-0.43548250, -1.08923948, -0.53767651, 0.51705003, 1.05224979
0.96185434, 0.69568992, 0.42435625, 0.19626230, -0.02783510
-0.21172196, -0.17474557, 0.06925842, 0.38544586, 0.65177077

```



## Scala

{{Out}}See it yourself by running in your browser either by [https://scalafiddle.io/sf/0D4zyWF/0 ScalaFiddle (ES aka JavaScript, non JVM)] or [https://scastie.scala-lang.org/mUPbvXTQTXijp2DV1piQgQ Scastie (remote JVM)].
{{libheader|Scala Tail recursion}}
{{libheader|Scala Digital Signal Processing}}
{{libheader|ScalaFiddle qualified}}
{{libheader|Scastie qualified}}
{{works with|Scala|2.13}}

```Scala
object ButterworthFilter extends App {
  private def filter(a: Vector[Double],
                     b: Vector[Double],
                     signal: Vector[Double]): Vector[Double] = {

    @scala.annotation.tailrec
    def outer(i: Int, acc: Vector[Double]): Vector[Double] = {
      if (i >= signal.length) acc
      else {
        @scala.annotation.tailrec
        def inner0(j: Int, tmp: Double): Double = if (j >= b.length) tmp
        else if ((i - j) >= 0) inner0(j + 1, tmp + b(j) * signal(i - j)) else inner0(j + 1, tmp)

        @scala.annotation.tailrec
        def inner1(j: Int, tmp: Double): Double = if (j >= a.length) tmp
        else if (i - j >= 0) inner1(j + 1, tmp - a(j) * acc(i - j)) else inner1(j + 1, tmp)

        outer(i + 1, acc :+ inner1(1, inner0(0, 0D)) / a(0))
      }
    }

    outer(0, Vector())
  }

  filter(Vector[Double](1, -2.77555756e-16, 3.33333333e-01, -1.85037171e-17),
    Vector[Double](0.16666667, 0.5, 0.5, 0.16666667),
    Vector[Double](
      -0.917843918645, 0.141984778794, 1.20536903482, 0.190286794412, -0.662370894973,
      -1.00700480494, -0.404707073677, 0.800482325044, 0.743500089861, 1.01090520172,
      0.741527555207, 0.277841675195, 0.400833448236, -0.2085993586, -0.172842103641,
      -0.134316096293, 0.0259303398477, 0.490105989562, 0.549391221511, 0.9047198589)
  ).grouped(5)
    .map(_.map(x => f"$x% .8f"))
    .foreach(line => println(line.mkString(" ")))

}
```


## Sidef

{{trans|Perl 6}}

```ruby
func TDF_II_filter(signal, a, b) {
    var out = [0]*signal.len
    for i in ^signal {
        var this = 0
        for j in ^b { i-j >= 0 && (this += b[j]*signal[i-j]) }
        for j in ^a { i-j >= 0 && (this -= a[j]*   out[i-j]) }
        out[i] = this/a[0]
    }
    return out
}

var signal = [
    -0.917843918645,  0.141984778794, 1.20536903482,   0.190286794412,
    -0.662370894973, -1.00700480494, -0.404707073677,  0.800482325044,
     0.743500089861,  1.01090520172,  0.741527555207,  0.277841675195,
     0.400833448236, -0.2085993586,  -0.172842103641, -0.134316096293,
     0.0259303398477, 0.490105989562, 0.549391221511,  0.9047198589
]

var a = [1.00000000, -2.77555756e-16, 3.33333333e-01, -1.85037171e-17]
var b = [0.16666667,  0.5,            0.5,             0.16666667    ]
var f = TDF_II_filter(signal, a, b)

say "["
say f.map { "% 0.8f" % _ }.slices(5).map{.join(', ')}.join(",\n")
say "]"
```

{{out}}

```txt

[
-0.15297399, -0.43525783, -0.13604340,  0.69750333,  0.65644469,
-0.43548245, -1.08923946, -0.53767655,  0.51704999,  1.05224975,
 0.96185430,  0.69569009,  0.42435630,  0.19626223, -0.02783512,
-0.21172192, -0.17474556,  0.06925841,  0.38544587,  0.65177084
]
```



## Yabasic

{{trans|D}}

```Yabasic
sub filter(a(), b(), signal(), result())
    local i, j, tmp

    for i = 0 to arraysize(signal(), 1)
        tmp = 0
        for j = 0 to arraysize(b(), 1)
            if (i-j<0) continue
            tmp = tmp + b(j) * signal(i-j)
        next
        for j = 0 to arraysize(a(), 1)
            if (i-j<0) continue
            tmp = tmp - a(j) * result(i-j)
        next
        tmp = tmp / a(0)
        result(i) = tmp
    next
end sub

dim a(4), b(4), signal(20), result(20)

// a()
data 1, -2.77555756e-16, 3.33333333e-01, -1.85037171e-17
// b()
data 0.16666667, 0.5, 0.5, 0.16666667
// signal()
data -0.917843918645, 0.141984778794, 1.20536903482, 0.190286794412
data -0.662370894973, -1.00700480494, -0.404707073677, 0.800482325044
data 0.743500089861, 1.01090520172, 0.741527555207, 0.277841675195
data 0.400833448236, -0.2085993586, -0.172842103641, -0.134316096293
data 0.0259303398477, 0.490105989562, 0.549391221511, 0.9047198589

for i = 0 to 3 : read a(i) : next
for i = 0 to 3 : read b(i) : next
for i = 0 to 19 : read signal(i) : next

filter(a(),b(),signal(),result())

for i = 0 to 19
    print result(i) using "%11.8f";
    if mod(i+1, 5) <> 0 then
        print ", ";
    else
        print
    end if
next
```



## zkl

{{trans|C++}}

```zkl
fcn direct_form_II_transposed_filter(b,a,signal){
   out:=List.createLong(signal.len(),0.0);  // vector of zeros
   foreach i in (signal.len()){
      tmp:=0.0;
      foreach j in (b.len()){ if(i-j >=0) tmp += b[j]*signal[i-j] }
      foreach j in (a.len()){ if(i-j >=0) tmp -= a[j]*out[i-j]    }
      out[i] = tmp/a[0];
   }
   out
}
```


```zkl
signal:=T(-0.917843918645, 0.141984778794, 1.20536903482,  0.190286794412,
	  -0.662370894973,-1.00700480494, -0.404707073677, 0.800482325044,
	   0.743500089861, 1.01090520172,  0.741527555207, 0.277841675195,
	   0.400833448236,-0.2085993586,  -0.172842103641,-0.134316096293,
	   0.0259303398477,0.490105989562, 0.549391221511, 0.9047198589 );
a:=T(1.0, -2.77555756e-16, 3.33333333e-01, -1.85037171e-17 );
b:=T(0.16666667, 0.5, 0.5, 0.16666667 );
result:=direct_form_II_transposed_filter(b,a,signal);
println(result);
```

{{out}}

```txt

L(-0.152974,-0.435258,-0.136043,  0.697503, 0.656445,-0.435482,
  -1.08924, -0.537677, 0.51705,   1.05225,  0.961854, 0.69569,
   0.424356, 0.196262,-0.0278351,-0.211722,-0.174746, 0.0692584,
   0.385446, 0.651771)

```


'''References'''

<references/>
