+++
title = "Fast Fourier transform"
description = ""
date = 2019-10-09T20:47:20Z
aliases = []
[extra]
id = 9235
[taxonomies]
categories = ["task"]
tags = []
languages = [
  "ada",
  "algol_68",
  "apl",
  "bbc_basic",
  "c",
  "common_lisp",
  "cpp",
  "crystal",
  "csharp",
  "d",
  "echolisp",
  "erre",
  "factor",
  "fortran",
  "gap",
  "go",
  "haskell",
  "idris",
  "j",
  "java",
  "javascript",
  "jq",
  "julia",
  "kotlin",
  "lambdatalk",
  "liberty_basic",
  "lua",
  "maple",
  "maxima",
  "nim",
  "ocaml",
  "oorexx",
  "pari_gp",
  "perl",
  "perl_6",
  "phix",
  "php",
  "picolisp",
  "pl_i",
  "prolog",
  "python",
  "r",
  "racket",
  "rexx",
  "ruby",
  "run_basic",
  "rust",
  "scala",
  "scilab",
  "sequencel",
  "sidef",
  "stata",
  "systemverilog",
  "tcl",
  "ursala",
  "zkl",
]
+++

## Task

Calculate the   FFT   (<u>F</u>ast <u>F</u>ourier <u>T</u>ransform)   of an input sequence.

The most general case allows for complex numbers at the input
and results in a sequence of equal length, again of complex numbers.
If you need to restrict yourself to real numbers, the output should
be the magnitude (i.e. sqrt(re²+im²)) of the complex result.

The classic version is the recursive Cooley–Tukey FFT. [http://en.wikipedia.org/wiki/Cooley–Tukey_FFT_algorithm Wikipedia] has pseudo-code for that.
Further optimizations are possible but not required.





## Ada


The FFT function is defined as a generic function, instantiated upon
a user instance of Ada.Numerics.Generic_Complex_Arrays.


```Ada

with Ada.Numerics.Generic_Complex_Arrays;

generic
   with package Complex_Arrays is
      new Ada.Numerics.Generic_Complex_Arrays (<>);
   use Complex_Arrays;
function Generic_FFT (X : Complex_Vector) return Complex_Vector;

```



```Ada

with Ada.Numerics;
with Ada.Numerics.Generic_Complex_Elementary_Functions;

function Generic_FFT (X : Complex_Vector) return Complex_Vector is

   package Complex_Elementary_Functions is
      new Ada.Numerics.Generic_Complex_Elementary_Functions
        (Complex_Arrays.Complex_Types);

   use Ada.Numerics;
   use Complex_Elementary_Functions;
   use Complex_Arrays.Complex_Types;

   function FFT (X : Complex_Vector; N, S : Positive)
      return Complex_Vector is
   begin
      if N = 1 then
         return (1..1 => X (X'First));
      else
         declare
            F : constant Complex  := exp (Pi * j / Real_Arrays.Real (N/2));
            Even : Complex_Vector := FFT (X, N/2, 2*S);
            Odd  : Complex_Vector := FFT (X (X'First + S..X'Last), N/2, 2*S);
         begin
            for K in 0..N/2 - 1 loop
               declare
                  T : constant Complex := Odd (Odd'First + K) / F ** K;
               begin
                  Odd  (Odd'First  + K) := Even (Even'First + K) - T;
                  Even (Even'First + K) := Even (Even'First + K) + T;
               end;
            end loop;
            return Even & Odd;
         end;
      end if;
   end FFT;
begin
   return FFT (X, X'Length, 1);
end Generic_FFT;

```


Example:


```Ada

with Ada.Numerics.Complex_Arrays;  use Ada.Numerics.Complex_Arrays;
with Ada.Complex_Text_IO;          use Ada.Complex_Text_IO;
with Ada.Text_IO;                  use Ada.Text_IO;

with Ada.Numerics.Complex_Elementary_Functions;
with Generic_FFT;

procedure Example is
   function FFT is new Generic_FFT (Ada.Numerics.Complex_Arrays);
   X : Complex_Vector := (1..4 => (1.0, 0.0), 5..8 => (0.0, 0.0));
   Y : Complex_Vector := FFT (X);
begin
   Put_Line ("       X              FFT X ");
   for I in Y'Range loop
      Put (X (I - Y'First + X'First), Aft => 3, Exp => 0);
      Put (" ");
      Put (Y (I), Aft => 3, Exp => 0);
      New_Line;
   end loop;
end;

```


```txt

       X              FFT X
( 1.000, 0.000) ( 4.000, 0.000)
( 1.000, 0.000) ( 1.000,-2.414)
( 1.000, 0.000) ( 0.000, 0.000)
( 1.000, 0.000) ( 1.000,-0.414)
( 0.000, 0.000) ( 0.000, 0.000)
( 0.000, 0.000) ( 1.000, 0.414)
( 0.000, 0.000) ( 0.000, 0.000)
( 0.000, 0.000) ( 1.000, 2.414)

```



## ALGOL 68

{{trans|Python}}Note: This specimen retains the original [[#Python|Python]] coding style.
'''File: Template.Fast_Fourier_transform.a68'''
```algol68
PRIO DICE = 9; # ideally = 11 #

OP DICE = ([]SCALAR in, INT step)[]SCALAR: (
    ### Dice the array, extract array values a "step" apart ###
    IF step = 1 THEN
        in
    ELSE
        INT upb out := 0;
        [(UPB in-LWB in)%step+1]SCALAR out;
        FOR index FROM LWB in BY step TO UPB in DO
            out[upb out+:=1] := in[index] OD;
        out[@LWB in]
    FI
);

PROC fft = ([]SCALAR in t)[]SCALAR: (
    ### The Cooley-Tukey FFT algorithm ###
    IF LWB in t >= UPB in t THEN
      in t[@0]
    ELSE
        []SCALAR t = in t[@0];
        INT n = UPB t + 1, half n = n % 2;
        [LWB t:UPB t]SCALAR coef;

        []SCALAR even = fft(t    DICE 2),
                  odd = fft(t[1:]DICE 2);

        COMPL i = 0 I 1;

        REAL w =  2*pi / n;
        FOR k FROM LWB t TO half n-1 DO
            COMPL cis t = scalar exp(0 I (-w * k))*odd[k];
            coef[k]          := even[k] + cis t;
            coef[k + half n] := even[k] - cis t
        OD;
        coef
    FI
);
```
'''File: test.Fast_Fourier_transform.a68'''
```algol68
#!/usr/local/bin/a68g --script #
# -*- coding: utf-8 -*- #

MODE SCALAR = COMPL;
PROC (COMPL)COMPL scalar exp = complex exp;
PR READ "Template.Fast_Fourier_transform.a68" PR

FORMAT real fmt := $g(0,3)$;
FORMAT real array fmt := $f(real fmt)", "$;
FORMAT compl fmt := $f(real fmt)"⊥"f(real fmt)$;
FORMAT compl array fmt := $f(compl fmt)", "$;

test:(
  []COMPL
    tooth wave ft = fft((1, 1, 1, 1, 0, 0, 0, 0)),
    one and a quarter wave ft = fft((0, 0.924, 0.707,-0.383,-1,-0.383, 0.707, 0.924,
                                     0,-0.924,-0.707, 0.383, 1, 0.383,-0.707,-0.924));
  printf((
    $"Tooth wave: "$,compl array fmt, tooth wave ft, $l$,
    $"1¼ cycle wave: "$, compl array fmt, one and a quarter wave ft, $l$
  ))
)
```

```txt

Tooth wave: 4.000⊥.000, 1.000⊥-2.414, .000⊥.000, 1.000⊥-.414, .000⊥.000, 1.000⊥.414, .000⊥.000, 1.000⊥2.414,
1¼ cycle wave: .000⊥.000, .000⊥.001, .000⊥.000, .000⊥-8.001, .000⊥.000, -.000⊥-.001, .000⊥.000, .000⊥.001, .000⊥.000, .000⊥-.001, .000⊥.000, -.000⊥.001, .000⊥.000, -.000⊥8.001, .000⊥.000, -.000⊥-.001,

```



## APL

```APL

fft←{
    N←⍴⍵
    N≤1:⍵
    (1|2⍟N)≠0:'Argument must be a power of 2 in length'
    even←fft(N⍴0 1)/⍵
    odd←fft(N⍴1 0)/⍵
    k←N÷2
    T←even×*(0J¯2×(○1)×(¯1+⍳k)÷N)
    (odd+T),odd-T
}

```


'''Example:'''

```APL

      fft 1 1 1 1 0 0 0 0

```


```txt

4 1J¯2.414213562 0 1J¯0.4142135624 0 1J0.4142135624
      0 1J2.414213562

```



## BBC BASIC

```bbcbasic
      @% = &60A

      DIM Complex{r#, i#}
      DIM in{(7)} = Complex{}, out{(7)} = Complex{}
      DATA 1, 1, 1, 1, 0, 0, 0, 0

      PRINT "Input (real, imag):"
      FOR I% = 0 TO 7
        READ in{(I%)}.r#
        PRINT in{(I%)}.r# "," in{(I%)}.i#
      NEXT

      PROCfft(out{()}, in{()}, 0, 1, DIM(in{()},1)+1)

      PRINT "Output (real, imag):"
      FOR I% = 0 TO 7
        PRINT out{(I%)}.r# "," out{(I%)}.i#
      NEXT
      END

      DEF PROCfft(b{()}, o{()}, B%, S%, N%)
      LOCAL I%, t{} : DIM t{} = Complex{}
      IF S% < N% THEN
        PROCfft(o{()}, b{()}, B%, S%*2, N%)
        PROCfft(o{()}, b{()}, B%+S%, S%*2, N%)
        FOR I% = 0 TO N%-1 STEP 2*S%
          t.r# = COS(-PI*I%/N%)
          t.i# = SIN(-PI*I%/N%)
          PROCcmul(t{}, o{(B%+I%+S%)})
          b{(B%+I% DIV 2)}.r# = o{(B%+I%)}.r# + t.r#
          b{(B%+I% DIV 2)}.i# = o{(B%+I%)}.i# + t.i#
          b{(B%+(I%+N%) DIV 2)}.r# = o{(B%+I%)}.r# - t.r#
          b{(B%+(I%+N%) DIV 2)}.i# = o{(B%+I%)}.i# - t.i#
        NEXT
      ENDIF
      ENDPROC

      DEF PROCcmul(c{},d{})
      LOCAL r#, i#
      r# = c.r#*d.r# - c.i#*d.i#
      i# = c.r#*d.i# + c.i#*d.r#
      c.r# = r#
      c.i# = i#
      ENDPROC

```

```txt
Input (real, imag):
         1,         0
         1,         0
         1,         0
         1,         0
         0,         0
         0,         0
         0,         0
         0,         0
Output (real, imag):
         4,         0
         1,  -2.41421
         0,         0
         1, -0.414214
         0,         0
         1,  0.414214
         0,         0
         1,   2.41421
```



## C

Inplace FFT with O(n) memory usage.
Note: array size is assumed to be power of 2 and not checked by code;
you can just pad it with 0 otherwise.
Also, <code>complex</code> is C99 standard.
```C


#include <stdio.h>
#include <math.h>
#include <complex.h>

double PI;
typedef double complex cplx;

void _fft(cplx buf[], cplx out[], int n, int step)
{
	if (step < n) {
		_fft(out, buf, n, step * 2);
		_fft(out + step, buf + step, n, step * 2);

		for (int i = 0; i < n; i += 2 * step) {
			cplx t = cexp(-I * PI * i / n) * out[i + step];
			buf[i / 2]     = out[i] + t;
			buf[(i + n)/2] = out[i] - t;
		}
	}
}

void fft(cplx buf[], int n)
{
	cplx out[n];
	for (int i = 0; i < n; i++) out[i] = buf[i];

	_fft(buf, out, n, 1);
}


void show(const char * s, cplx buf[]) {
	printf("%s", s);
	for (int i = 0; i < 8; i++)
		if (!cimag(buf[i]))
			printf("%g ", creal(buf[i]));
		else
			printf("(%g, %g) ", creal(buf[i]), cimag(buf[i]));
}

int main()
{
	PI = atan2(1, 1) * 4;
	cplx buf[] = {1, 1, 1, 1, 0, 0, 0, 0};

	show("Data: ", buf);
	fft(buf, 8);
	show("\nFFT : ", buf);

	return 0;
}


```

```txt
Data: 1 1 1 1 0 0 0 0
FFT : 4 (1, -2.41421) 0 (1, -0.414214) 0 (1, 0.414214) 0 (1, 2.41421)
```



### OS X / iOS

OS X 10.7+ / iOS 4+

```c
#include <stdio.h>
#include <Accelerate/Accelerate.h>

void fft(DSPComplex buf[], int n) {
  float inputMemory[2*n];
  float outputMemory[2*n];
  // half for real and half for complex
  DSPSplitComplex inputSplit = {inputMemory, inputMemory + n};
  DSPSplitComplex outputSplit = {outputMemory, outputMemory + n};

  vDSP_ctoz(buf, 2, &inputSplit, 1, n);

  vDSP_DFT_Setup setup = vDSP_DFT_zop_CreateSetup(NULL, n, vDSP_DFT_FORWARD);

  vDSP_DFT_Execute(setup,
                   inputSplit.realp, inputSplit.imagp,
                   outputSplit.realp, outputSplit.imagp);

  vDSP_ztoc(&outputSplit, 1, buf, 2, n);
}


void show(const char *s, DSPComplex buf[], int n) {
  printf("%s", s);
  for (int i = 0; i < n; i++)
    if (!buf[i].imag)
      printf("%g ", buf[i].real);
    else
      printf("(%g, %g) ", buf[i].real, buf[i].imag);
  printf("\n");
}

int main() {
  DSPComplex buf[] = {{1,0}, {1,0}, {1,0}, {1,0}, {0,0}, {0,0}, {0,0}, {0,0}};

  show("Data: ", buf, 8);
  fft(buf, 8);
  show("FFT : ", buf, 8);

  return 0;
}
```

```txt
Data: 1 1 1 1 0 0 0 0
FFT : 4 (1, -2.41421) 0 (1, -0.414214) 0 (1, 0.414214) 0 (1, 2.41421)
```



## C++


```cpp
#include <complex>
#include <iostream>
#include <valarray>

const double PI = 3.141592653589793238460;

typedef std::complex<double> Complex;
typedef std::valarray<Complex> CArray;

// Cooley–Tukey FFT (in-place, divide-and-conquer)
// Higher memory requirements and redundancy although more intuitive
void fft(CArray& x)
{
    const size_t N = x.size();
    if (N <= 1) return;

    // divide
    CArray even = x[std::slice(0, N/2, 2)];
    CArray  odd = x[std::slice(1, N/2, 2)];

    // conquer
    fft(even);
    fft(odd);

    // combine
    for (size_t k = 0; k < N/2; ++k)
    {
        Complex t = std::polar(1.0, -2 * PI * k / N) * odd[k];
        x[k    ] = even[k] + t;
        x[k+N/2] = even[k] - t;
    }
}

// Cooley-Tukey FFT (in-place, breadth-first, decimation-in-frequency)
// Better optimized but less intuitive
// !!! Warning : in some cases this code make result different from not optimased version above (need to fix bug)
// The bug is now fixed @2017/05/30
void fft(CArray &x)
{
	// DFT
	unsigned int N = x.size(), k = N, n;
	double thetaT = 3.14159265358979323846264338328L / N;
	Complex phiT = Complex(cos(thetaT), -sin(thetaT)), T;
	while (k > 1)
	{
		n = k;
		k >>= 1;
		phiT = phiT * phiT;
		T = 1.0L;
		for (unsigned int l = 0; l < k; l++)
		{
			for (unsigned int a = l; a < N; a += n)
			{
				unsigned int b = a + k;
				Complex t = x[a] - x[b];
				x[a] += x[b];
				x[b] = t * T;
			}
			T *= phiT;
		}
	}
	// Decimate
	unsigned int m = (unsigned int)log2(N);
	for (unsigned int a = 0; a < N; a++)
	{
		unsigned int b = a;
		// Reverse bits
		b = (((b & 0xaaaaaaaa) >> 1) | ((b & 0x55555555) << 1));
		b = (((b & 0xcccccccc) >> 2) | ((b & 0x33333333) << 2));
		b = (((b & 0xf0f0f0f0) >> 4) | ((b & 0x0f0f0f0f) << 4));
		b = (((b & 0xff00ff00) >> 8) | ((b & 0x00ff00ff) << 8));
		b = ((b >> 16) | (b << 16)) >> (32 - m);
		if (b > a)
		{
			Complex t = x[a];
			x[a] = x[b];
			x[b] = t;
		}
	}
	//// Normalize (This section make it not working correctly)
	//Complex f = 1.0 / sqrt(N);
	//for (unsigned int i = 0; i < N; i++)
	//	x[i] *= f;
}

// inverse fft (in-place)
void ifft(CArray& x)
{
    // conjugate the complex numbers
    x = x.apply(std::conj);

    // forward fft
    fft( x );

    // conjugate the complex numbers again
    x = x.apply(std::conj);

    // scale the numbers
    x /= x.size();
}

int main()
{
    const Complex test[] = { 1.0, 1.0, 1.0, 1.0, 0.0, 0.0, 0.0, 0.0 };
    CArray data(test, 8);

    // forward fft
    fft(data);

    std::cout << "fft" << std::endl;
    for (int i = 0; i < 8; ++i)
    {
        std::cout << data[i] << std::endl;
    }

    // inverse fft
    ifft(data);

    std::cout << std::endl << "ifft" << std::endl;
    for (int i = 0; i < 8; ++i)
    {
        std::cout << data[i] << std::endl;
    }
    return 0;
}
```

```txt

fft
(4,0)
(1,-2.41421)
(0,0)
(1,-0.414214)
(0,0)
(1,0.414214)
(0,0)
(1,2.41421)

ifft
(1,-0)
(1,-5.55112e-017)
(1,2.4895e-017)
(1,-5.55112e-017)
(5.55112e-017,0)
(5.55112e-017,5.55112e-017)
(0,-2.4895e-017)
(5.55112e-017,5.55112e-017)

```


## C#

```c#
using System;
using System.Numerics;
using System.Linq;
using System.Diagnostics;

// Fast Fourier Transform in C#
public class Program {

    /* Performs a Bit Reversal Algorithm on a postive integer
     * for given number of bits
     * e.g. 011 with 3 bits is reversed to 110 */
    public static int BitReverse(int n, int bits) {
       int reversedN = n;
       int count = bits - 1;

       n >>= 1;
       while (n > 0) {
            reversedN = (reversedN << 1) | (n & 1);
            count--;
            n >>= 1;
        }

        return ((reversedN << count) & ((1 << bits) - 1));
    }

    /* Uses Cooley-Tukey iterative in-place algorithm with radix-2 DIT case
     * assumes no of points provided are a power of 2 */
    public static void FFT(Complex[] buffer) {
#if false
        int bits = (int)Math.Log(buffer.Length, 2);
        for (int j = 1; j < buffer.Length / 2; j++) {

            int swapPos = BitReverse(j, bits);
            var temp = buffer[j];
            buffer[j] = buffer[swapPos];
            buffer[swapPos] = temp;
        }
// Said Zandian
// The above section of the code is incorrect and does not work correctly and has two bugs.
// BUG 1
// The bug is that when you reach and index that was swapped previously it does swap it again
// Ex. binary value n = 0010 and Bits = 4 as input to BitReverse routine and  returns 4. The code section above //     swaps it. Cells 2 and 4 are swapped. just fine.
//     now binary value n = 0010 and Bits = 4 as input to BitReverse routine and returns 2. The code Section
//     swap it. Cells 4 and 2 are swapped.     WROOOOONG
//
// Bug 2
// The code works on the half section of the cells. In the case of Bits = 4 it means that we are having 16 cells
// The code works on half the cells        for (int j = 1; j < buffer.Length / 2; j++) buffer.Length returns 16
// and divide by 2 makes 8, so j goes from 1 to 7. This covers almost everything but what happened to 1011 value
// which must be swap with 1101. and this is the second bug.
//
// use the following corrected section of the code. I have seen this bug in other languages that uses bit
// reversal routine.

#else
            for (int j = 1; j < buffer.Length; j++)
            {
                int swapPos = BitReverse(j, bits);
                if (swapPos <= j)
                {
                    continue;
                }
                var temp = buffer[j];
                buffer[j] = buffer[swapPos];
                buffer[swapPos] = temp;
            }

// First the full length is used and 1011 value is swapped with 1101. Second if new swapPos is less than j
// then it means that swap was happen when j was the swapPos.

#endif

        for (int N = 2; N <= buffer.Length; N <<= 1) {
            for (int i = 0; i < buffer.Length; i += N) {
                for (int k = 0; k < N / 2; k++) {

                    int evenIndex = i + k;
                    int oddIndex = i + k + (N / 2);
                    var even = buffer[evenIndex];
                    var odd = buffer[oddIndex];

                    double term = -2 * Math.PI * k / (double)N;
                    Complex exp = new Complex(Math.Cos(term), Math.Sin(term)) * odd;

                    buffer[evenIndex] = even + exp;
                    buffer[oddIndex] = even - exp;

                }
            }
        }
    }

    public static void Main(string[] args) {
        Complex[] input = {1.0, 1.0, 1.0, 1.0, 0.0, 0.0, 0.0, 0.0};

        FFT(input);

        Console.WriteLine("Results:");
        foreach (Complex c in input) {
            Console.WriteLine(c);
        }
    }
}
```

```txt

Results:
(4, 0)
(1, -2.41421356237309)
(0, 0)
(1, -0.414213562373095)
(0, 0)
(1, 0.414213562373095)
(0, 0)
(1, 2.41421356237309)

```



## Common Lisp


```lisp

;;; This is adapted from the Python sample; it uses lists for simplicity.
;;; Production code would use complex arrays (for compiler optimization).
;;; This version exhibits LOOP features, closing with compositional golf.
(defun fft (x &aux (length (length x)))
  ;; base case: return the list as-is
  (if (<= length 1) x
    ;; collect alternating elements into separate lists...
    (loop for (a b) on x by #'cddr collect a into as collect b into bs finally
          ;; ... and take the FFT of both;
          (let* ((ffta (fft as)) (fftb (fft bs))
                 ;; incrementally phase shift each element of the 2nd list
                 (aux (loop for b in fftb and k from 0 by (/ pi length -1/2)
                            collect (* b (cis k)))))
            ;; finally, concatenate the sum and difference of the lists
            (return (mapcan #'mapcar '(+ -) `(,ffta ,ffta) `(,aux ,aux)))))))

```

```lisp

;;; Demonstrates printing an FFT in both rectangular and polar form:
CL-USER> (mapc (lambda (c) (format t "~&~6F~6@Fi = ~6Fe^~6@Fipi"
                                   (realpart c) (imagpart c) (abs c) (/ (phase c) pi)))
               (fft '(1 1 1 1 0 0 0 0)))

   4.0  +0.0i =    4.0e^  +0.0ipi
   1.0-2.414i = 2.6131e^-0.375ipi
   0.0  +0.0i =    0.0e^  +0.0ipi
   1.0-0.414i = 1.0824e^-0.125ipi
   0.0  +0.0i =    0.0e^  +0.0ipi
   1.0+0.414i = 1.0824e^+0.125ipi
   0.0  +0.0i =    0.0e^  +0.0ipi
   1.0+2.414i = 2.6131e^+0.375ipi
;;; MAPC also returns the FFT data, which looks like this:
(#C(4.0 0.0) #C(1.0D0 -2.414213562373095D0) #C(0.0D0 0.0D0)
 #C(1.0D0 -0.4142135623730949D0) #C(0.0 0.0)
 #C(0.9999999999999999D0 0.4142135623730949D0) #C(0.0D0 0.0D0)
 #C(0.9999999999999997D0 2.414213562373095D0))

```



## Crystal

```ruby
require "complex"

def fft(x : Array(Int32 | Float64)) #: Array(Complex)
  return [x[0].to_c] if x.size <= 1
  even = fft(Array.new(x.size / 2) { |k| x[2 * k] })
  odd  = fft(Array.new(x.size / 2) { |k| x[2 * k + 1] })
  c = Array.new(x.size / 2) { |k| (-2 * Math::PI * k / x.size).i.exp }
  codd = Array.new(x.size / 2) { |k| c[k] * odd[k] }
  return Array.new(x.size / 2) { |k| even[k] + codd[k] } + Array.new(x.size / 2) { |k| even[k] - codd[k] }
end

fft([1,1,1,1,0,0,0,0]).each{ |c| puts c }

```

```txt

4.0 + 0.0i
1.0 - 2.414213562373095i
0.0 + 0.0i
1.0 - 0.4142135623730949i
0.0 + 0.0i
0.9999999999999999 + 0.4142135623730949i
0.0 + 0.0i
0.9999999999999997 + 2.414213562373095i

```



## D


### Standard Version


```d
void main() {
    import std.stdio, std.numeric;

    [1.0, 1, 1, 1, 0, 0, 0, 0].fft.writeln;
}
```

```txt
[4+0i, 1-2.41421i, 0-0i, 1-0.414214i, 0+0i, 1+0.414214i, 0+0i, 1+2.41421i]
```



### creals Version

Built-in complex numbers will be deprecated.

```d
import std.stdio, std.algorithm, std.range, std.math;

const(creal)[] fft(in creal[] x) pure /*nothrow*/ @safe {
    immutable N = x.length;
    if (N <= 1) return x;
    const ev = x.stride(2).array.fft;
    const od = x[1 .. $].stride(2).array.fft;
    auto l = iota(N / 2).map!(k => ev[k] + expi(-2*PI * k/N) * od[k]);
    auto r = iota(N / 2).map!(k => ev[k] - expi(-2*PI * k/N) * od[k]);
    return l.chain(r).array;
}

void main() @safe {
    [1.0L+0i, 1, 1, 1, 0, 0, 0, 0].fft.writeln;
}
```

```txt
[4+0i, 1+-2.41421i, 0+0i, 1+-0.414214i, 0+0i, 1+0.414214i, 0+0i, 1+2.41421i]
```



### Phobos Complex Version


```d
import std.stdio, std.algorithm, std.range, std.math, std.complex;

auto fft(T)(in T[] x) pure /*nothrow @safe*/ {
    immutable N = x.length;
    if (N <= 1) return x;
    const ev = x.stride(2).array.fft;
    const od = x[1 .. $].stride(2).array.fft;
    alias E = std.complex.expi;
    auto l = iota(N / 2).map!(k => ev[k] + T(E(-2* PI * k/N)) * od[k]);
    auto r = iota(N / 2).map!(k => ev[k] - T(E(-2* PI * k/N)) * od[k]);
    return l.chain(r).array;
}

void main() {
    [1.0, 1, 1, 1, 0, 0, 0, 0].map!complex.array.fft.writeln;
}
```

```txt
[4+0i, 1-2.41421i, 0+0i, 1-0.414214i, 0+0i, 1+0.414214i, 0+0i, 1+2.41421i]
```



## EchoLisp


```scheme

(define -∏*2 (complex 0 (* -2 PI)))

(define (fft xs N)
	(if (<= N 1) xs
	(let* [
		(N/2 (/ N 2))
		(even (fft (for/vector ([i (in-range 0 N 2)]) [xs i]) N/2))
		(odd  (fft (for/vector ([i (in-range 1 N 2)]) [xs i]) N/2))
		]
	(for ((k N/2)) (vector*= odd k  (exp (/ (* -∏*2 k) N ))))
	(vector-append (vector-map + even odd) (vector-map - even odd)))))

(define data #( 1 1 1 1  0 0 0 0 ))

(fft data 8)
    → #( 4+0i 1-2.414213562373095i 0+0i 1-0.4142135623730949i
       0+0i 1+0.4142135623730949i 0+0i 1+2.414213562373095i)

```




## ERRE


```ERRE

PROGRAM FFT

CONST CNT=8

!$DYNAMIC
DIM REL[0],IMG[0],CMP[0],V[0]

BEGIN
   SIG=INT(LOG(CNT)/LOG(2)+0.9999)
   REAL1=2^SIG

   REAL=REAL1-1
   REAL2=INT(REAL1/2)
   REAL4=INT(REAL1/4)
   REAL3=REAL4+REAL2

!$DIM REL[REAL1],IMG[REAL1],CMP[REAL3]

FOR I=0 TO CNT-1 DO
   READ(REL[I],IMG[I])
END FOR

DATA(1,0,1,0,1,0,1,0,0,0,0,0,0,0,0,0)

SIG2=INT(SIG/2)
SIG1=SIG-SIG2
CNT1=2^SIG1
CNT2=2^SIG2

!$DIM V[CNT1-1]
V[0]=0
DV=1
PTR=CNT1

FOR J=1 TO SIG1 DO
  HLFPTR=INT(PTR/2)
  PT=CNT1-HLFPTR
  FOR I=HLFPTR TO PT STEP PTR DO
    V[I]=V[I-HLFPTR]+DV
  END FOR
  DV=2*DV
  PTR=HLFPTR
END FOR

K=2*π/REAL1

FOR X=0 TO REAL4 DO
   CMP[X]=COS(K*X)
   CMP[REAL2-X]=-CMP[X]
   CMP[REAL2+X]=-CMP[X]
END FOR

PRINT("FFT: BIT REVERSAL")

FOR I=0 TO CNT1-1 DO
  IP=I*CNT2
  FOR J=0 TO CNT2-1 DO
    H=IP+J
    G=V[J]*CNT2+V[I]
    IF G>H THEN
       SWAP(REL[G],REL[H])
       SWAP(IMG[G],IMG[H])
    END IF
  END FOR
END FOR

T=1
FOR STAGE=1 TO SIG DO
  PRINT("STAGE:";STAGE)
  D=INT(REAL2/T)
  FOR II=0 TO T-1 DO
     L=D*II
     LS=L+REAL4
     FOR I=0 TO D-1 DO
       A=2*I*T+II
       B=A+T
       F1=REL[A]
       F2=IMG[A]
       CNT1=CMP[L]*REL[B]
       CNT2=CMP[LS]*IMG[B]
       CNT3=CMP[LS]*REL[B]
       CNT4=CMP[L]*IMG[B]
       REL[A]=F1+CNT1-CNT2
       IMG[A]=F2+CNT3+CNT4
       REL[B]=F1-CNT1+CNT2
       IMG[B]=F2-CNT3-CNT4
     END FOR
  END FOR
  T=2*T
END FOR

PRINT("NUM REAL     IMAG")
FOR I=0 TO REAL DO
    IF ABS(REL[I])<1E-5 THEN REL[I]=0 END IF
    IF ABS(IMG[I])<1E-5 THEN IMG[I]=0 END IF
    PRINT(I;"";)
    WRITE("##.###### ##.######";REL[I];IMG[I])
END FOR
END PROGRAM

```

```txt

FFT: BIT REVERSAL
STAGE: 1
STAGE: 2
STAGE: 3
NUM REAL     IMAG
 0  4.000000  0.000000
 1  1.000000 -2.414214
 2  0.000000  0.000000
 3  1.000000 -0.414214
 4  0.000000  0.000000
 5  1.000000  0.414214
 6  0.000000  0.000000
 7  1.000000  2.414214

```



## Factor


```Factor

IN: USE math.transforms.fft
IN: { 1 1 1 1 0 0 0 0 } fft .
{
    C{ 4.0 0.0 }
    C{ 1.0 -2.414213562373095 }
    C{ 0.0 0.0 }
    C{ 1.0 -0.4142135623730949 }
    C{ 0.0 0.0 }
    C{ 0.9999999999999999 0.4142135623730949 }
    C{ 0.0 0.0 }
    C{ 0.9999999999999997 2.414213562373095 }
}

```



## Fortran


```Fortran

module fft_mod
  implicit none
  integer,       parameter :: dp=selected_real_kind(15,300)
  real(kind=dp), parameter :: pi=3.141592653589793238460_dp
contains

  ! In place Cooley-Tukey FFT
  recursive subroutine fft(x)
    complex(kind=dp), dimension(:), intent(inout)  :: x
    complex(kind=dp)                               :: t
    integer                                        :: N
    integer                                        :: i
    complex(kind=dp), dimension(:), allocatable    :: even, odd

    N=size(x)

    if(N .le. 1) return

    allocate(odd((N+1)/2))
    allocate(even(N/2))

    ! divide
    odd =x(1:N:2)
    even=x(2:N:2)

    ! conquer
    call fft(odd)
    call fft(even)

    ! combine
    do i=1,N/2
       t=exp(cmplx(0.0_dp,-2.0_dp*pi*real(i-1,dp)/real(N,dp),kind=dp))*even(i)
       x(i)     = odd(i) + t
       x(i+N/2) = odd(i) - t
    end do

    deallocate(odd)
    deallocate(even)

  end subroutine fft

end module fft_mod

program test
  use fft_mod
  implicit none
  complex(kind=dp), dimension(8) :: data = (/1.0, 1.0, 1.0, 1.0, 0.0,

0.0, 0.0, 0.0/)
  integer :: i

  call fft(data)

  do i=1,8
     write(*,'("(", F20.15, ",", F20.15, "i )")') data(i)
  end do

end program test
```

```txt

(   4.000000000000000,   0.000000000000000i )
(   1.000000000000000,  -2.414213562373095i )
(   0.000000000000000,   0.000000000000000i )
(   1.000000000000000,  -0.414213562373095i )
(   0.000000000000000,   0.000000000000000i )
(   1.000000000000000,   0.414213562373095i )
(   0.000000000000000,   0.000000000000000i )
(   1.000000000000000,   2.414213562373095i )
```



## GAP


```gap
# Here an implementation with no optimization (O(n^2)).
# In GAP, E(n) = exp(2*i*pi/n), a primitive root of the unity.

Fourier := function(a)
	local n, z;
	n := Size(a);
	z := E(n);
	return List([0 .. n - 1], k -> Sum([0 .. n - 1], j -> a[j + 1]*z^(-k*j)));
end;

InverseFourier := function(a)
	local n, z;
	n := Size(a);
	z := E(n);
	return List([0 .. n - 1], k -> Sum([0 .. n - 1], j -> a[j + 1]*z^(k*j)))/n;
end;

Fourier([1, 1, 1, 1, 0, 0, 0, 0]);
# [ 4, 1-E(8)-E(8)^2-E(8)^3, 0, 1-E(8)+E(8)^2-E(8)^3,
#   0, 1+E(8)-E(8)^2+E(8)^3, 0, 1+E(8)+E(8)^2+E(8)^3 ]

InverseFourier(last);
# [ 1, 1, 1, 1, 0, 0, 0, 0 ]
```



## Go


```go
package main

import (
    "fmt"
    "math"
    "math/cmplx"
)

func ditfft2(x []float64, y []complex128, n, s int) {
    if n == 1 {
        y[0] = complex(x[0], 0)
        return
    }
    ditfft2(x, y, n/2, 2*s)
    ditfft2(x[s:], y[n/2:], n/2, 2*s)
    for k := 0; k < n/2; k++ {
        tf := cmplx.Rect(1, -2*math.Pi*float64(k)/float64(n)) * y[k+n/2]
        y[k], y[k+n/2] = y[k]+tf, y[k]-tf
    }
}

func main() {
    x := []float64{1, 1, 1, 1, 0, 0, 0, 0}
    y := make([]complex128, len(x))
    ditfft2(x, y, len(x), 1)
    for _, c := range y {
        fmt.Printf("%8.4f\n", c)
    }
}
```

```txt

(  4.0000 +0.0000i)
(  1.0000 -2.4142i)
(  0.0000 +0.0000i)
(  1.0000 -0.4142i)
(  0.0000 +0.0000i)
(  1.0000 +0.4142i)
(  0.0000 +0.0000i)
(  1.0000 +2.4142i)

```



## Haskell


```haskell
import Data.Complex

-- Cooley-Tukey
fft [] = []
fft [x] = [x]
fft xs = zipWith (+) ys ts ++ zipWith (-) ys ts
    where n = length xs
          ys = fft evens
          zs = fft odds
          (evens, odds) = split xs
          split [] = ([], [])
          split [x] = ([x], [])
          split (x:y:xs) = (x:xt, y:yt) where (xt, yt) = split xs
          ts = zipWith (\z k -> exp' k n * z) zs [0..]
          exp' k n = cis $ -2 * pi * (fromIntegral k) / (fromIntegral n)

main = mapM_ print $ fft [1,1,1,1,0,0,0,0]
```


```txt
4.0 :+ 0.0
1.0 :+ (-2.414213562373095)
0.0 :+ 0.0
1.0 :+ (-0.4142135623730949)
0.0 :+ 0.0
0.9999999999999999 :+ 0.4142135623730949
0.0 :+ 0.0
0.9999999999999997 :+ 2.414213562373095

```



## Idris

<lang>module Main

import Data.Complex


concatPair : List (a, a) -> List (a)
concatPair xs with (unzip xs)
  | (xs1, xs2) = xs1 ++ xs2

fft' : List (Complex Double) -> Nat -> Nat -> List (Complex Double)
fft' (x::xs) (S Z) _ = [x]
fft' xs n s = concatPair $ map (\(x1,x2,k) =>
                let eTerm = ((cis (-2 * pi * ((cast k) - 1) / (cast n))) * x2) in
                  (x1 + eTerm, x1 - eTerm)) $ zip3 left right [1..n `div` 2]

             where
                  left : List (Complex Double)
                  right : List (Complex Double)
                  left  = fft' (xs) (n `div` 2) (2 * s)
                  right = fft' (drop s xs) (n `div` 2) (2 * s)


-- Recursive Cooley-Tukey with radix-2 DIT case
-- assumes no of points provided are a power of 2
fft : List (Complex Double) -> List (Complex Double)
fft [] = []
fft xs = fft' xs (length xs) 1


main : IO()
main = traverse_ printLn $ fft [1,1,1,1,0,0,0,0]
```


```txt

4 :+ 0
1 :+ -2.414213562373095
0 :+ 0
1 :+ -0.4142135623730949
0 :+ 0
0.9999999999999999 :+ 0.4142135623730949
0 :+ 0
0.9999999999999997 :+ 2.414213562373095

```



## J

Based on [[j:Essays/FFT]], with some simplifications -- sacrificing accuracy, optimizations and convenience which are not relevant to the task requirements, for clarity:


```j
cube  =: ($~ q:@#) :. ,
rou   =: ^@j.@o.@(% #)@i.@-:  NB. roots of unity
floop =: 4 : 'for_r. i.#$x do. (y=.{."1 y) ] x=.(+/x) ,&,:"r (-/x)*y end.'
fft   =: ] floop&.cube rou@#
```


Example (first row of result is sine, second row of result is fft of the first row, (**+)&.+. cleans an irrelevant least significant bit of precision from the result so that it displays nicely):


```j
   (**+)&.+. (,: fft) 1 o. 2p1*3r16 * i.16
0 0.92388 0.707107 0.382683 1 0.382683 0.707107 0.92388 0 0.92388 0.707107 0.382683 1 0.382683 0.707107 0.92388
0       0        0      0j8 0        0        0       0 0       0        0        0 0      0j8        0       0
```


Here is a representation of an example which appears in some of the other implementations, here:

```J
   Re=: {.@+.@fft
   Im=: {:@+.@fft
   M=: 4#1 0
   M
1 1 1 1 0 0 0 0
   Re M
4 1 0 1 0 1 0 1
   Im M
0 2.41421 0 0.414214 0 _0.414214 0 _2.41421
```


Note that Re and Im are not functions of 1 and 0 but are functions of the complete sequence.

Also note that J uses a different character for negative sign than for subtraction, to eliminate ambiguity (is this a single list of numbers or are lists being subtracted?).


## Java

```java
import static java.lang.Math.*;

public class FastFourierTransform {

    public static int bitReverse(int n, int bits) {
        int reversedN = n;
        int count = bits - 1;

        n >>= 1;
        while (n > 0) {
            reversedN = (reversedN << 1) | (n & 1);
            count--;
            n >>= 1;
        }

        return ((reversedN << count) & ((1 << bits) - 1));
    }

    static void fft(Complex[] buffer) {

        int bits = (int) (log(buffer.length) / log(2));
        for (int j = 1; j < buffer.length / 2; j++) {

            int swapPos = bitReverse(j, bits);
            Complex temp = buffer[j];
            buffer[j] = buffer[swapPos];
            buffer[swapPos] = temp;
        }

        for (int N = 2; N <= buffer.length; N <<= 1) {
            for (int i = 0; i < buffer.length; i += N) {
                for (int k = 0; k < N / 2; k++) {

                    int evenIndex = i + k;
                    int oddIndex = i + k + (N / 2);
                    Complex even = buffer[evenIndex];
                    Complex odd = buffer[oddIndex];

                    double term = (-2 * PI * k) / (double) N;
                    Complex exp = (new Complex(cos(term), sin(term)).mult(odd));

                    buffer[evenIndex] = even.add(exp);
                    buffer[oddIndex] = even.sub(exp);
                }
            }
        }
    }

    public static void main(String[] args) {
        double[] input = {1.0, 1.0, 1.0, 1.0, 0.0, 0.0, 0.0, 0.0};

        Complex[] cinput = new Complex[input.length];
        for (int i = 0; i < input.length; i++)
            cinput[i] = new Complex(input[i], 0.0);

        fft(cinput);

        System.out.println("Results:");
        for (Complex c : cinput) {
            System.out.println(c);
        }
    }
}

class Complex {
    public final double re;
    public final double im;

    public Complex() {
        this(0, 0);
    }

    public Complex(double r, double i) {
        re = r;
        im = i;
    }

    public Complex add(Complex b) {
        return new Complex(this.re + b.re, this.im + b.im);
    }

    public Complex sub(Complex b) {
        return new Complex(this.re - b.re, this.im - b.im);
    }

    public Complex mult(Complex b) {
        return new Complex(this.re * b.re - this.im * b.im,
                this.re * b.im + this.im * b.re);
    }

    @Override
    public String toString() {
        return String.format("(%f,%f)", re, im);
    }
}
```



```txt
Results:
(4,000000 + 0,000000 i)
(1,000000 + -2,414214 i)
(0,000000 + 0,000000 i)
(1,000000 + -0,414214 i)
(0,000000 + 0,000000 i)
(1,000000 + 0,414214 i)
(0,000000 + 0,000000 i)
(1,000000 + 2,414214 i)
```



## JavaScript

Complex fourier transform & it's inverse reimplemented from the C++ & Python
variants on this page.


```javascript
/*
complex fast fourier transform and inverse from
http://rosettacode.org/wiki/Fast_Fourier_transform#C.2B.2B
*/
function icfft(amplitudes)
{
	var N = amplitudes.length;
	var iN = 1 / N;

	//conjugate if imaginary part is not 0
	for(var i = 0 ; i < N; ++i)
		if(amplitudes[i] instanceof Complex)
			amplitudes[i].im = -amplitudes[i].im;

	//apply fourier transform
	amplitudes = cfft(amplitudes)

	for(var i = 0 ; i < N; ++i)
	{
		//conjugate again
		amplitudes[i].im = -amplitudes[i].im;
		//scale
		amplitudes[i].re *= iN;
		amplitudes[i].im *= iN;
	}
	return amplitudes;
}

function cfft(amplitudes)
{
	var N = amplitudes.length;
	if( N <= 1 )
		return amplitudes;

	var hN = N / 2;
	var even = [];
	var odd = [];
	even.length = hN;
	odd.length = hN;
	for(var i = 0; i < hN; ++i)
	{
		even[i] = amplitudes[i*2];
		odd[i] = amplitudes[i*2+1];
	}
	even = cfft(even);
	odd = cfft(odd);

	var a = -2*Math.PI;
	for(var k = 0; k < hN; ++k)
	{
		if(!(even[k] instanceof Complex))
			even[k] = new Complex(even[k], 0);
		if(!(odd[k] instanceof Complex))
			odd[k] = new Complex(odd[k], 0);
		var p = k/N;
		var t = new Complex(0, a * p);
		t.cexp(t).mul(odd[k], t);
		amplitudes[k] = even[k].add(t, odd[k]);
		amplitudes[k + hN] = even[k].sub(t, even[k]);
	}
	return amplitudes;
}

//test code
//console.log( cfft([1,1,1,1,0,0,0,0]) );
//console.log( icfft(cfft([1,1,1,1,0,0,0,0])) );
```

Very very basic Complex number that provides only the components
required by the code above.

```javascript
/*
basic complex number arithmetic from
http://rosettacode.org/wiki/Fast_Fourier_transform#Scala
*/
function Complex(re, im)
{
	this.re = re;
	this.im = im || 0.0;
}
Complex.prototype.add = function(other, dst)
{
	dst.re = this.re + other.re;
	dst.im = this.im + other.im;
	return dst;
}
Complex.prototype.sub = function(other, dst)
{
	dst.re = this.re - other.re;
	dst.im = this.im - other.im;
	return dst;
}
Complex.prototype.mul = function(other, dst)
{
	//cache re in case dst === this
	var r = this.re * other.re - this.im * other.im;
	dst.im = this.re * other.im + this.im * other.re;
	dst.re = r;
	return dst;
}
Complex.prototype.cexp = function(dst)
{
	var er = Math.exp(this.re);
	dst.re = er * Math.cos(this.im);
	dst.im = er * Math.sin(this.im);
	return dst;
}
Complex.prototype.log = function()
{
	/*
	although 'It's just a matter of separating out the real and imaginary parts of jw.' is not a helpful quote
	the actual formula I found here and the rest was just fiddling / testing and comparing with correct results.
	http://cboard.cprogramming.com/c-programming/89116-how-implement-complex-exponential-functions-c.html#post637921
	*/
	if( !this.re )
		console.log(this.im.toString()+'j');
	else if( this.im < 0 )
		console.log(this.re.toString()+this.im.toString()+'j');
	else
		console.log(this.re.toString()+'+'+this.im.toString()+'j');
}
```



## jq

Currently jq has no support for complex numbers, so the following implementation uses [x,y] to represent the complex number x+iy.

### =Complex number arithmetic=


```jq

# multiplication of real or complex numbers
def cmult(x; y):
    if (x|type) == "number" then
       if  (y|type) == "number" then [ x*y, 0 ]
       else [x * y[0], x * y[1]]
       end
    elif (y|type) == "number" then cmult(y;x)
    else [ x[0] * y[0] - x[1] * y[1],  x[0] * y[1] + x[1] * y[0]]
    end;

def cplus(x; y):
    if (x|type) == "number" then
       if  (y|type) == "number" then [ x+y, 0 ]
       else [ x + y[0], y[1]]
       end
    elif (y|type) == "number" then cplus(y;x)
    else [ x[0] + y[0], x[1] + y[1] ]
    end;

def cminus(x; y): cplus(x; cmult(-1; y));

# e(ix) = cos(x) + i sin(x)
def expi(x): [ (x|cos), (x|sin) ];
```


### =FFT=


```jq
def fft:
  length as $N
  | if $N <= 1 then .
    else   ( [ .[ range(0; $N; 2) ] ] | fft) as $even
         | ( [ .[ range(1; $N; 2) ] ] | fft) as $odd
         | (1|atan * 4) as $pi
         | [ range(0; $N/2) | cplus($even[.];  cmult( expi(-2*$pi*./$N); $odd[.] )) ] +
           [ range(0; $N/2) | cminus($even[.]; cmult( expi(-2*$pi*./$N); $odd[.] )) ]
    end;
```

Example:

```jq
[1.0, 1.0, 1.0, 1.0, 0.0, 0.0, 0.0, 0.0] | fft

```

 [[4,-0],[1,-2.414213562373095],
  [0,0],[1,-0.4142135623730949],
  [0,0],[0.9999999999999999,0.4142135623730949],
  [0,0],[0.9999999999999997,2.414213562373095]]


## Julia


```julia
using FFTW # or using DSP

fft([1,1,1,1,0,0,0,0])
```

```julia
8-element Array{Complex{Float64},1}:
      4.0+0.0im
  1.0-2.41421im
      0.0+0.0im
 1.0-0.414214im
      0.0+0.0im
 1.0+0.414214im
      0.0+0.0im
  1.0+2.41421im
```


An implementation of the radix-2 algorithm, which works for any vector for length that is a power of 2:

```julia

function fft(a)
    y1 = Any[]; y2 = Any[]
    n = length(a)
    if n ==1 return a end
    wn(n) = exp(-2*π*im/n)
    y_even = fft(a[1:2:end])
    y_odd = fft(a[2:2:end])
    w = 1
    for k in 1:Int(n/2)
        push!(y1, y_even[k] + w*y_odd[k])
        push!(y2, y_even[k] - w*y_odd[k])
        w = w*wn(n)
    end
    return vcat(y1,y2)
end

```



## Kotlin

From Scala.

```scala
import java.lang.Math.*

class Complex(val re: Double, val im: Double) {
    operator infix fun plus(x: Complex) = Complex(re + x.re, im + x.im)
    operator infix fun minus(x: Complex) = Complex(re - x.re, im - x.im)
    operator infix fun times(x: Double) = Complex(re * x, im * x)
    operator infix fun times(x: Complex) = Complex(re * x.re - im * x.im, re * x.im + im * x.re)
    operator infix fun div(x: Double) = Complex(re / x, im / x)
    val exp: Complex by lazy { Complex(cos(im), sin(im)) * (cosh(re) + sinh(re)) }

    override fun toString() = when {
        b == "0.000" -> a
        a == "0.000" -> b + 'i'
        im > 0 -> a + " + " + b + 'i'
        else -> a + " - " + b + 'i'
    }

    private val a = "%1.3f".format(re)
    private val b = "%1.3f".format(abs(im))
}
```



```scala
object FFT {
    fun fft(a: Array<Complex>) = _fft(a, Complex(0.0, 2.0), 1.0)
    fun rfft(a: Array<Complex>) = _fft(a, Complex(0.0, -2.0), 2.0)

    private fun _fft(a: Array<Complex>, direction: Complex, scalar: Double): Array<Complex> =
            if (a.size == 1)
                a
            else {
                val n = a.size
                require(n % 2 == 0, { "The Cooley-Tukey FFT algorithm only works when the length of the input is even." })

                var (evens, odds) = Pair(emptyArray<Complex>(), emptyArray<Complex>())
                for (i in a.indices)
                    if (i % 2 == 0) evens += a[i]
                    else odds += a[i]
                evens = _fft(evens, direction, scalar)
                odds = _fft(odds, direction, scalar)

                val pairs = (0 until n / 2).map {
                    val offset = (direction * (java.lang.Math.PI * it / n)).exp * odds[it] / scalar
                    val base = evens[it] / scalar
                    Pair(base + offset, base - offset)
                }
                var (left, right) = Pair(emptyArray<Complex>(), emptyArray<Complex>())
                for ((l, r) in pairs) { left += l; right += r }
                left + right
            }
}
```



```scala
fun Array<*>.println() = println(joinToString(prefix = "[", postfix = "]"))

fun main(args: Array<String>) {
    val data = arrayOf(Complex(1.0, 0.0), Complex(1.0, 0.0), Complex(1.0, 0.0), Complex(1.0, 0.0),
            Complex(0.0, 0.0), Complex(0.0, 2.0), Complex(0.0, 0.0), Complex(0.0, 0.0))

    val a = FFT.fft(data)
    a.println()
    FFT.rfft(a).println()
}
```


```txt
[4.000 + 2.000i, 2.414 + 1.000i, -2.000, 2.414 + 1.828i, 2.000i, -0.414 + 1.000i, 2.000, -0.414 - 3.828i]
[1.000, 1.000, 1.000, 1.000, 0.000, 2.000i, 0.000, 0.000]
```



## lambdatalk


```scheme


1) the function fft

{def fft
 {lambda {:s :x}
  {if {= {list.length :x} 1}
   then :x
   else {let { {:s :s}
               {:ev {fft :s {evens :x}} }
               {:od {fft :s {odds  :x}} } }
        {let { {:ev :ev} {:t {rotate :s :od 0 {list.length :od}}} }
        {list.append {list.map Cadd :ev :t}
                     {list.map Csub :ev :t}} }}}}}

{def rotate
 {lambda {:s :f :k :N}
  {if {list.null? :f}
   then nil
   else {cons {Cmul {car :f} {Cexp {Cnew 0 {/ {* :s {PI} :k} :N}}}}
              {rotate :s {cdr :f} {+ :k 1} :N}}}}}

2) functions for lists

We add to the existing {lambda talk}'s list primitives a small set of functions required by the function fft.

{def evens
 {lambda {:l}
  {if {list.null? :l}
   then nil
   else {cons {car :l} {evens {cdr {cdr :l}}}}}}}

{def odds
 {lambda {:l}
  {if {list.null? {cdr :l}}
   then nil
   else {cons {car {cdr :l}} {odds {cdr {cdr :l}}}}}}}

{def list.map
 {def list.map.r
  {lambda {:f :a :b :c}
   {if {list.null? :a}
    then :c
    else {list.map.r :f {cdr :a} {cdr :b}
                        {cons {:f {car :a} {car :b}} :c}} }}}
 {lambda {:f :a :b}
  {list.map.r :f {list.reverse :a} {list.reverse :b} nil}}}

{def list.append
 {def list.append.r
  {lambda {:a :b}
   {if {list.null? :b}
    then :a
    else {list.append.r {cons {car :b} :a} {cdr :b}}}}}
 {lambda {:a :b}
  {list.append.r :b {list.reverse :a}} }}

3) functions for Cnumbers

{lambda talk} has no primitive functions working on complex numbers. We add the minimal set required by the function fft.

{def Cnew
 {lambda {:x :y}
  {cons :x :y} }}

{def Cnorm
 {lambda {:c}
  {sqrt {+ {* {car :c} {car :c}}
           {* {cdr :c} {cdr :c}}}} }}

{def Cadd
 {lambda {:x :y}
  {cons {+ {car :x} {car :y}}
        {+ {cdr :x} {cdr :y}}} }}

{def Csub
 {lambda {:x :y}
  {cons {- {car :x} {car :y}}
        {- {cdr :x} {cdr :y}}} }}

{def Cmul
 {lambda {:x :y}
  {cons {- {* {car :x} {car :y}} {* {cdr :x} {cdr :y}}}
        {+ {* {car :x} {cdr :y}} {* {cdr :x} {car :y}}}} }}

{def Cexp
  {lambda {:x}
   {cons {* {exp {car :x}} {cos {cdr :x}}}
         {* {exp {car :x}} {sin {cdr :x}}}} }}

{def Clist
 {lambda {:s}
  {list.new {map {lambda {:i} {cons :i 0}} :s}}}}

4) testing

Applying the fft function on such a sample (1 1 1 1 0 0 0 0) where numbers have been promoted as complex

{list.disp {fft -1 {Clist 1 1 1 1 0 0 0 0}}} ->

(4 0)
(1 -2.414213562373095)
(0 0)
(1 -0.4142135623730949)
(0 0)
(0.9999999999999999 0.4142135623730949)
(0 0)
(0.9999999999999997 2.414213562373095)

A more usefull example can be seen in http://lambdaway.free.fr/lambdaspeech/?view=zorg


```



## Liberty BASIC


```lb

    P =8
    S  =int( log( P) /log( 2) +0.9999)

    Pi =3.14159265
    R1 =2^S

    R =R1 -1
    R2 =div( R1,  2)
    R4 =div( R1,  4)
    R3 =R4 +R2

    Dim Re( R1), Im( R1), Co( R3)

    for N =0 to P -1
        read dummy: Re( N) =dummy
        read dummy: Im( N) =dummy
    next N

    data    1, 0,      1, 0,      1, 0,      1, 0,      0, 0,     0, 0,      0, 0,       0, 0

    S2 =div( S, 2)
    S1 =S -S2
    P1 =2^S1
    P2 =2^S2

    dim V( P1 -1)
    V( 0) =0
    DV =1
    DP =P1

    for J =1 to S1
        HA =div( DP, 2)
        PT =P1 -HA
        for I =HA to PT step DP
            V( I) =V( I -HA) +DV
        next I
        DV =DV +DV
        DP =HA
    next J

    K =2 *Pi /R1

    for X =0 to R4
        COX =cos( K *X)
        Co( X) =COX
        Co( R2 -X) =0 -COX
        Co( R2 +X) =0 -COX
    next X

    print "FFT: bit reversal"

    for I =0 to P1 -1
        IP =I *P2
        for J =0 to P2 -1
            H =IP +J
            G =V( J) *P2 +V( I)
            if G >H then temp =Re( G): Re( G) =Re( H): Re( H) =temp
            if G >H then temp =Im( G): Im( G) =Im( H): Im( H) =temp
        next J
    next I

    T =1

    for stage =0 to S -1
        print "  Stage:- "; stage
        D =div( R2, T)
        for Z =0 to T -1
            L   =D *Z
            LS  =L +R4
            for I =0 to D -1
                A      =2 *I *T +Z
                B      =A +T
                F1     =Re( A)
                F2     =Im( A)
                P1     =Co( L)  *Re( B)
                P2     =Co( LS) *Im( B)
                P3     =Co( LS) *Re( B)
                P4     =Co( L)  *Im( B)
                Re( A) =F1 +P1 -P2
                Im( A) =F2 +P3 +P4
                Re( B) =F1 -P1 +P2
                Im( B) =F2 -P3 -P4
            next I
        next Z
        T =T +T
    next stage

    print "   M          Re( M)       Im( M)"

    for M =0 to R
        if abs( Re( M)) <10^-5 then Re( M) =0
        if abs( Im( M)) <10^-5 then Im( M) =0
        print "   "; M, Re( M), Im( M)
    next M

    end


    wait

    function div( a, b)
        div =int( a /b)
    end function

    end

```


    M          Re( M)       Im( M)
   0          4             0
   1          1.0           -2.41421356
   2          0             0
   3          1.0           -0.41421356
   4          0             0
   5          1.0           0.41421356
   6          0             0
   7          1.0           2.41421356


## Lua


```Lua
-- operations on complex number
complex = {__mt={} }

function complex.new (r, i)
  local new={r=r, i=i or 0}
  setmetatable(new,complex.__mt)
  return new
end

function complex.__mt.__add (c1, c2)
  return complex.new(c1.r + c2.r, c1.i + c2.i)
end

function complex.__mt.__sub (c1, c2)
  return complex.new(c1.r - c2.r, c1.i - c2.i)
end

function complex.__mt.__mul (c1, c2)
  return complex.new(c1.r*c2.r - c1.i*c2.i,
                      c1.r*c2.i + c1.i*c2.r)
end

function complex.expi (i)
  return complex.new(math.cos(i),math.sin(i))
end

function complex.__mt.__tostring(c)
  return "("..c.r..","..c.i..")"
end


-- Cooley–Tukey FFT (in-place, divide-and-conquer)
-- Higher memory requirements and redundancy although more intuitive
function fft(vect)
  local n=#vect
  if n<=1 then return vect end
-- divide
  local odd,even={},{}
  for i=1,n,2 do
    odd[#odd+1]=vect[i]
    even[#even+1]=vect[i+1]
  end
-- conquer
  fft(even);
  fft(odd);
-- combine
  for k=1,n/2 do
    local t=even[k] * complex.expi(-2*math.pi*(k-1)/n)
    vect[k] = odd[k] + t;
    vect[k+n/2] = odd[k] - t;
  end
  return vect
end

function toComplex(vectr)
  vect={}
  for i,r in ipairs(vectr) do
    vect[i]=complex.new(r)
  end
  return vect
end

-- test
data = toComplex{1, 1, 1, 1, 0, 0, 0, 0};

print("orig:", unpack(data))
print("fft:", unpack(fft(data)))
```



## Maple

Maple has a built-in package DiscreteTransforms, and FourierTransform and InverseFourierTransform are in the commands available from that package. The FourierTransform command offers an FFT method by default.


```Maple

with( DiscreteTransforms ):

FourierTransform( <1,1,1,1,0,0,0,0>, normalization=none );

```



```txt

                         [       4. + 0. I        ]
                         [                        ]
                         [1. - 2.41421356237309 I ]
                         [                        ]
                         [       0. + 0. I        ]
                         [                        ]
                         [1. - 0.414213562373095 I]
                         [                        ]
                         [       0. + 0. I        ]
                         [                        ]
                         [1. + 0.414213562373095 I]
                         [                        ]
                         [       0. + 0. I        ]
                         [                        ]
                         [1. + 2.41421356237309 I ]

```

Optionally, the FFT may be performed inplace on a Vector of hardware double-precision complex floats.

```Maple

v := Vector( [1,1,1,1,0,0,0,0], datatype=complex[8] ):

FourierTransform( v, normalization=none, inplace ):

v;

```



```txt

                         [       4. + 0. I        ]
                         [                        ]
                         [1. - 2.41421356237309 I ]
                         [                        ]
                         [       0. + 0. I        ]
                         [                        ]
                         [1. - 0.414213562373095 I]
                         [                        ]
                         [       0. + 0. I        ]
                         [                        ]
                         [1. + 0.414213562373095 I]
                         [                        ]
                         [       0. + 0. I        ]
                         [                        ]
                         [1. + 2.41421356237309 I ]

```


```Maple

InverseFourierTransform( v, normalization=full, inplace ):

v;

```



```txt

                       [          1. + 0. I          ]
                       [                             ]
                       [          1. + 0. I          ]
                       [                             ]
                       [          1. + 0. I          ]
                       [                             ]
                       [          1. + 0. I          ]
                       [                             ]
                       [          0. + 0. I          ]
                       [                             ]
                       [          0. + 0. I          ]
                       [                             ]
                       [                   -17       ]
                       [5.55111512312578 10    + 0. I]
                       [                             ]
                       [          0. + 0. I          ]

```


=={{header|Mathematica}} / {{header|Wolfram Language}}==
Mathematica has a built-in FFT function which uses a proprietary algorithm developed at Wolfram Research.
It also has an option to tune the algorithm for specific applications.
The options shown below, while not default,
produce output that is consistent with most other FFT routines.


```Mathematica

Fourier[{1,1,1,1,0,0,0,0}, FourierParameters->{1,-1}]

```


```txt
{4. + 0. I, 1. - 2.4142136 I, 0. + 0. I, 1. - 0.41421356 I, 0. + 0. I, 1. + 0.41421356 I, 0. + 0. I, 1. + 2.4142136 I}
```


=={{header|MATLAB}} / {{header|Octave}}==

Matlab/Octave have a builtin FFT function.


```MATLAB
 fft([1,1,1,1,0,0,0,0]')

```

```txt
ans =

   4.00000 + 0.00000i
   1.00000 - 2.41421i
   0.00000 + 0.00000i
   1.00000 - 0.41421i
   0.00000 + 0.00000i
   1.00000 + 0.41421i
   0.00000 - 0.00000i
   1.00000 + 2.41421i
```



## Maxima


```maxima
load(fft)$
fft([1, 2, 3, 4]);
[2.5, -0.5 * %i - 0.5, -0.5, 0.5 * %i - 0.5]
```



## Nim

```nim
import math, complex, strutils

# Works with floats and complex numbers as input
proc fft[T: float | Complex[float]](x: openarray[T]): seq[Complex[float]] =
  let n = x.len
  if n == 0: return

  result.newSeq(n)

  if n == 1:
    result[0] = (when T is float: complex(x[0]) else: x[0])
    return

  var evens, odds = newSeq[T]()
  for i, v in x:
    if i mod 2 == 0: evens.add v
    else: odds.add v
  var (even, odd) = (fft(evens), fft(odds))

  let halfn = n div 2

  for k in 0 .. < halfn:
    let a = exp(complex(0.0, -2 * Pi* float(k) / float(n))) * odd[k]
    result[k] = even[k] + a
    result[k + halfn] = even[k] - a

for i in fft(@[1.0, 1.0, 1.0, 1.0, 0.0, 0.0, 0.0, 0.0]):
  echo formatFloat(abs(i), ffDecimal, 3)
```

```txt
4.000
2.613
-0.000
1.082
-0.000
1.082
-0.000
2.613
```



## OCaml

This is a simple implementation of the Cooley-Tukey pseudo-code

```OCaml
open Complex

let fac k n =
   let m2pi = -4.0 *. acos 0.0 in
   polar 1.0 (m2pi*.(float k)/.(float n))

let merge l r n =
   let f (k,t) x = (succ k, (mul (fac k n) x) :: t) in
   let z = List.rev (snd (List.fold_left f (0,[]) r)) in
   (List.map2 add l z) @ (List.map2 sub l z)

let fft lst =
   let rec ditfft2 a n s =
      if n = 1 then [List.nth lst a] else
      let odd = ditfft2 a (n/2) (2*s) in
      let even = ditfft2 (a+s) (n/2) (2*s) in
      merge odd even n in
   ditfft2 0 (List.length lst) 1;;

let show l =
   let pr x = Printf.printf "(%f %f) " x.re x.im in
   (List.iter pr l; print_newline ()) in
let indata = [one;one;one;one;zero;zero;zero;zero] in
show indata;
show (fft indata)
```


```txt

(1.000000 0.000000) (1.000000 0.000000) (1.000000 0.000000) (1.000000 0.000000) (0.000000 0.000000) (0.000000 0.000000) (0.000000 0.000000) (0.000000 0.000000)
(4.000000 0.000000) (1.000000 -2.414214) (0.000000 0.000000) (1.000000 -0.414214) (0.000000 0.000000) (1.000000 0.414214) (0.000000 0.000000) (1.000000 2.414214)

```




## ooRexx

{{trans|PL/I}} Output as shown in REXX

```oorexx
Numeric Digits 16
list='1 1 1 1 0 0 0 0'
n=words(list)
x=.array~new(n)
Do i=1 To n
  x[i]=.complex~new(word(list,i),0)
  End
Call show 'FFT  in',x
call fft x
Call show 'FFT out',x
Exit

show: Procedure
  Use Arg data,x
  Say '---data---   num       real-part   imaginary-part'
  Say '----------   ---       ---------   --------------'
  Do i=1 To x~size
    say data right(i,7)'       ' x[i]~string
    End
  Return

fft: Procedure
  Use Arg in
  Numeric Digits 16
  n=in~size
  If n=1 Then Return
  odd=.array~new(n/2)
  even=.array~new(n/2)
  Do j=1 To n By 2; odd[(j+1)/2]=in[j]; End
  Do j=2 To n By 2; even[j/2]=in[j]; End
  Call fft odd
  Call fft even
  pi=3.14159265358979323E0
  n_2=n/2
  Do i=1 To n_2
    w=-2*pi*(i-1)/N
    t=.complex~new(rxCalcCos(w,,'R'),rxCalcSin(w,,'R'))*even[i]
    in[i]=odd[i]+t
    in[i+n_2]=odd[i]-t
    End
  Return

::class complex
::method init
  expose r i
  use strict arg r, i = 0

-- complex instances are immutable, so these are
-- read only attributes
::attribute r GET
::attribute i GET

::method add
  expose r i
  Numeric Digits 16
  use strict arg other
  if other~isa(.complex) then
     return self~class~new(r + other~r, i + other~i)
  else return self~class~new(r + other, i)

::method subtract
  expose r i
  Numeric Digits 16
  use strict arg other
  if other~isa(.complex) then
     return self~class~new(r - other~r, i - other~i)
  else return self~class~new(r - other, i)

::method "+"
  Numeric Digits 16
  -- need to check if this is a prefix plus or an addition
  if arg() == 0 then
      return self  -- we can return this copy since it is immutable
  else
      forward message("ADD")

::method "-"
  Numeric Digits 16
  -- need to check if this is a prefix minus or a subtract
  if arg() == 0 then
      forward message("NEGATIVE")
  else
      forward message("SUBTRACT")

::method times
  expose r i
  Numeric Digits 16
  use strict arg other
  if other~isa(.complex) then
     return self~class~new(r * other~r - i * other~i, r * other~i + i * other~r)
  else return self~class~new(r * other, i * other)

::method "*"
  Numeric Digits 16
  forward message("TIMES")

::method string
  expose r i
  Numeric Digits 12
  Select
    When i=0 Then
      If r=0 Then
        Return '0'
      Else
        Return format(r,1,9)
    When i>0 Then
      Return format(r,1,9)' +'format(i,1,9)'i'
    Otherwise
      Return format(r,1,9)' -'format(abs(i),1,9)'i'
    End

::method formatnumber private
  use arg value
  Numeric Digits 16
  if value > 0 then return "+" value
  else return "-" value~abs

::requires rxMath library
```

```txt
---data---   num       real-part   imaginary-part
----------   ---       ---------   --------------
FFT  in       1        1.000000000
FFT  in       2        1.000000000
FFT  in       3        1.000000000
FFT  in       4        1.000000000
FFT  in       5        0
FFT  in       6        0
FFT  in       7        0
FFT  in       8        0
---data---   num       real-part   imaginary-part
----------   ---       ---------   --------------
FFT out       1        4.000000000
FFT out       2        1.000000000 -2.414213562i
FFT out       3        0
FFT out       4        1.000000000 -0.414213562i
FFT out       5        0
FFT out       6        1.000000000 +0.414213562i
FFT out       7        0
FFT out       8        1.000000000 +2.414213562i
```



## PARI/GP

Naive implementation, using the same testcase as Ada:

```parigp
FFT(v)=my(t=-2*Pi*I/#v,tt);vector(#v,k,tt=t*(k-1);sum(n=0,#v-1,v[n+1]*exp(tt*n)));
FFT([1,1,1,1,0,0,0,0])
```

```txt
[4.0000000000000000000000000000000000000, 1.0000000000000000000000000000000000000 - 2.4142135623730950488016887242096980786*I, 0.E-37 + 0.E-38*I, 1.0000000000000000000000000000000000000 - 0.41421356237309504880168872420969807856*I, 0.E-38 + 0.E-37*I, 0.99999999999999999999999999999999999997 + 0.41421356237309504880168872420969807860*I, 4.701977403289150032 E-38 + 0.E-38*I, 0.99999999999999999999999999999999999991 + 2.4142135623730950488016887242096980785*I]
```



## Perl

```Perl
use strict;
use warnings;
use Math::Complex;

sub fft {
    return @_ if @_ == 1;
    my @evn = fft(@_[grep { not $_ % 2 } 0 .. $#_ ]);
    my @odd = fft(@_[grep { $_ % 2 } 1 .. $#_ ]);
    my $twd = 2*i* pi / @_;
    $odd[$_] *= exp( $_ * -$twd ) for 0 .. $#odd;
    return
    (map { $evn[$_] + $odd[$_] } 0 .. $#evn ),
    (map { $evn[$_] - $odd[$_] } 0 .. $#evn );
}

print "$_\n" for fft qw(1 1 1 1 0 0 0 0);
```

```txt
4
1-2.41421356237309i
0
1-0.414213562373095i
0
1+0.414213562373095i
0
1+2.41421356237309i
```



## Perl 6

```perl6
sub fft {
    return @_ if @_ == 1;
    my @evn = fft( @_[0, 2 ... *] );
    my @odd = fft( @_[1, 3 ... *] ) Z*
    map &cis, (0, -tau / @_ ... *);
    return flat @evn »+« @odd, @evn »-« @odd;
}

.say for fft <1 1 1 1 0 0 0 0>;
```

```txt
4+0i
1-2.41421356237309i
0+0i
1-0.414213562373095i
0+0i
1+0.414213562373095i
0+0i
1+2.41421356237309i
```


For the fun of it, here is a purely functional version:


```perl6
sub fft {
    @_ == 1 ?? @_ !!
    fft(@_[0,2...*]) «+«
    fft(@_[1,3...*]) «*« map &cis, (0,-τ/@_...^-τ)
}
```


This particular version is numerically inaccurate though, because of the pi approximation.  It is possible to fix it with the 'cisPi' function available in the TrigPi module:


```perl6
sub fft {
    use TrigPi;
    @_ == 1 ?? @_ !!
    fft(@_[0,2...*]) «+«
    fft(@_[1,3...*]) «*« map &cisPi, (0,-2/@_...^-2)
}
```



## Phix


```Phix
--
-- demo\rosetta\FastFourierTransform.exw
--
### ===============================

--
--  Originally written by Robert Craig and posted to EuForum Dec 13, 2001
--

constant REAL = 1, IMAG = 2

type complex(sequence x)
    return length(x)=2 and atom(x[REAL]) and atom(x[IMAG])
end type

function p2round(integer x)
-- rounds x up to a power of two
integer p = 1
    while p<x do
        p += p
    end while
    return p
end function

function log2(atom x)
-- return log2 of x, or -1 if x is not a power of 2
    if x>0 then
        integer p = -1
        while floor(x)=x do
            x /= 2
            p += 1
        end while
        if x=0.5 then
            return p
        end if
    end if
    return -1
end function

function bitrev(sequence a)
-- bitrev an array of complex numbers
integer j=1, n = length(a)
    for i=1 to n-1 do
        if i<j then
            {a[i],a[j]} = {a[j],a[i]}
        end if
        integer k = n/2
        while k<j do
            j -= k
            k /= 2
        end while
        j = j+k
    end for
    return a
end function

function cmult(complex arg1, complex arg2)
-- complex multiply
    return {arg1[REAL]*arg2[REAL]-arg1[IMAG]*arg2[IMAG],
            arg1[REAL]*arg2[IMAG]+arg1[IMAG]*arg2[REAL]}
end function

function ip_fft(sequence a)
-- perform an in-place fft on an array of complex numbers
-- that has already been bit reversed
integer n = length(a)
integer ip, le, le1
complex u, w, t

    for l=1 to log2(n) do
        le = power(2, l)
        le1 = le/2
        u = {1, 0}
        w = {cos(PI/le1), sin(PI/le1)}
        for j=1 to le1 do
            for i=j to n by le do
                ip = i+le1
                t = cmult(a[ip], u)
                a[ip] = sq_sub(a[i],t)
                a[i] = sq_add(a[i],t)
            end for
            u = cmult(u, w)
        end for
    end for
    return a
end function

function fft(sequence a)
integer n = length(a)
    if log2(n)=-1 then
        puts(1, "input vector length is not a power of two, padded with 0's\n\n")
        n = p2round(n)
         -- pad with 0's
        for j=length(a)+1 to n do
            a = append(a,{0, 0})
        end for
    end if
    a = ip_fft(bitrev(a))
    -- reverse output from fft to switch +ve and -ve frequencies
    for i=2 to n/2 do
        integer j = n+2-i
        {a[i],a[j]} = {a[j],a[i]}
    end for
    return a
end function

function ifft(sequence a)
integer n = length(a)
    if log2(n)=-1 then ?9/0 end if -- (or as above?)
    a = ip_fft(bitrev(a))
    -- modifies results to get inverse fft
    for i=1 to n do
        a[i] = sq_div(a[i],n)
    end for
    return a
end function

constant a = {{1, 0},
              {1, 0},
              {1, 0},
              {1, 0},
              {0, 0},
              {0, 0},
              {0, 0},
              {0, 0}}

printf(1, "Results of %d-point fft:\n\n", length(a))
ppOpt({pp_Nest,1,pp_IntFmt,"%10.6f",pp_FltFmt,"%10.6f"})
pp(fft(a))
printf(1, "\nResults of %d-point inverse fft (rounded to 6 d.p.):\n\n", length(a))
pp(ifft(fft(a)))
```

```txt

Results of 8-point fft:

{{  4.000000,  0.000000},
 {  1.000000, -2.414214},
 {  0.000000,  0.000000},
 {  1.000000, -0.414214},
 {  0.000000,  0.000000},
 {  1.000000,  0.414214},
 {  0.000000,  0.000000},
 {  1.000000,  2.414214}}

Results of 8-point inverse fft (rounded to 6 d.p.):

{{  1.000000,  0.000000},
 {  1.000000, -0.000000},
 {  1.000000, -0.000000},
 {  1.000000, -0.000000},
 {  0.000000,  0.000000},
 {  0.000000,  0.000000},
 {  0.000000,  0.000000},
 {  0.000000,  0.000000}}

```



## PHP

Complex Fourier transform the inverse reimplemented from the C++, Python & JavaScript variants on this page.

Complex Class File:

```PHP

<?php

class Complex
{
    public $real;
    public $imaginary;

    function __construct($real, $imaginary){
        $this->real = $real;
        $this->imaginary = $imaginary;
    }

    function Add($other, $dst){
        $dst->real = $this->real + $other->real;
        $dst->imaginary = $this->imaginary + $other->imaginary;
        return $dst;
    }

    function Subtract($other, $dst){

        $dst->real = $this->real - $other->real;
        $dst->imaginary = $this->imaginary - $other->imaginary;
        return $dst;
    }

    function Multiply($other, $dst){
        //cache real in case dst === this
        $r = $this->real * $other->real - $this->imaginary * $other->imaginary;
        $dst->imaginary = $this->real * $other->imaginary + $this->imaginary * $other->real;
        $dst->real = $r;
        return $dst;
    }

    function ComplexExponential($dst){
        $er = exp($this->real);
        $dst->real = $er * cos($this->imaginary);
        $dst->imaginary = $er * sin($this->imaginary);
        return $dst;
    }
}


```


Example:

```PHP

<?php

include 'complex.class.php';

function IFFT($amplitudes)
{
    $N = count($amplitudes);
    $iN = 1 / $N;

    // Conjugate if imaginary part is not 0
    for($i = 0; $i < $N; ++$i){
        if($amplitudes[$i] instanceof Complex){
            $amplitudes[$i]->imaginary = -$amplitudes[$i]->imaginary;
        }
    }

    // Apply Fourier Transform
    $amplitudes = FFT($amplitudes);

    for($i = 0; $i < $N; ++$i){
        //Conjugate again
        $amplitudes[$i]->imaginary = -$amplitudes[$i]->imaginary;
        // Scale
        $amplitudes[$i]->real *= $iN;
        $amplitudes[$i]->imaginary *= $iN;
    }
    return $amplitudes;
}


function FFT($amplitudes)
{
    $N = count($amplitudes);
    if($N <= 1){
        return $amplitudes;
    }

    $hN = $N / 2;

    $even =  array_pad(array() , $hN, 0);
    $odd =  array_pad(array() , $hN, 0);
    for($i = 0; $i < $hN; ++$i){
        $even[$i] = $amplitudes[$i*2];
        $odd[$i] = $amplitudes[$i*2+1];
    }
    $even = FFT($even);
    $odd = FFT($odd);

    $a = -2*PI();
    for($k = 0; $k < $hN; ++$k){
        if(!($even[$k] instanceof Complex)){
            $even[$k] = new Complex($even[$k], 0);
        }

        if(!($odd[$k] instanceof Complex)){
            $odd[$k] = new Complex($odd[$k], 0);
        }
        $p = $k/$N;
        $t = new Complex(0, $a * $p);

        $t->ComplexExponential($t);
        $t->Multiply($odd[$k], $t);


        $amplitudes[$k] = $even[$k]->Add($t, $odd[$k]);
        $amplitudes[$k + $hN] = $even[$k]->Subtract($t, $even[$k]);
    }
    return $amplitudes;
}

function EchoSamples(&$samples){
    echo "Index\tReal\t\t\t\tImaginary" . PHP_EOL;
    foreach($samples as $key=>&$sample){
        echo  "$key\t" . number_format($sample->real, 13) . "\t\t\t\t" . number_format($sample->imaginary, 13) . PHP_EOL;
    }
}


// Input Amplitudes
$time_amplitude_samples = array(1,1,1,1,0,0,0,0);


// echo input for reference
echo 'Input '. PHP_EOL;
echo "Index\tReal" . PHP_EOL;
foreach($time_amplitude_samples as $key=>&$sample){
    echo  "$key\t" . number_format($sample, 13) . PHP_EOL;
}
echo PHP_EOL;

// Do FFT and echo results
echo 'FFT '. PHP_EOL;
$frequency_amplitude_samples = FFT($time_amplitude_samples);
EchoSamples($frequency_amplitude_samples);
echo PHP_EOL;

// Do inverse FFT and echo results
echo 'Inverse FFT '. PHP_EOL;
$frequency_back_to_time_amplitude_samples = IFFT($frequency_amplitude_samples);
EchoSamples($frequency_back_to_time_amplitude_samples);
echo PHP_EOL;


```



```txt

Input
Index   Real
0       1.0000000000000
1       1.0000000000000
2       1.0000000000000
3       1.0000000000000
4       0.0000000000000
5       0.0000000000000
6       0.0000000000000
7       0.0000000000000

FFT
Index   Real                            Imaginary
0       4.0000000000000                         0.0000000000000
1       1.0000000000000                         -2.4142135623731
2       0.0000000000000                         0.0000000000000
3       1.0000000000000                         -0.4142135623731
4       0.0000000000000                         0.0000000000000
5       1.0000000000000                         0.4142135623731
6       0.0000000000000                         0.0000000000000
7       1.0000000000000                         2.4142135623731

Inverse FFT
Index   Real                            Imaginary
0       1.0000000000000                         0.0000000000000
1       1.0000000000000                         0.0000000000000
2       1.0000000000000                         0.0000000000000
3       1.0000000000000                         0.0000000000000
4       0.0000000000000                         0.0000000000000
5       0.0000000000000                         0.0000000000000
6       0.0000000000000                         0.0000000000000
7       0.0000000000000                         0.0000000000000

```



## PicoLisp

```PicoLisp
# apt-get install libfftw3-dev

(scl 4)

(de FFTW_FORWARD . -1)
(de FFTW_ESTIMATE . 64)

(de fft (Lst)
   (let
      (Len (length Lst)
         In (native "libfftw3.so" "fftw_malloc" 'N (* Len 16))
         Out (native "libfftw3.so" "fftw_malloc" 'N (* Len 16))
         P (native "libfftw3.so" "fftw_plan_dft_1d" 'N
            Len In Out FFTW_FORWARD FFTW_ESTIMATE ) )
      (struct In NIL (cons 1.0 (apply append Lst)))
      (native "libfftw3.so" "fftw_execute" NIL P)
      (prog1 (struct Out (make (do Len (link (1.0 . 2)))))
         (native "libfftw3.so" "fftw_destroy_plan" NIL P)
         (native "libfftw3.so" "fftw_free" NIL Out)
         (native "libfftw3.so" "fftw_free" NIL In) ) ) )
```

Test:

```PicoLisp
(for R (fft '((1.0 0) (1.0 0) (1.0 0) (1.0 0) (0 0) (0 0) (0 0) (0 0)))
   (tab (6 8)
      (round (car R))
      (round (cadr R)) ) )
```

```txt
 4.000   0.000
 1.000  -2.414
 0.000   0.000
 1.000  -0.414
 0.000   0.000
 1.000   0.414
 0.000   0.000
 1.000   2.414
```



## PL/I


```PL/I
test: PROCEDURE OPTIONS (MAIN, REORDER); /* Derived from Fortran Rosetta Code */

  /* In-place Cooley-Tukey FFT */
FFT: PROCEDURE (x) RECURSIVE;
   DECLARE  x(*) COMPLEX FLOAT (18);
   DECLARE  t    COMPLEX FLOAT (18);
   DECLARE ( N, Half_N ) FIXED BINARY (31);
   DECLARE ( i, j ) FIXED BINARY (31);
   DECLARE (even(*), odd(*)) CONTROLLED COMPLEX FLOAT (18);
   DECLARE pi FLOAT (18) STATIC INITIAL ( 3.14159265358979323E0);

   N = HBOUND(x);

   if N <= 1 THEN return;

   allocate odd((N+1)/2), even(N/2);

    /* divide */
   do j = 1 to N by 2; odd((j+1)/2) = x(j); end;
   do j = 2 to N by 2; even(j/2)    = x(j); end;

    /* conquer */
   call fft(odd);
   call fft(even);

    /* combine */
   half_N = N/2;
   do i=1 TO half_N;
      t = exp(COMPLEX(0, -2*pi*(i-1)/N))*even(i);
      x(i)        = odd(i) + t;
      x(i+half_N) = odd(i) - t;
   end;

   FREE odd, even;

END fft;


   DECLARE data(8)  COMPLEX FLOAT (18) STATIC INITIAL (
                    1, 1, 1, 1, 0, 0, 0, 0);
   DECLARE ( i ) FIXED BINARY (31);

   call fft(data);

   do i=1 TO 8;
      PUT SKIP LIST ( fixed(data(i), 25, 12) );
   end;

END test;
```

```txt
    4.000000000000+0.000000000000I
    1.000000000000-2.414213562373I
    0.000000000000+0.000000000000I
    1.000000000000-0.414213562373I
    0.000000000000+0.000000000000I
    0.999999999999+0.414213562373I
    0.000000000000+0.000000000000I
    0.999999999999+2.414213562373I
```



## Prolog

{{trans|Python}}Note: Similar algorithmically to the python example.
```prolog
:- dynamic twiddles/2.
%_______________________________________________________________
% Arithemetic for complex numbers; only the needed rules
add(cx(R1,I1),cx(R2,I2),cx(R,I)) :- R is R1+R2, I is I1+I2.
sub(cx(R1,I1),cx(R2,I2),cx(R,I)) :- R is R1-R2, I is I1-I2.
mul(cx(R1,I1),cx(R2,I2),cx(R,I)) :- R is R1*R2-I1*I2, I is R1*I2+R2*I1.
polar_cx(Mag, Theta, cx(R, I)) :-     % Euler
	R is Mag * cos(Theta), I is Mag * sin(Theta).
%___________________________________________________
% FFT Implementation. Note: K rdiv N is a rational number,
% making the lookup in dynamic database predicate twiddles/2 very
% efficient.  Also, polar_cx/2 gets called only when necessary- in
% this case (N=8), exactly 3 times: (where Tf=1/4, 1/8, or 3/8).
tw(0,cx(1,0)) :- !.                    % Calculate e^(-2*pi*k/N)
tw(Tf, Cx) :- twiddles(Tf, Cx), !.     % dynamic match?
tw(Tf, Cx) :- polar_cx(1.0, -2*pi*Tf, Cx), assert(twiddles(Tf, Cx)).

fftVals(N, Even, Odd, V0, V1) :-       % solves all V0,V1 for N,Even,Odd
	nth0(K,Even,E), nth0(K,Odd,O), Tf is K rdiv N, tw(Tf,Cx),
	mul(Cx,O,M), add(E,M,V0), sub(E,M,V1).

split([],[],[]). % split [[a0,b0],[a1,b1],...] into [a0,a1,...] and [b0,b1,...]
split([[V0,V1]|T], [V0|T0], [V1|T1]) :- !, split(T, T0, T1).

fft([H], [H]).
fft([H|T], List) :-
	length([H|T],N),
	findall(Ve, (nth0(I,[H|T],Ve),I mod 2 =:= 0), EL), !, fft(EL, Even),
	findall(Vo, (nth0(I,T,Vo),I mod 2 =:= 0),OL), !, fft(OL, Odd),
	findall([V0,V1],fftVals(N,Even,Odd,V0,V1),FFTVals),    % calc FFT
	split(FFTVals,L0,L1), append(L0,L1,List).
%___________________________________________________
test :- D=[cx(1,0),cx(1,0),cx(1,0),cx(1,0),cx(0,0),cx(0,0),cx(0,0),cx(0,0)],
	time(fft(D,DRes)), writef('fft=['), P is 10^3, !,
	(member(cx(Ri,Ii), DRes), R is integer(Ri*P)/P, I is integer(Ii*P)/P,
	 write(R), (I>=0, write('+'),fail;write(I)), write('j, '),
	 fail; write(']'), nl).

```


```txt
 test.
% 681 inferences, 0.000 CPU in 0.001 seconds (0% CPU, Infinite Lips)
fft=[4+0j, 1-2.414j, 0+0j, 1-0.414j, 0+0j, 1+0.414j, 0+0j, 1+2.414j, ]
true.
```



## Python


### Python: Recursive


```python
from cmath import exp, pi

def fft(x):
    N = len(x)
    if N <= 1: return x
    even = fft(x[0::2])
    odd =  fft(x[1::2])
    T= [exp(-2j*pi*k/N)*odd[k] for k in range(N//2)]
    return [even[k] + T[k] for k in range(N//2)] + \
           [even[k] - T[k] for k in range(N//2)]

print( ' '.join("%5.3f" % abs(f)
                for f in fft([1.0, 1.0, 1.0, 1.0, 0.0, 0.0, 0.0, 0.0])) )
```


```txt
4.000 2.613 0.000 1.082 0.000 1.082 0.000 2.613
```


===Python: Using module [http://numpy.scipy.org/ numpy]===

```python>>>
 from numpy.fft import fft
>>> from numpy import array
>>> a = array([1.0, 1.0, 1.0, 1.0, 0.0, 0.0, 0.0, 0.0])
>>> print( ' '.join("%5.3f" % abs(f) for f in fft(a)) )
4.000 2.613 0.000 1.082 0.000 1.082 0.000 2.613
```



## R

The function "fft" is readily available in R

```R
fft(c(1,1,1,1,0,0,0,0))
```

```txt
4+0.000000i 1-2.414214i 0+0.000000i 1-0.414214i 0+0.000000i 1+0.414214i 0+0.000000i 1+2.414214i
```



## Racket


```racket

#lang racket
(require math)
(array-fft (array #[1. 1. 1. 1. 0. 0. 0. 0.]))

```


```txt

(fcarray
 #[4.0+0.0i
   1.0-2.414213562373095i
   0.0+0.0i
   1.0-0.4142135623730949i
   0.0+0.0i
   0.9999999999999999+0.4142135623730949i
   0.0+0.0i
   0.9999999999999997+2.414213562373095i])

```



## REXX

This REXX program is modeled after the   '''Run BASIC'''   version and is a   '''radix-2 DIC'''   (decimation-in-time)

form of the   '''Cooley-Turkey FFT'''    algorithm,   and as such, this simplified form assumes that the number of

data points is equal to an exact power of two.

Note that the REXX language doesn't have any higher math functions, such as the functions   '''COS'''   and   '''R2R'''

('''cos'''ine   and   reduce radians to a unit circle).

A normalization of radians function   ('''r2r''')   has been included here, as well as the constant   '''pi'''.

This REXX program also adds zero values   if   the number of data points in the list doesn't exactly equal to a

power of two.   This is known as   ''zero-padding''.

```rexx
/*REXX program performs a  fast Fourier transform  (FFT)  on a set of  complex numbers. */
numeric digits length( pi() )   -  length(.)     /*limited by the  PI  function result. */
arg data                                         /*ARG verb uppercases the DATA from CL.*/
if data=''  then data= 1 1 1 1 0                 /*Not specified?  Then use the default.*/
size=words(data);       pad= left('', 5)         /*PAD:  for indenting and padding SAYs.*/
  do p=0  until  2**p>=size         ;   end      /*number of args exactly a power of 2? */
  do j=size+1 to 2**p;  data= data 0;   end      /*add zeroes to DATA 'til a power of 2.*/
size= words(data);      ph= p % 2   ;   call hdr         /*╔═══════════════════════════╗*/
                        /* [↓] TRANSLATE allows I & J*/  /*║ Numbers in data can be in ║*/
         do j=0  for size                                /*║ seven formats:  real      ║*/
         _= translate( word(data, j+1), 'J', "I")        /*║                 real,imag ║*/
         parse  var  _    #.1.j  ''  $  1     "," #.2.j  /*║                     ,imag ║*/
         if $=='J'  then parse var #.1.j #2.j "J" #.1.j  /*║                      nnnJ ║*/
                                                         /*║                      nnnj ║*/
           do m=1  for  2;      #.m.j= word(#.m.j 0, 1)  /*║                      nnnI ║*/
           end   /*m*/          /*omitted part?  [↑] */  /*║                      nnni ║*/
                                                         /*╚═══════════════════════════╝*/
         say pad ' FFT   in '     center(j+1, 7)     pad    fmt(#.1.j)     fmt(#.2.j, "i")
         end     /*j*/
say
tran= pi()*2 / 2**p;     !.=0;    hp= 2**p %2;       A= 2**(p-ph);      ptr= A;     dbl= 1
say
         do p-ph;        halfPtr=ptr % 2
                     do i=halfPtr  by ptr  to A-halfPtr;  _= i - halfPtr;   !.i= !._ + dbl
                     end   /*i*/
         ptr= halfPtr;                     dbl= dbl + dbl
         end   /*p-ph*/

         do j=0  to 2**p%4;  cmp.j= cos(j*tran);      _= hp - j;            cmp._= -cmp.j
                                                      _= hp + j;            cmp._= -cmp.j
         end  /*j*/
B= 2**ph
         do i=0      for A;            q= i * B
             do j=0  for B;   h=q+j;   _= !.j*B+!.i;    if _<=h  then iterate
             parse value  #.1._  #.1.h  #.2._  #.2.h    with    #.1.h  #.1._  #.2.h  #.2._
             end   /*j*/                              /* [↑]  swap  two sets of values. */
         end       /*i*/
dbl= 1
         do p                    ;       w= hp % dbl
           do k=0   for dbl      ;      Lb= w * k            ;          Lh= Lb + 2**p % 4
             do j=0 for w        ;       a= j * dbl * 2 + k  ;           b=  a + dbl
             r= #.1.a;  i= #.2.a ;      c1= cmp.Lb * #.1.b   ;          c4= cmp.Lb * #.2.b
                                        c2= cmp.Lh * #.2.b   ;          c3= cmp.Lh * #.1.b
                                     #.1.a= r + c1 - c2      ;       #.2.a= i + c3 + c4
                                     #.1.b= r - c1 + c2      ;       #.2.b= i - c3 - c4
             end     /*j*/
           end       /*k*/
         dbl= dbl + dbl
         end         /*p*/
call hdr
         do z=0  for size
         say pad     " FFT  out "     center(z+1,7)    pad    fmt(#.1.z)    fmt(#.2.z,'j')
         end   /*z*/                             /*[↑] #s are shown with ≈20 dec. digits*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
cos: procedure; parse arg x;  q= r2r(x)**2;      z=1;    _=1;   p=1   /*bare bones COS. */
       do k=2  by 2;  _=-_*q/(k*(k-1));  z=z+_;  if z=p  then return z;   p=z;  end  /*k*/
/*──────────────────────────────────────────────────────────────────────────────────────*/
fmt: procedure; parse arg y,j;          y= y/1   /*prettifies complex numbers for output*/
     if abs(y) < '1e-'digits() %4  then y= 0;    if y=0 & j\==''  then return ''
     dp= digits()%3;  y= format(y, dp%6+1, dp);  if pos(.,y)\==0  then y= strip(y, 'T', 0)
     y=  strip(y, 'T', .);                       return left(y || j, dp)
/*──────────────────────────────────────────────────────────────────────────────────────*/
hdr: _=pad '   data      num' pad "  real─part  " pad pad '         imaginary─part       '
     say _;   say translate(_,  " "copies('═', 256),  " "xrange());                 return
/*──────────────────────────────────────────────────────────────────────────────────────*/
pi:  return 3.1415926535897932384626433832795028841971693993751058209749445923078164062862
r2r: return arg(1)  //  ( pi() * 2 )             /*reduce the radians to a unit circle. */
```

Programming note:   the numeric precision (decimal digits) is only restricted by the number of decimal digits in the

'''pi'''   variable   (which is defined in the penultimate assignment statement in the REXX program.


```txt

         data      num         real─part                        imaginary─part       
      ══════════   ═══       ═════════════               ════════════════════════════
       FFT   in     1              1
       FFT   in     2              1
       FFT   in     3              1
       FFT   in     4              1
       FFT   in     5              0
       FFT   in     6              0
       FFT   in     7              0
       FFT   in     8              0


         data      num         real─part                        imaginary─part       
      ══════════   ═══       ═════════════               ════════════════════════════
       FFT  out     1              4
       FFT  out     2              1                        -2.4142135623730950488
       FFT  out     3              0
       FFT  out     4              1                        -0.4142135623730950488
       FFT  out     5              0
       FFT  out     6              1                         0.4142135623730950488
       FFT  out     7              0
       FFT  out     8              1                         2.4142135623730950488

```



## Ruby


```ruby
def fft(vec)
  return vec if vec.size <= 1
  evens_odds = vec.partition.with_index{|_,i| i.even?}
  evens, odds = evens_odds.map{|even_odd| fft(even_odd)*2}
  evens.zip(odds).map.with_index do |(even, odd),i|
    even + odd * Math::E ** Complex(0, -2 * Math::PI * i / vec.size)
  end
end

fft([1,1,1,1,0,0,0,0]).each{|c| puts "%9.6f %+9.6fi" % c.rect}
```

```txt

 4.000000 +0.000000i
 1.000000 -2.414214i
-0.000000 -0.000000i
 1.000000 -0.414214i
 0.000000 -0.000000i
 1.000000 +0.414214i
 0.000000 -0.000000i
 1.000000 +2.414214i

```



## Run BASIC


```runbasic
cnt  = 8
sig  = int(log(cnt) /log(2) +0.9999)

pi    = 3.14159265
real1 = 2^sig

real  = real1 -1
real2 = int(real1 /  2)
real4 = int(real1 /  4)
real3 = real4 +real2

dim rel(real1)
dim img(real1)
dim cmp(real3)

for i = 0 to cnt -1
    read rel(i)
    read img(i)
next i

data    1,0, 1,0, 1,0, 1,0, 0,0, 0,0, 0,0, 0,0

sig2 = int(sig / 2)
sig1 = sig -sig2
cnt1 = 2^sig1
cnt2 = 2^sig2

dim v(cnt1 -1)
v(0) = 0
dv   = 1
ptr  = cnt1

for j = 1 to sig1
    hlfPtr = int(ptr / 2)
    pt     = cnt1 - hlfPtr
    for i = hlfPtr to pt step ptr
        v(i) = v(i -hlfPtr) + dv
    next i
    dv = dv + dv
    ptr = hlfPtr
next j

k = 2 *pi /real1

for x = 0 to real4
    cmp(x)         = cos(k *x)
    cmp(real2 - x) = 0 - cmp(x)
    cmp(real2 + x) = 0 - cmp(x)
next x

print "fft: bit reversal"

for i = 0 to cnt1 -1
    ip = i *cnt2
    for j = 0 to cnt2 -1
        h = ip +j
        g = v(j) *cnt2 +v(i)
        if g >h then
                temp   = rel(g)
                rel(g) = rel(h)
                rel(h) = temp
                temp   = img(g)
                img(g) = img(h)
                img(h) = temp
         end if
    next j
next i

t = 1
for stage = 1 to sig
    print "  stage:- "; stage
    d = int(real2 / t)
    for ii = 0 to t -1
        l   = d *ii
        ls  = l +real4
        for i = 0 to d -1
            a      = 2 *i *t +ii
            b      = a +t
            f1     = rel(a)
            f2     = img(a)
            cnt1   = cmp(l)  *rel(b)
            cnt2   = cmp(ls) *img(b)
            cnt3   = cmp(ls) *rel(b)
            cnt4   = cmp(l)  *img(b)
            rel(a) = f1 + cnt1 - cnt2
            img(a) = f2 + cnt3 + cnt4
            rel(b) = f1 - cnt1 + cnt2
            img(b) = f2 - cnt3 - cnt4
        next i
    next ii
    t = t +t
next stage

print "  Num   real   imag"
for i = 0 to real
    if abs(rel(i)) <10^-5 then rel(i) = 0
    if abs(img(i)) <10^-5 then img(i) = 0
    print "   "; i;"   ";using("##.#",rel(i));"    ";img(i)
next i
end
```


```txt
  Num   real   imag
   0    4.0    0
   1    1.0    -2.41421356
   2    0.0    0
   3    1.0    -0.414213565
   4    0.0    0
   5    1.0    0.414213562
   6    0.0    0
   7    1.0    2.41421356
```


## Rust

```rust
extern crate num;
use num::complex::Complex;
use std::f64::consts::PI;

const I: Complex<f64> = Complex { re: 0.0, im: 1.0 };

pub fn fft(input: &[Complex<f64>]) -> Vec<Complex<f64>> {
    fn fft_inner(
        buf_a: &mut [Complex<f64>],
        buf_b: &mut [Complex<f64>],
        n: usize,    // total length of the input array
        step: usize, // precalculated values for t
    ) {
        if step >= n {
            return;
        }

        fft_inner(buf_b, buf_a, n, step * 2);
        fft_inner(&mut buf_b[step..], &mut buf_a[step..], n, step * 2);
        // create a slice for each half of buf_a:
        let (left, right) = buf_a.split_at_mut(n / 2);

        for i in (0..n).step_by(step * 2) {
            let t = (-I * PI * (i as f64) / (n as f64)).exp() * buf_b[i + step];
            left[i / 2] = buf_b[i] + t;
            right[i / 2] = buf_b[i] - t;
        }
    }

    // round n (length) up to a power of 2:
    let n_orig = input.len();
    let n = n_orig.next_power_of_two();
    // copy the input into a buffer:
    let mut buf_a = input.to_vec();
    // right pad with zeros to a power of two:
    buf_a.append(&mut vec![Complex { re: 0.0, im: 0.0 }; n - n_orig]);
    // alternate between buf_a and buf_b to avoid allocating a new vector each time:
    let mut buf_b = buf_a.clone();
    fft_inner(&mut buf_a, &mut buf_b, n, 1);
    buf_a
}

fn show(label: &str, buf: &[Complex<f64>]) {
    println!("{}", label);
    let string = buf
        .into_iter()
        .map(|x| format!("{:.4}{:+.4}i", x.re, x.im))
        .collect::<Vec<_>>()
        .join(", ");
    println!("{}", string);
}

fn main() {
    let input: Vec<_> = [1.0, 1.0, 1.0, 1.0, 0.0, 0.0, 0.0, 0.0]
        .into_iter()
        .map(|x| Complex::from(x))
        .collect();
    show("input:", &input);
    let output = fft(&input);
    show("output:", &output);
}
```

```txt

input:
1.0000+0.0000i, 1.0000+0.0000i, 1.0000+0.0000i, 1.0000+0.0000i, 0.0000+0.0000i, 0.0000+0.0000i, 0.0000+0.0000i, 0.0000+0.0000i
output:
4.0000+0.0000i, 1.0000-2.4142i, 0.0000+0.0000i, 1.0000-0.4142i, 0.0000+0.0000i, 1.0000+0.4142i, 0.0000+0.0000i, 1.0000+2.4142i

```


## Scala

Imports and Complex arithmetic:

```Scala
import scala.math.{ Pi, cos, sin, cosh, sinh, abs }

case class Complex(re: Double, im: Double) {
    def +(x: Complex): Complex = Complex(re + x.re, im + x.im)
    def -(x: Complex): Complex = Complex(re - x.re, im - x.im)
    def *(x: Double):  Complex = Complex(re * x, im * x)
    def *(x: Complex): Complex = Complex(re * x.re - im * x.im, re * x.im + im * x.re)
    def /(x: Double):  Complex = Complex(re / x, im / x)

    override def toString(): String = {
        val a = "%1.3f" format re
        val b = "%1.3f" format abs(im)
        (a,b) match {
            case (_, "0.000") => a
            case ("0.000", _) => b + "i"
            case (_, _) if im > 0 => a + " + " + b + "i"
            case (_, _) => a + " - " + b + "i"
        }
    }
}

def exp(c: Complex) : Complex = {
    val r = (cosh(c.re) + sinh(c.re))
    Complex(cos(c.im), sin(c.im)) * r
}
```


The FFT definition itself:

```Scala
def _fft(cSeq: Seq[Complex], direction: Complex, scalar: Int): Seq[Complex] = {
    if (cSeq.length == 1) {
        return cSeq
    }
    val n = cSeq.length
    assume(n % 2 == 0, "The Cooley-Tukey FFT algorithm only works when the length of the input is even.")

    val evenOddPairs = cSeq.grouped(2).toSeq
    val evens = _fft(evenOddPairs map (_(0)), direction, scalar)
    val odds  = _fft(evenOddPairs map (_(1)), direction, scalar)

    def leftRightPair(k: Int): Pair[Complex, Complex] = {
        val base = evens(k) / scalar
        val offset = exp(direction * (Pi * k / n)) * odds(k) / scalar
        (base + offset, base - offset)
    }

    val pairs = (0 until n/2) map leftRightPair
    val left  = pairs map (_._1)
    val right = pairs map (_._2)
    left ++ right
}

def  fft(cSeq: Seq[Complex]): Seq[Complex] = _fft(cSeq, Complex(0,  2), 1)
def rfft(cSeq: Seq[Complex]): Seq[Complex] = _fft(cSeq, Complex(0, -2), 2)
```


Usage:

```Scala
val data = Seq(Complex(1,0), Complex(1,0), Complex(1,0), Complex(1,0),
               Complex(0,0), Complex(0,2), Complex(0,0), Complex(0,0))

println(fft(data))
println(rfft(fft(data)))
```


```txt
Vector(4.000 + 2.000i, 2.414 + 1.000i, -2.000, 2.414 + 1.828i, 2.000i, -0.414 + 1.000i, 2.000, -0.414 - 3.828i)
Vector(1.000, 1.000, 1.000, 1.000, 0.000, 2.000i, 0.000, 0.000)
```



## Scilab


Scilab has a builtin FFT function.


```Scilab
fft([1,1,1,1,0,0,0,0]')
```



## Sidef

```ruby
func fft(arr) {
    arr.len == 1 && return arr

    var evn = fft([arr[^arr -> grep { .is_even }]])
    var odd = fft([arr[^arr -> grep { .is_odd  }]])
    var twd = (Num.tau.i / arr.len)

    ^odd -> map {|n| odd[n] *= ::exp(twd * n)}
    (evn »+« odd) + (evn »-« odd)
}

var cycles = 3
var sequence = 0..15
var wave = sequence.map {|n| ::sin(n * Num.tau / sequence.len * cycles) }
say "wave:#{wave.map{|w| '%6.3f' % w }.join(' ')}"
say "fft: #{fft(wave).map { '%6.3f' % .abs }.join(' ')}"
```

```txt

wave: 0.000  0.924  0.707 -0.383 -1.000 -0.383  0.707  0.924  0.000 -0.924 -0.707  0.383  1.000  0.383 -0.707 -0.924
fft:  0.000  0.000  0.000  8.000  0.000  0.000  0.000  0.000  0.000  0.000  0.000  0.000  0.000  8.000  0.000  0.000

```



## SequenceL


```sequencel>import <Utilities/Complex.sl
;
import <Utilities/Math.sl>;
import <Utilities/Sequence.sl>;

fft(x(1)) :=
    let
        n := size(x);

        top := fft(x[range(1,n-1,2)]);
        bottom := fft(x[range(2,n,2)]);

        d[i] := makeComplex(cos(2.0*pi*i/n), -sin(2.0*pi*i/n)) foreach i within 0...(n / 2 - 1);

        z := complexMultiply(d, bottom);
    in
        x when n <= 1
    else
        complexAdd(top,z) ++ complexSubtract(top,z);
```


<pre style="overflow: scroll">
cmd:>fft(makeComplex([1,1,1,1,0,0,0,0],0))
[(Imaginary:0.00000000,Real:4.00000000),(Imaginary:-2.41421356,Real:1.00000000),(Imaginary:0.00000000,Real:0.00000000),(Imaginary:-0.41421356,Real:1.00000000),(Imaginary:0.00000000,Real:0.00000000),(Imaginary:0.41421356,Real:1.00000000),(Imaginary:0.00000000,Real:0.00000000),(Imaginary:2.41421356,Real:1.00000000)]

```



## Stata



###  Mata

See the '''[https://www.stata.com/help.cgi?mf_fft fft function]''' in Mata help, and in the FAQ: [https://www.stata.com/support/faqs/mata/discrete-fast-fourier-transform/ How can I calculate the Fourier coefficients of a discretely sampled function in Stata?].

<lang>. mata
: a=1,2,3,4
: fft(a)
             1         2         3         4
    +-----------------------------------------+
  1 |       10   -2 - 2i        -2   -2 + 2i  |
    +-----------------------------------------+
: end
```



###  fft command

Stata can also compute FFT using the undocumented '''fft''' command. Here is an example showing its syntax. A time variable must have been set prior to calling this command. Notice that in order to get the same result as Mata's fft() function, in both the input and the output variables the imaginary part must be passed '''first'''.


```stata
clear
set obs 4
gen t=_n
gen x=_n
gen y=0
tsset t
fft y x, gen(v u)
list u v, noobs
```


'''Output'''


```txt
  +-----------------+
  |  u            v |
  |-----------------|
  | 10            0 |
  | -2           -2 |
  | -2   -2.449e-16 |
  | -2            2 |
  +-----------------+
```



## SystemVerilog

Differently from the java implementation I have not implemented a complex type. I think it would worth only if the simulators supported operator overloading, since it is not the case I prefer to expand the complex operations, that are trivial for any electrical engineer to understand :D

I could have written a more beautiful code by using non-blocking assignments in the bit_reverse_order function, but it could not be coded in a function, so FFT could not be implemented as a function as well.


```SystemVerilog


package math_pkg;
  // Inspired by the post
  // https://community.cadence.com/cadence_blogs_8/b/fv/posts/create-a-sine-wave-generator-using-systemverilog
  // import functions directly from C library
  //import dpi task      C Name = SV function name
  import "DPI" pure function real cos (input real rTheta);
  import "DPI" pure function real sin(input real y);
  import "DPI" pure function real atan2(input real y, input real x);
endpackage : math_pkg


// Encapsulates the functions in a parameterized class
// The FFT is implemented using floating point arithmetic (systemverilog real)
// Complex values are represented as a real vector [1:0], the index 0 is the real part
// and the index 1 is the imaginary part.
class fft_fp #(
  parameter LOG2_NS = 7,
  parameter NS = 1<<LOG2_NS
);


  static function void bit_reverse_order(input real buffer_in[0:NS-1][1:0], output real buffer[0:NS-1][1:0]);
  begin
    for(reg [LOG2_NS:0] j = 0; j < NS; j = j + 1) begin
      reg [LOG2_NS-1:0] ij;
      ij = {<<{j[LOG2_NS-1:0]}}; // Right to left streaming
      buffer[j][0] = buffer_in[ij][0];
      buffer[j][1] = buffer_in[ij][1];
    end
  end
  endfunction
  // SystemVerilog FFT implementation translated from Java
  static function void transform(input real buffer_in[0:NS-1][1:0], output real buffer[0:NS-1][1:0]);
  begin
    static real pi = math_pkg::atan2(0.0, -1.0);
    bit_reverse_order(buffer_in, buffer);
    for(int N = 2; N <= NS; N = N << 1) begin
      for(int i = 0; i < NS; i = i + N) begin
        for(int k =0; k < N/2; k = k + 1) begin
          int evenIndex;
          int oddIndex;
          real theta;
          real wr, wi;
          real zr, zi;
          evenIndex = i + k;
          oddIndex  = i + k + (N/2);
          theta     = (-2.0*pi*k/real'(N));
          // Call to the DPI C functions
          // (it could be memorized to save some calls but I dont think it worthes)
          // w = exp(-2j*pi*k/N);
          wr = math_pkg::cos(theta);
          wi = math_pkg::sin(theta);
          // x = w * buffer[oddIndex]
          zr = buffer[oddIndex][0] * wr - buffer[oddIndex][1] * wi;
          zi = buffer[oddIndex][0] * wi + buffer[oddIndex][1] * wr;
          // update oddIndex before evenIndex
          buffer[ oddIndex][0] = buffer[evenIndex][0] - zr;
          buffer[ oddIndex][1] = buffer[evenIndex][1] - zi;
          // because evenIndex is in the rhs
          buffer[evenIndex][0] = buffer[evenIndex][0] + zr;
          buffer[evenIndex][1] = buffer[evenIndex][1] + zi;
        end
      end
    end
  end
  endfunction
  // Implements the inverse FFT using the following identity
  // ifft(x) = conj(fft(conj(x))/NS;
  static function void invert(input real buffer_in[0:NS-1][1:0], output real buffer[0:NS-1][1:0]);
    real tmp[0:NS-1][1:0];
  begin
    // Conjugates the input
    for(int i = 0; i < NS; i = i + 1) begin
      tmp[i][0] = buffer_in[i][0];
      tmp[i][1] = -buffer_in[i][1];
    end
    transform(tmp, buffer);
    // Conjugate and scale the output
    for(int i = 0; i < NS; i = i + 1) begin
      buffer[i][0] = buffer[i][0]/NS;
      buffer[i][1] = -buffer[i][1]/NS;
    end
  end
  endfunction

endclass

```


Now let's perform the standard test

```SystemVerilog

/// @Author: Alexandre Felipe (o.alexandre.felipe@gmail.com)
/// @Date: 2018-Jan-25
///
module fft_model_sanity;
  initial begin
    real x[0:7][1:0]; // input data
    real X[0:7][1:0]; // transformed data
    real y[0:7][1:0]; // inverted data
    for(int i = 0; i < 8; i = i + 1)x[i][0] = 0.0;
    for(int i = 4; i < 8; i = i + 1)x[i][1] = 0.0;
    for(int i = 0; i < 4; i = i + 1)x[i][0] = 1.0;
    fft_fp #(.LOG2_NS(3), .NS(8))::transform(x, X);
    $display("Direct FFT");
    for(int i = 0; i < 8; i = i + 1) begin
      $display("(%f, %f)", X[i][0], X[i][1]);
    end
    $display("Inverse FFT");
    fft_fp #(.LOG2_NS(3), .NS(8))::invert(X, y);
    for(int i = 0; i < 8; i = i + 1) begin
      $display("(%f, %f)", y[i][0], y[i][1]);
    end
  end
endmodule

```

By running the sanity test it outputs the following

```txt

Direct FFT
(4.000000, 0.000000)
(1.000000, -2.414214)
(0.000000, 0.000000)
(1.000000, -0.414214)
(0.000000, 0.000000)
(1.000000, 0.414214)
(0.000000, 0.000000)
(1.000000, 2.414214)
Inverse FFT
(1.000000, 0.000000)
(1.000000, -0.000000)
(1.000000, 0.000000)
(1.000000, -0.000000)
(0.000000, 0.000000)
(0.000000, 0.000000)
(0.000000, -0.000000)
(0.000000, 0.000000)

```

Giving some indication that the test is correct.

A more reliable test is to implement the Discrete Fourier Transform by its definition and compare the results obtained by FFT and by definition evaluation. For that let's create a class with a random data vector, and each time the vector is randomized the FFT is calculated and the output is compared by the result obtained by the definition.


```SystemVerilog

/// @Author: Alexandre Felipe (o.alexandre.felipe@gmail.com)
/// @Date: 2018-Jan-25
///
class fft_definition_checker #(
  parameter LOG2_NS = 3,
  parameter NS = 1<<LOG2_NS,
  parameter NB = 10);
    rand logic [NB:0] x_bits[0:NS-1][1:0];
    static real TWO_PI = 2.0*math_pkg::atan2(0.0, -1.0);
    real w[0:NS-1][1:0];
    function new;
      foreach(w[i]) begin
        w[i][0] = math_pkg::cos(TWO_PI * i / real'(NS));
        w[i][1] =-math_pkg::sin(TWO_PI * i / real'(NS));
      end
    endfunction
    function void post_randomize;
       real x[0:NS-1][1:0];
       real X[0:NS-1][1:0];
       real X_ref[0:NS-1][1:0];
       real errorEnergy;
    begin
      // Convert randomized binary numbers to real (floating point)
      foreach(x_bits[i]) begin
        x[i][0] = x_bits[i][0];
        x[i][1] = x_bits[i][1];
      end

      ////               START THE MAGIC HERE           ////
      fft_fp #(.LOG2_NS(LOG2_NS), .NS(NS))::transform(x, X);
      ////                 END OF THE MAGIC            ////


      /// Calculate X_ref, the discrete Fourier transform by the definition ///
      foreach(X_ref[k]) begin
        X_ref[k] = '{0.0, 0.0};
        foreach(x[i]) begin
          X_ref[k][0] = X_ref[k][0] + x[i][0] * w[(i*k) % NS][0] - x[i][1] * w[(i*k) % NS][1];
          X_ref[k][1] = X_ref[k][1] + x[i][0] * w[(i*k) % NS][1] + x[i][1] * w[(i*k) % NS][0];
        end
      end

      // Measure the error
      errorEnergy = 0.0;
      foreach(X[k]) begin
        errorEnergy = errorEnergy + (X_ref[k][0] - X[k][0]) * (X_ref[k][0] - X[k][0]);
        errorEnergy = errorEnergy + (X_ref[k][1] - X[k][1]) * (X_ref[k][1] - X[k][1]);
      end
      $display("FFT of %d integers %d bits (error @ %g)", NS, NB, errorEnergy / real'(NS));
    end
    endfunction
endclass

```


Now let's create a code that tests the FFT with random inputs for different sizes.
Uses a generate block since the number of samples is a parameter and must be defined at compile time.

```SystemVerilog

/// @Author: Alexandre Felipe (o.alexandre.felipe@gmail.com)
/// @Date: 2018-Jan-25
///
module fft_test_by_definition;
  genvar LOG2_NS;
  generate for(LOG2_NS = 3; LOG2_NS < 7; LOG2_NS = LOG2_NS + 1) begin
    initial begin
      fft_definition_checker #(.NB(10), .LOG2_NS(LOG2_NS), .NS(1<<LOG2_NS)) chkInst;
      chkInst = new;
      repeat(5) assert(chkInst.randomize()); // randomize and check the outputs
    end
  end
  endgenerate
endmodule

```


Simulating the fft_test_by_definition we get the following output:

```txt

FFT of           8 integers          10 bits (error @ 3.11808e-25)
FFT of           8 integers          10 bits (error @ 7.86791e-25)
FFT of           8 integers          10 bits (error @ 7.26776e-25)
FFT of           8 integers          10 bits (error @ 2.75458e-25)
FFT of           8 integers          10 bits (error @ 4.83061e-25)
FFT of          16 integers          10 bits (error @ 1.73615e-24)
FFT of          16 integers          10 bits (error @ 3.00742e-24)
FFT of          16 integers          10 bits (error @ 1.70818e-24)
FFT of          16 integers          10 bits (error @ 2.47367e-24)
FFT of          16 integers          10 bits (error @ 2.13661e-24)
FFT of          32 integers          10 bits (error @ 9.52803e-24)
FFT of          32 integers          10 bits (error @ 1.19533e-23)
FFT of          32 integers          10 bits (error @ 6.50223e-24)
FFT of          32 integers          10 bits (error @ 8.05807e-24)
FFT of          32 integers          10 bits (error @ 7.07355e-24)
FFT of          64 integers          10 bits (error @ 3.54266e-23)
FFT of          64 integers          10 bits (error @ 2.952e-23)
FFT of          64 integers          10 bits (error @ 3.41618e-23)
FFT of          64 integers          10 bits (error @ 3.66977e-23)
FFT of          64 integers          10 bits (error @ 3.4069e-23)

```

As expected the error is small and it increases with the number of terms in the FFT.


## Tcl

```tcl
package require math::constants
package require math::fourier

math::constants::constants pi
# Helper functions
proc wave {samples cycles} {
    global pi
    set wave {}
    set factor [expr {2*$pi * $cycles / $samples}]
    for {set i 0} {$i < $samples} {incr i} {
	lappend wave [expr {sin($factor * $i)}]
    }
    return $wave
}
proc printwave {waveName {format "%7.3f"}} {
    upvar 1 $waveName wave
    set out [format "%-6s" ${waveName}:]
    foreach value $wave {
	append out [format $format $value]
    }
    puts $out
}
proc waveMagnitude {wave} {
    set out {}
    foreach value $wave {
	lassign $value re im
	lappend out [expr {hypot($re, $im)}]
    }
    return $out
}

set wave [wave 16 3]
printwave wave
# Uses FFT if input length is power of 2, and a less efficient algorithm otherwise
set fft [math::fourier::dft $wave]
# Convert to magnitudes for printing
set fft2 [waveMagnitude $fft]
printwave fft2
```

```txt

wave:   0.000  0.924  0.707 -0.383 -1.000 -0.383  0.707  0.924  0.000 -0.924 -0.707  0.383  1.000  0.383 -0.707 -0.924
fft2:   0.000  0.000  0.000  8.000  0.000  0.000  0.000  0.000  0.000  0.000  0.000  0.000  0.000  8.000  0.000  0.000

```



## Ursala

The [http://www.fftw.org <code>fftw</code> library] is callable from Ursala using the syntax <code>..u_fw_dft</code> for a one dimensional forward discrete Fourier transform operating on a list of complex numbers. Ordinarily the results are scaled so that the forward and reverse transforms are inverses of each other, but additional scaling can be performed as shown below to conform to convention.

```ursala
#import nat
#import flo

f = <1+0j,1+0j,1+0j,1+0j,0+0j,0+0j,0+0j,0+0j>    # complex sequence of 4 1's and 4 0's

g = c..mul^*D(sqrt+ float+ length,..u_fw_dft) f  # its fft

#cast %jLW

t = (f,g)
```

```txt
(
   <
      1.000e+00+0.000e+00j,
      1.000e+00+0.000e+00j,
      1.000e+00+0.000e+00j,
      1.000e+00+0.000e+00j,
      0.000e+00+0.000e+00j,
      0.000e+00+0.000e+00j,
      0.000e+00+0.000e+00j,
      0.000e+00+0.000e+00j>,
   <
      4.000e+00+0.000e+00j,
      1.000e+00-2.414e+00j,
      0.000e+00+0.000e+00j,
      1.000e+00-4.142e-01j,
      0.000e+00+0.000e+00j,
      1.000e+00+4.142e-01j,
      0.000e+00+0.000e+00j,
      1.000e+00+2.414e+00j>)
```



## zkl


```zkl
var [const] GSL=Import("zklGSL");	// libGSL (GNU Scientific Library)
v:=GSL.ZVector(8).set(1,1,1,1);
GSL.FFT(v).toList().concat("\n").println();  // in place
```

```txt

(4.00+0.00i)
(1.00-2.41i)
(0.00+0.00i)
(1.00-0.41i)
(0.00+0.00i)
(1.00+0.41i)
(0.00+0.00i)
(1.00+2.41i)

```

