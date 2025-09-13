+++
title = "Test integerness"
description = ""
date = 2018-05-09T18:53:20Z
aliases = []
[extra]
id = 17719
[taxonomies]
categories = ["task"]
tags = []
+++

Mathematically,
* the [[wp:Integer|integers]] '''Z''' are included in the [[wp:Rational number|rational numbers]] '''Q''',
* which are included in the [[wp:Real number|real numbers]] '''R''',
* which can be generalized to the [[wp:Complex number|complex numbers]] '''C'''.


This means that each of those larger sets, and the data types used to represent them, include some integers.

Given a rational, real, or complex number of any type, test whether it is mathematically an integer.

Your code should handle all numeric data types commonly used in your programming language.

Discuss any limitations of your code.

For the purposes of this task, integerness means that a number could theoretically be represented as an integer at no loss of precision ''<small>(given an infinitely wide integer type)</small>''.

In other words:

{| class="wikitable"
|-
! Set
! Common representation
! C++ type
! Considered an integer...
|-
| rational numbers '''Q'''
| [[wp:Rational data type|fraction]]
| <code>std::ratio</code>
| ...if its denominator is 1 (in reduced form)
|-
| rowspan=2 | real numbers '''Z'''
<small>''(approximated)''</small>
| [[wp:Fixed-point arithmetic|fixed-point]]
|
| ...if it has no non-zero digits after the decimal point
|-
| [[wp:Floating point|floating-point]]
| <code>float</code>, <code>double</code>
| ...if the number of significant decimal places of its mantissa isn't greater than its exponent
|-
| complex numbers '''C'''
| [[wp:Complex data type|pair of real numbers]]
| <code>std::complex</code>
| ...if its real part is considered an integer and its imaginary part is zero
|}

Optionally, make your code accept a <code>tolerance</code> parameter for fuzzy testing. The tolerance is the maximum amount by which the number may differ from the nearest integer, to still be considered an integer.

This is useful in practice, because when dealing with approximate numeric types (such as floating point), there may already be [[wp:Round-off error|round-off errors]] from previous calculations. For example, a float value of <code>0.9999999998</code> might actually be intended to represent the integer <code>1</code>.

{| class="wikitable"
|-
! colspan=2 | Input
! colspan=2 | Output
! rowspan=2 | Comment
|-
! <small>Type</small>
! <small>Value</small>
! <small><tt>exact</tt></small>
! <small><tt>tolerance = 0.00001</tt></small>
|-
| rowspan=3 | decimal
| <code>25.000000</code>
| colspan=2 | true
|
|-
| <code>24.999999</code>
| false
| true
|
|-
| <code>25.000100</code>
| colspan=2 | false
|
|-
| rowspan=4 | floating-point
| <code>-2.1e120</code>
| colspan=2 | true
| This one is tricky, because in most languages it is too large to fit into a native integer type.
It is, nonetheless, mathematically an integer, and your code should identify it as such.
|-
| <code>-5e-2</code>
| colspan=2 | false
|
|-
| <code>NaN</code>
| colspan=2 | false
|
|-
| <code>Inf</code>
| colspan=2 | false
| This one is debatable. If your code considers it an integer, that's okay too.
|-
| rowspan=2 | complex
| <code>5.0+0.0i</code>
| colspan=2 | true
|
|-
| <code>5-5i</code>
| colspan=2 | false
|
|}

(The types and notations shown in these tables are merely examples &ndash; you should use the native data types and number literals of your programming language and standard library. Use a different set of test-cases, if this one doesn't demonstrate all relevant behavior.)

<hr>


## ALGOL 68

Uses LONG LONG values which in Algol 68 have a programmer specifiable number of digits. As with the C version, we need only handle the complex case directly, as Algol 68 will automatically coerce integer and real values to complex as required.

```algol68
# set the required precision of LONG LONG values using    #
# "PR precision n PR" if required                         #
PR precision 24 PR

# returns TRUE if v has an integer value, FALSE otherwise #
OP ISINT = ( LONG LONG COMPL v )BOOL:
   IF im OF v /= 0 THEN
       # v has an imaginary part #
       FALSE
   ELSE
       # v has a real part only #
       ENTIER re OF v = v
   FI; # ISINT #

# test ISINT #

PROC test is int = ( LONG LONG COMPLEX v )VOID:
     print( ( re OF v, "_", im OF v, IF ISINT v THEN " is " ELSE " is not " FI, "integral", newline ) );


test is int( 1 );
test is int( 1.00000001 );
test is int( 4 I 3 );
test is int( 4.0 I 0 );
test is int( 123456789012345678901234 )

```

```txt

+1.0000000000000000000000000000000000e  +0_+0.0000000000000000000000000000000000e  +0 is integral
+1.0000000100000000000000000000000000e  +0_+0.0000000000000000000000000000000000e  +0 is not integral
+4.0000000000000000000000000000000000e  +0_+3.0000000000000000000000000000000000e  +0 is not integral
+4.0000000000000000000000000000000000e  +0_+0.0000000000000000000000000000000000e  +0 is integral
+1.2345678901234567890123400000000000e +23_+0.0000000000000000000000000000000000e  +0 is integral

```



## C

The main function that checks a numeric value is actually quite short. Because of C's weak types and implicit casting we can get away with making a function which checks long double complex types only.


```c

#include <stdio.h>
#include <complex.h>
#include <math.h>

/* Testing macros */
#define FMTSPEC(arg) _Generic((arg), \
    float: "%f", double: "%f", \
    long double: "%Lf", unsigned int: "%u", \
    unsigned long: "%lu", unsigned long long: "%llu", \
    int: "%d", long: "%ld", long long: "%lld", \
    default: "(invalid type (%p)")

#define CMPPARTS(x, y) ((long double complex)((long double)(x) + \
            I * (long double)(y)))

#define TEST_CMPL(i, j)\
    printf(FMTSPEC(i), i), printf(" + "), printf(FMTSPEC(j), j), \
    printf("i = %s\n", (isint(CMPPARTS(i, j)) ? "true" : "false"))

#define TEST_REAL(i)\
    printf(FMTSPEC(i), i), printf(" = %s\n", (isint(i) ? "true" : "false"))

/* Main code */
static inline int isint(long double complex n)
{
    return cimagl(n) == 0 && nearbyintl(creall(n)) == creall(n);
}

int main(void)
{
    TEST_REAL(0);
    TEST_REAL(-0);
    TEST_REAL(-2);
    TEST_REAL(-2.00000000000001);
    TEST_REAL(5);
    TEST_REAL(7.3333333333333);
    TEST_REAL(3.141592653589);
    TEST_REAL(-9.223372036854776e18);
    TEST_REAL(5e-324);
    TEST_REAL(NAN);
    TEST_CMPL(6, 0);
    TEST_CMPL(0, 1);
    TEST_CMPL(0, 0);
    TEST_CMPL(3.4, 0);

    /* Demonstrating that we can use the same function for complex values
     * constructed in the standard way */
    double complex test1 = 5 + 0*I,
                   test2 = 3.4f,
                   test3 = 3,
                   test4 = 0 + 1.2*I;

    printf("Test 1 (5+i) = %s\n", isint(test1) ? "true" : "false");
    printf("Test 2 (3.4+0i) = %s\n", isint(test2) ? "true" : "false");
    printf("Test 3 (3+0i) = %s\n", isint(test3) ? "true" : "false");
    printf("Test 4 (0+1.2i) = %s\n", isint(test4) ? "true" : "false");
}

```


Note: Some of the printed results are truncated and look incorrect. See the actual code if you wish to verify the actual value.

```txt

0 = true
0 = true
-2 = true
-2.000000 = false
5 = true
7.333333 = false
3.141593 = false
-9223372036854775808.000000 = true
0.000000 = false
nan = false
6 + 0i = true
0 + 1i = false
0 + 0i = true
3.400000 + 0i = false
Test 1 (5+i) = true
Test 2 (3.4+0i) = false
Test 3 (3+0i) = true
Test 4 (0+1.2i) = false

```



## C++


```cpp

#include <complex>
#include <math.h>
#include <iostream>

template<class Type>
struct Precision
{
public:
	static Type GetEps()
	{
		return eps;
	}

	static void SetEps(Type e)
	{
		eps = e;
	}

private:
	static Type eps;
};

template<class Type> Type Precision<Type>::eps = static_cast<Type>(1E-7);

template<class DigType>
bool IsDoubleEqual(DigType d1, DigType d2)
{
	return (fabs(d1 - d2) < Precision<DigType>::GetEps());
}

template<class DigType>
DigType IntegerPart(DigType value)
{
	return (value > 0) ? floor(value) : ceil(value);
}

template<class DigType>
DigType FractionPart(DigType value)
{
	return fabs(IntegerPart<DigType>(value) - value);
}

template<class Type>
bool IsInteger(const Type& value)
{
	return false;
}

#define GEN_CHECK_INTEGER(type)			\
template<>					\
bool IsInteger<type>(const type& value)         \
{						\
	return true;				\
}

#define GEN_CHECK_CMPL_INTEGER(type)					\
template<>								\
bool IsInteger<std::complex<type> >(const std::complex<type>& value)	\
{									\
	type zero = type();						\
	return value.imag() == zero;					\
}

#define GEN_CHECK_REAL(type)						\
template<>								\
bool IsInteger<type>(const type& value)					\
{									\
	type zero = type();						\
	return IsDoubleEqual<type>(FractionPart<type>(value), zero);	\
}

#define GEN_CHECK_CMPL_REAL(type)					\
template<>								\
bool IsInteger<std::complex<type> >(const std::complex<type>& value)	\
{									\
	type zero = type();						\
	return IsDoubleEqual<type>(value.imag(), zero);			\
}

#define GEN_INTEGER(type)		\
	GEN_CHECK_INTEGER(type)		\
	GEN_CHECK_CMPL_INTEGER(type)

#define GEN_REAL(type)			\
	GEN_CHECK_REAL(type)		\
	GEN_CHECK_CMPL_REAL(type)


GEN_INTEGER(char)
GEN_INTEGER(unsigned char)
GEN_INTEGER(short)
GEN_INTEGER(unsigned short)
GEN_INTEGER(int)
GEN_INTEGER(unsigned int)
GEN_INTEGER(long)
GEN_INTEGER(unsigned long)
GEN_INTEGER(long long)
GEN_INTEGER(unsigned long long)

GEN_REAL(float)
GEN_REAL(double)
GEN_REAL(long double)

template<class Type>
inline void TestValue(const Type& value)
{
	std::cout << "Value: " << value << " of type: " << typeid(Type).name() << " is integer - " << std::boolalpha << IsInteger(value) << std::endl;
}

int main()
{
	char c = -100;
	unsigned char uc = 200;
	short s = c;
	unsigned short us = uc;
	int i = s;
	unsigned int ui = us;
	long long ll = i;
	unsigned long long ull = ui;

	std::complex<unsigned int> ci1(2, 0);
	std::complex<int> ci2(2, 4);
	std::complex<int> ci3(-2, 4);
	std::complex<unsigned short> cs1(2, 0);
	std::complex<short> cs2(2, 4);
	std::complex<short> cs3(-2, 4);

	std::complex<double> cd1(2, 0);
	std::complex<float> cf1(2, 4);
	std::complex<double> cd2(-2, 4);

	float f1 = 1.0;
	float f2 = -2.0;
	float f3 = -2.4f;
	float f4 = 1.23e-5f;
	float f5 = 1.23e-10f;
	double d1 = f5;

	TestValue(c);
	TestValue(uc);
	TestValue(s);
	TestValue(us);
	TestValue(i);
	TestValue(ui);
	TestValue(ll);
	TestValue(ull);

	TestValue(ci1);
	TestValue(ci2);
	TestValue(ci3);
	TestValue(cs1);
	TestValue(cs2);
	TestValue(cs3);

	TestValue(cd1);
	TestValue(cd2);
	TestValue(cf1);

	TestValue(f1);
	TestValue(f2);
	TestValue(f3);
	TestValue(f4);
	TestValue(f5);
	std::cout << "Set float precision: 1e-15f\n";
	Precision<float>::SetEps(1e-15f);
	TestValue(f5);
	TestValue(d1);
	return 0;
}

```


```txt

Value: Ь of type: char is integer - true
Value: ╚ of type: unsigned char is integer - true
Value: -100 of type: short is integer - true
Value: 200 of type: unsigned short is integer - true
Value: -100 of type: int is integer - true
Value: 200 of type: unsigned int is integer - true
Value: -100 of type: __int64 is integer - true
Value: 200 of type: unsigned __int64 is integer - true
Value: (2,0) of type: class std::complex<unsigned int> is integer - true
Value: (2,4) of type: class std::complex<int> is integer - false
Value: (-2,4) of type: class std::complex<int> is integer - false
Value: (2,0) of type: class std::complex<unsigned short> is integer - true
Value: (2,4) of type: class std::complex<short> is integer - false
Value: (-2,4) of type: class std::complex<short> is integer - false
Value: (2,0) of type: class std::complex<double> is integer - true
Value: (-2,4) of type: class std::complex<double> is integer - false
Value: (2,4) of type: class std::complex<float> is integer - false
Value: 1 of type: float is integer - true
Value: -2 of type: float is integer - true
Value: -2.4 of type: float is integer - false
Value: 1.23e-05 of type: float is integer - false
Value: 1.23e-10 of type: float is integer - true
Set float precision: 1e-15f
Value: 1.23e-10 of type: float is integer - false
Value: 1.23e-10 of type: double is integer - true

```



## C#



Length and precision of entered numbers in this solution,
are limited by the limitations of variables type of [https://en.wikipedia.org/wiki/Double-precision_floating-point_format Double].



```c sharp

namespace Test_integerness
{
	class Program
	{
		public static void Main(string[] args)
		{
			Console.Clear();
			Console.WriteLine();
			Console.WriteLine(" ***************************************************");
			Console.WriteLine(" *                                                 *");
			Console.WriteLine(" *              Integerness test                   *");
			Console.WriteLine(" *                                                 *");
			Console.WriteLine(" ***************************************************");
			Console.WriteLine();

			ConsoleKeyInfo key = new ConsoleKeyInfo('Y',ConsoleKey.Y,true,true,true);

			while(key.Key == ConsoleKey.Y)
			{
				// Get number value from keyboard
				Console.Write(" Enter number value : ");

				string LINE = Console.ReadLine();

				// Get tolerance value from keyboard
				Console.Write(" Enter tolerance value : ");

				double TOLERANCE = double.Parse(Console.ReadLine());


				// Resolve entered number format and set NUMBER value
				double NUMBER = 0;

				string [] N;

				// Real number value
				if(!double.TryParse(LINE, out NUMBER))
				{
					// Rational number value
					if(LINE.Contains("/"))
					{
						N = LINE.Split('/');

						NUMBER = double.Parse(N[0]) / double.Parse(N[1]);
					}
					// Inf value
					else if(LINE.ToUpper().Contains("INF"))
					{
						NUMBER = double.PositiveInfinity;
					}
					// Complex value
					else if(LINE.ToUpper().Contains("I"))
					{
						// Delete letter i
						LINE = LINE.ToUpper().Replace("I","");

						string r = string.Empty; // real part
						string i = string.Empty; // imaginary part

						int s = 1; // sign offset

						// Get sign
						if(LINE[0]=='+' || LINE[0]=='-')
						{
							r+=LINE[0].ToString();
							LINE = LINE.Remove(0,1);
							s--;
						}
						// Get real part
						foreach (char element in LINE)
						{
							if(element!='+' && element!='-')
								r+=element.ToString();
							else
								break;
						}
						// get imaginary part
						i = LINE.Substring(LINE.Length-(r.Length+s));

						NUMBER = double.Parse(i);
						if(NUMBER==0)
							NUMBER = double.Parse(r);
						else
							NUMBER = double.NaN;

					}
					// NaN value
					else
						NUMBER = double.NaN;
				}


				// Test
				bool IS_INTEGER = false;
				bool IS_INTEGER_T = false;

				if(double.IsNaN(NUMBER))
					IS_INTEGER=false;

				else if(Math.Round(NUMBER,0).ToString() == NUMBER.ToString())
					IS_INTEGER = true;

				else if((decimal)TOLERANCE >= (decimal)Math.Abs( (decimal)Math.Round(NUMBER,0) - (decimal)NUMBER ))
					IS_INTEGER_T = true;



				if(IS_INTEGER)
					Console.WriteLine(" Is exact integer " + IS_INTEGER);

				else
				{
					Console.WriteLine( " Is exact integer " + IS_INTEGER );
					Console.WriteLine( " Is integer with tolerance " + IS_INTEGER_T );
				}


				Console.WriteLine();
				Console.Write(" Another test < Y /N > . . . ");
				key  = Console.ReadKey(true);
				Console.WriteLine();
				Console.WriteLine();
			}

		}

	}
}


```

```txt



 ***************************************************
 *                                                 *
 *              Integerness test                   *
 *                                                 *
 ***************************************************

 Enter number value : 25,000000
 Enter tolerance value : 0,00001
 Is exact integer True

 Another test < Y /N > . . .

 Enter number value : 24,999999
 Enter tolerance value : 0,00001
 Is exact integer False
 Is integer with tolerance True

 Another test < Y /N > . . .

 Enter number value : 25,000100
 Enter tolerance value : 0,00001
 Is exact integer False
 Is integer with tolerance False

 Another test < Y /N > . . .

 Enter number value : -2.1e120
 Enter tolerance value : 0,00001
 Is exact integer True

 Another test < Y /N > . . .

 Enter number value : -5e-2
 Enter tolerance value : 0,00001
 Is exact integer False
 Is integer with tolerance False

 Another test < Y /N > . . .

 Enter number value : NaN
 Enter tolerance value : 0,00001
 Is exact integer False
 Is integer with tolerance False

 Another test < Y /N > . . .

 Enter number value : Inf
 Enter tolerance value : 0,00001
 Is exact integer True

 Another test < Y /N > . . .

 Enter number value : 5,0+0,0i
 Enter tolerance value : 0,00001
 Is exact integer True

 Another test < Y /N > . . .

 Enter number value : 5-5i
 Enter tolerance value : 0,00001
 Is exact integer False
 Is integer with tolerance False

 Another test < Y /N > . . .

 Enter number value : 1,1
 Enter tolerance value : 0,1
 Is exact integer False
 Is integer with tolerance True

 Another test < Y /N > . . .

 Enter number value : 15/7
 Enter tolerance value : 0,15
 Is exact integer False
 Is integer with tolerance True

 Another test < Y /N > . . .


```



## COBOL

COBOL likes to work with fixed-point decimal numbers. For the sake of argument, this program tests the "integerness" of values that have up to nine digits before the decimal point and up to nine digits after. It can therefore be "tricked", as in the third of the four tests below, by computing a result that differs from an integer by less than 0.000000001; if there is any likelihood of such results arising, it would be a good idea to allow more digits of precision after the decimal point. Support for complex numbers (in a sense) is included, because the specification calls for it—but it adds little of interest.

```cobol
IDENTIFICATION DIVISION.
PROGRAM-ID. INTEGERNESS-PROGRAM.
DATA DIVISION.
WORKING-STORAGE SECTION.
01  INTEGERS-OR-ARE-THEY.
    05 POSSIBLE-INTEGER PIC S9(9)V9(9).
    05 DEFINITE-INTEGER PIC S9(9).
01  COMPLEX-NUMBER.
    05 REAL-PART        PIC S9(9)V9(9).
    05 IMAGINARY-PART   PIC S9(9)V9(9).
PROCEDURE DIVISION.
TEST-PARAGRAPH.
    MOVE ZERO TO IMAGINARY-PART.
    DIVIDE -28 BY 7 GIVING POSSIBLE-INTEGER.
    PERFORM INTEGER-PARAGRAPH.
    DIVIDE 28 BY 18 GIVING POSSIBLE-INTEGER.
    PERFORM INTEGER-PARAGRAPH.
    DIVIDE 3 BY 10000000000 GIVING POSSIBLE-INTEGER.
    PERFORM INTEGER-PARAGRAPH.
TEST-COMPLEX-PARAGRAPH.
    MOVE ZERO TO REAL-PART.
    MOVE 1 TO IMAGINARY-PART.
    MOVE REAL-PART TO POSSIBLE-INTEGER.
    PERFORM INTEGER-PARAGRAPH.
    STOP RUN.
INTEGER-PARAGRAPH.
    IF IMAGINARY-PART IS EQUAL TO ZERO THEN PERFORM REAL-PARAGRAPH,
    ELSE PERFORM COMPLEX-PARAGRAPH.
REAL-PARAGRAPH.
    MOVE POSSIBLE-INTEGER TO DEFINITE-INTEGER.
    IF DEFINITE-INTEGER IS EQUAL TO POSSIBLE-INTEGER
    THEN DISPLAY POSSIBLE-INTEGER ' IS AN INTEGER.',
    ELSE DISPLAY POSSIBLE-INTEGER ' IS NOT AN INTEGER.'.
COMPLEX-PARAGRAPH.
    DISPLAY REAL-PART '+' IMAGINARY-PART 'i IS NOT AN INTEGER.'.
```

```txt
-000000004.000000000 IS AN INTEGER.
 000000001.555555555 IS NOT AN INTEGER.
 000000000.000000000 IS AN INTEGER.
 000000000.000000000+ 000000001.000000000i IS NOT AN INTEGER.
```



## D


```D
import std.complex;
import std.math;
import std.meta;
import std.stdio;
import std.traits;

void main() {
    print(25.000000);
    print(24.999999);
    print(24.999999, 0.00001);
    print(25.000100);
    print(-2.1e120);
    print(-5e-2);
    print(real.nan);
    print(real.infinity);
    print(5.0+0.0i);
    print(5-5i);
}

void print(T)(T v, real tol = 0.0) {
    writefln("Is %0.10s an integer? %s", v, isInteger(v, tol));
}

/// Test for plain integers
bool isInteger(T)(T v)
if (isIntegral!T) {
    return true;
}

unittest {
    assert(isInteger(5));
    assert(isInteger(-5));

    assert(isInteger(2L));
    assert(isInteger(-2L));
}

/// Test for floating point
bool isInteger(T)(T v, real tol = 0.0)
if (isFloatingPoint!T) {
    return (v - floor(v)) <= tol || (ceil(v) - v) <= tol;
}

unittest {
    assert(isInteger(25.000000));

    assert(!isInteger(24.999999));
    assert(isInteger(24.999999, 0.00001));
}

/// Test for complex numbers
bool isInteger(T)(Complex!T v, real tol = 0.0) {
    return isInteger(v.re, tol) && abs(v.im) <= tol;
}

unittest {
    assert(isInteger(complex(1.0)));
    assert(!isInteger(complex(1.0, 0.0001)));

    assert(isInteger(complex(1.0, 0.00009), 0.0001));
}

/// Test for built-in complex types
bool isInteger(T)(T v, real tol = 0.0)
if (staticIndexOf!(Unqual!T, AliasSeq!(cfloat, cdouble, creal)) >= 0) {
    return isInteger(v.re, tol) && abs(v.im) <= tol;
}

unittest {
    assert(isInteger(1.0 + 0.0i));
    assert(!isInteger(1.0 + 0.0001i));

    assert(isInteger(1.0 + 0.00009i, 0.0001));
}

/// Test for built-in imaginary types
bool isInteger(T)(T v, real tol = 0.0)
if (staticIndexOf!(Unqual!T, AliasSeq!(ifloat, idouble, ireal)) >= 0) {
    return abs(v) <= tol;
}

unittest {
    assert(isInteger(0.0i));
    assert(!isInteger(0.0001i));

    assert(isInteger(0.00009i, 0.0001));
}
```


```txt
Is 25 an integer? true
Is 24.999999 an integer? false
Is 24.999999 an integer? true
Is 25.0001 an integer? false
Is -2.1e+120 an integer? true
Is -0.05 an integer? false
Is nan an integer? false
Is inf an integer? false
Is 5+0i an integer? true
Is 5-5i an integer? false
```



## Elixir


```elixir
defmodule Test do
  def integer?(n) when n == trunc(n), do: true
  def integer?(_), do: false
end

Enum.each([2, 2.0, 2.5, 2.000000000000001, 1.23e300, 1.0e-300, "123", '123', :"123"], fn n ->
  IO.puts "#{inspect n} is integer?: #{Test.integer?(n)}"
end)
```


```txt

2 is integer?: true
2.0 is integer?: true
2.5 is integer?: false
2.000000000000001 is integer?: false
1.23e300 is integer?: true
1.0e-300 is integer?: false
"123" is integer?: false
'123' is integer?: false
:"123" is integer?: false

```



## Factor

The <code>number=</code> word in the <code>math</code> vocabulary comes very close to encapsulating this task. It compares numbers for equality without regard for class, like <code>=</code> would. However, since <code>>integer</code> and <code>round</code> do not specialize on the <code>complex</code> class, we need to handle complex numbers specially. We use <code>>rect</code> to extract the real components of the complex number for further processing.

```factor
USING: formatting io kernel math math.functions sequences ;
IN: rosetta-code.test-integerness

GENERIC: integral? ( n -- ? )

M: real integral? [ ] [ >integer ] bi number= ;
M: complex integral? >rect [ integral? ] [ 0 number= ] bi* and ;

GENERIC# fuzzy-int? 1 ( n tolerance -- ? )

M: real fuzzy-int? [ dup round - abs ] dip <= ;
M: complex fuzzy-int? [ >rect ] dip swapd fuzzy-int? swap 0
    number= and ;

{
    25/1
    50+2/3
    34/73
    312459210312903/129381293812491284512951
    25.000000
    24.999999
    25.000100
    -2.1e120
    -5e-2
    0/0. ! NaN
    1/0. ! Infinity
    C{ 5.0 0.0 }
    C{ 5 -5 }
    C{ 5 0 }
}
"Number" "Exact int?" "Fuzzy int? (tolerance=0.00001)"
"%-41s %-11s %s\n" printf
[
   [ ] [ integral? ] [ 0.00001 fuzzy-int? ] tri
   "%-41u %-11u %u\n" printf
] each
```

```txt

Number                                    Exact int?  Fuzzy int? (tolerance=0.00001)
25                                        t           t
50+2/3                                    f           f
34/73                                     f           f
312459210312903/129381293812491284512951  f           t
25.0                                      t           t
24.999999                                 f           t
25.0001                                   f           f
-2.1e+120                                 t           t
-0.05                                     f           f
NAN: 8000000000000                        f           f
1/0.                                      f           f
C{ 5.0 0.0 }                              t           t
C{ 5 -5 }                                 f           f
5                                         t           t

```



## Fortran


### Straightforward

The issue is a little delicate, because a number such as 3E120 is integral, but, cannot be represented in an ordinary integer variable. If the idea slides towards whether the value can be represented exactly by an integer variable then the task become easy. The following code takes advantage of the availability of INTEGER*8 variables, and an associated function KIDINT(x) that deals with double precision values and returns a 64-bit integer result. This function truncates - if it were to round then it might want to round up to one beyond the maximum possible integer of that sign. There is a bewildering variety of these truncation and rounding functions, some of which are generic and some not, and if only INTEGER*4 were available, different choices would have to be made.

The MODULE protocol of F90 is used, merely to save on the need to define the types of the function in each routine that uses them, since there is no default type for LOGICAL. Otherwise, this is F77 style.

```Fortran
      MODULE ZERMELO	!Approach the foundations of mathematics.
       CONTAINS
        LOGICAL FUNCTION ISINTEGRAL(X)	!A whole number?
         REAL*8 X	!Alas, this is not really a REAL number.
         INTEGER*8 N	!Largest available.
          IF (ISNAN(X)) THEN	!Avoid some sillyness.
            ISINTEGRAL = .FALSE.	!And possible error messages.
          ELSE			!But now it is safe to try.
            N = KIDINT(X)		!This one truncates.
            ISINTEGRAL = N .EQ. X	!Any difference?
          END IF		!A floating-point number may overflow an integer.
        END FUNCTION ISINTEGRAL	!And even if integral, it will not seem so.

        LOGICAL FUNCTION ISINTEGRALZ(Z)	!For complex numbers, two tests.
         DOUBLE COMPLEX Z	!Still not really REAL, though.
          ISINTEGRALZ = ISINTEGRAL(DBLE(Z)) .AND. ISINTEGRAL(DIMAG(Z))	!Separate the parts.
        END FUNCTION ISINTEGRALZ!No INTEGER COMPLEX type is offered.
      END MODULE ZERMELO	!Much more mathematics lie elsewhere.

      PROGRAM TEST
      USE ZERMELO
      DOUBLE COMPLEX Z

      WRITE (6,*) "See if some numbers are integral..."
      WRITE (6,*) ISINTEGRAL(666D0),666D0
      Z = DCMPLX(-3D0,4*ATAN(1D0))
      WRITE (6,*) ISINTEGRALZ(Z),Z
      END
```


```txt

 See if some numbers are integral...
 T   666.000000000000
 F (-3.00000000000000,3.14159265358979)
```



### Tricky

If however large numbers are to be affirmed as integral even if there is no integer variable capable of holding such values, then a different approach is required. Given that a floating-point number has a finite precision, there will be some number above which no digits can be devoted to fractional parts and so the number represented by the floating-point value must be integral, while for smaller numbers the floating point value can be compared to its integer truncation, as above. Suppose a decimal computer (like the IBM1620!) for convenience, using eight decimal digits for the mantissa (and two for the exponent, as did the IBM1620). A (non-zero) normalised number must be of the form d·ddddddd and the largest number with a fractional digit would be 9999999·9 (represented as 9·9999999E+06 or possibly as ·99999999E+07 depending on the style of normalisation) and the next possible floating-point number would be 1·0000000E+07, then 1·0000001E+07, ''etc.'' advancing in steps of one. No fractional part is possible. Thus the boundary is clear and a test need merely involve a comparison: integral if greater than that, otherwise compare the floating-point number to its truncated integer form.

The argument is the same with binary (or base 4, 8 or 16), but, you have to know what base is used to prepare the proper boundary value, similarly you must ascertain just how many digits of precision are in use, remembering that in binary the leading one of normalised numbers may be represented implicitly, or it may be explicitly present. One would have to devise probing routines with delicate calculations that may be disrupted by various compiler optimisation tricks and unanticipated details of the arithmetic mill. For instance, the Intel 8087 floating-point co-processor and its descendants use an implicit leading-one bit for 32- and 64-bit floating-point numbers, but ''not'' for 80-bit floating-point numbers. So if your compiler offers a REAL*10 type, such variables will enjoy a slightly different style of arithmetic. Further, ''during'' a calculation (add, subtract, multiply, divide) a further three guard bits (with special meanings) are employed. Calculations are done with full 83-bit precision to yield an 80-bit result; it is only when values are stored that they are converted to single or double precision format in storage - the register retains full precision. On top of that, the arithmetic can employ "denormalised" numbers during underflow towards zero. Chapter 6 of ''The I8087 Numeric Data Processor'', page 219, remarks "At least some of the generalised numerical solutions to common mathematical procedures have coding that is so involved and tricky in order to take care of all possible roundoff contingencies that they have been termed 'pornographic algorithms'". So a probe routine that worked for one design will likely need tweaking when tried on another system.

To determine the number of digits of precision, one probes somewhat as follows:
```Fortran
      X = 1
   10 X = X*BASE
      Y = X + 1
      D = Y - X
      IF (D .EQ. 1) GO TO 10
```

Or alternatively, compare 1 + ''eps'' to 1, successively dividing ''eps'' by BASE.

The difficulties include the risk that a compiler might wrongly apply the axia of mathematics to floating-point arithmetic and deduce that D was always one. Similarly, after assigning the result of X + 1 to Y, it may notice that the register could retain that value and so there would be no need to load Y's value to calculate Y - X: if the register was of greater precision than the variable, the probe will err. Producing output can help. As well as being interesting, otherwise a compiler might deduce that there is no need to calculate something because it is not used to produce output nor affects something that does lead to output.

Conveniently, F90 standardised the functions FRACTION(x) and EXPONENT(x) that reveal the parts of a floating point number and with the assistance of the RADIX(x) function that reports the base of the number system and DIGITS(x) the number of digits represented, a suitable boundary value can be constructed. For the decimal example above, 1E7 is the first value that has no space for fractional digits.

If the highest-precision floating-point number is 64-bit, and the largest integer is 64-bit, then, given that some of the 64 bits of the floating-point number are devoted to the exponent, floating-point values up to the threshold will never overflow a 64-bit integer range, and all will be well... A similar process would apply for 32-bit floating-point variables, and so on.

Since the special functions are only available for F90 and later, the example proceeds to activate the F90 protocol for making a function (or subroutine) generic. This requires a suitable function for each desired combination of parameter types and ringing the changes can soon become tedious as well as error-prone, though fortunately here, there is only one parameter and its types to work through. It would be helpful to have a decent pre-processor scheme (such as in pl/i) whereby the various routines would be generated, but there can be surprise obstacles also. Here, INTEGER*8 is not fully incorporated into the compiler as a possibility for integer constants, so it is necessary to pre-define INTEGER*8 BIG so that in its PARAMETER statement the compiler's calculation will have sufficient scope. The INTEGER*4 routine has no such difficulty.

Despite the attempt at generality, there will be difficulties on systems whose word sizes are not multiples of eight bits so that the REAL*4 scheme falters. Still, it is preferable to a blizzard of terms such as small int, int, long int, long long int. A decimal computer would be quite different in its size specifications, and there have been rumours of a Russian computer that worked in base three...

```Fortran

      MODULE ZERMELO	!Approach the foundations of mathematics.
       INTERFACE ISINTEGRAL	!And obscure them with computerese.
        MODULE PROCEDURE ISINTEGRALF4, ISINTEGRALF8,
     1                   ISINTEGRALZ8, ISINTEGRALZ16
       END INTERFACE		!Selection is by parameter type and number.
       CONTAINS			!Sop, now for a grabbag of routines.
        LOGICAL FUNCTION ISINTEGRALF8(X)	!A whole number?
         REAL*8 X	!Alas, this is not really a REAL number.
         INTEGER*8 N	!Largest available.
         INTEGER*8 BIG	!The first number too big to have any fractional digits in floating-point.
         PARAMETER (BIG = RADIX(X)**(DIGITS(X) - 1))	!These "functions" are in fact constants.
          IF (ISNAN(X)) THEN		!Avoid some sillyness.
            ISINTEGRALF8 = .FALSE.	!And possible error messages.
          ELSE IF (ABS(X).GE.BIG) THEN	!But now it is safe to try.
            ISINTEGRALF8 = .TRUE.	!Can't have fractional digits => integral.
          ELSE				!But smaller numbers can have fractional digits.
            N = KIDINT(X)		!So, truncate to an integral value.
            ISINTEGRALF8 = N .EQ. X	!Any difference?
          END IF			!So much for inspection.
        END FUNCTION ISINTEGRALF8	!No need to look at digit sequences.

        LOGICAL FUNCTION ISINTEGRALF4(X)	!A whole number?
         REAL*4 X	!Alas, this is not really a REAL number.
         INTEGER*4 N	!Largest available.
          IF (ISNAN(X)) THEN		!Avoid some sillyness.
            ISINTEGRALF4 = .FALSE.	!And possible error messages.
          ELSE IF (ABS(X) .GE. RADIX(X)**(DIGITS(X) - 1)) THEN	!Constant results as appropriate for X.
            ISINTEGRALF4 = .TRUE.	!Can't have fractional digits => integral.
          ELSE				!But smaller numbers can have fractional digits.
            N = INT(X)			!So, truncate to an integral value.
            ISINTEGRALF4 = N .EQ. X	!Any difference?
          END IF			!A real*4 should not overflow INTEGER*4.
        END FUNCTION ISINTEGRALF4	!Thanks to the size check.

        LOGICAL FUNCTION ISINTEGRALZ8(Z)	!For complex numbers, two tests.
         COMPLEX Z		!Still not really REAL, though.
          ISINTEGRALZ8 = ISINTEGRAL(REAL(Z)) .AND. ISINTEGRAL(AIMAG(Z))	!Separate the parts.
        END FUNCTION ISINTEGRALZ8	!No INTEGER COMPLEX type is offered.

        LOGICAL FUNCTION ISINTEGRALZ16(Z)	!And there are two sorts of complex numbers.
         DOUBLE COMPLEX Z	!Still not really REAL.
          ISINTEGRALZ16 = ISINTEGRAL(DBLE(Z)) .AND. ISINTEGRAL(DIMAG(Z))	!Separate the parts.
        END FUNCTION ISINTEGRALZ16	!No INTEGER COMPLEX type is offered.
      END MODULE ZERMELO	!Much more mathematics lie elsewhere.

      PROGRAM TEST
      USE ZERMELO
      DOUBLE COMPLEX Z
      DOUBLE PRECISION X
      REAL U
Cast forth some pearls.
      WRITE (6,1) 4,DIGITS(U),RADIX(U)
      WRITE (6,1) 8,DIGITS(X),RADIX(X)
    1 FORMAT ("REAL*",I1,":",I3," digits, in base",I2)

      WRITE (6,*) "See if some numbers are integral..."
      WRITE (6,*) ISINTEGRAL(666D0),666D0
      WRITE (6,*) ISINTEGRAL(665.9),665.9
      Z = DCMPLX(-3D0,4*ATAN(1D0))
      WRITE (6,*) ISINTEGRAL(Z),Z
      END

```



```txt

REAL*4: 24 digits, in base 2
REAL*8: 53 digits, in base 2
 See if some numbers are integral...
 T   666.000000000000
 F   665.9000
 F (-3.00000000000000,3.14159265358979)

```



## Go


```go
package main

import (
	"fmt"
	"math"
	"math/big"
	"reflect"
	"strings"
	"unsafe"
)

// Go provides an integerness test only for the big.Rat and big.Float types
// in the standard library.

// The fundamental piece of code needed for built-in floating point types
// is a test on the float64 type:

func Float64IsInt(f float64) bool {
	_, frac := math.Modf(f)
	return frac == 0
}

// Other built-in or stanadard library numeric types are either always
// integer or can be easily tested using Float64IsInt.

func Float32IsInt(f float32) bool {
	return Float64IsInt(float64(f))
}

func Complex128IsInt(c complex128) bool {
	return imag(c) == 0 && Float64IsInt(real(c))
}

func Complex64IsInt(c complex64) bool {
	return imag(c) == 0 && Float64IsInt(float64(real(c)))
}

// Usually just the above statically typed functions would be all that is used,
// but if it is desired to have a single function that can test any arbitrary
// type, including the standard math/big types, user defined types based on
// an integer, float, or complex builtin types, or user defined types that
// have an IsInt() method, then reflection can be used.

type hasIsInt interface {
	IsInt() bool
}

var bigIntT = reflect.TypeOf((*big.Int)(nil))

func IsInt(i interface{}) bool {
	if ci, ok := i.(hasIsInt); ok {
		// Handles things like *big.Rat
		return ci.IsInt()
	}
	switch v := reflect.ValueOf(i); v.Kind() {
	case reflect.Int, reflect.Int8, reflect.Int16,
		reflect.Int32, reflect.Int64,
		reflect.Uint, reflect.Uint8, reflect.Uint16,
		reflect.Uint32, reflect.Uint64, reflect.Uintptr:
		// Built-in types and any custom type based on them
		return true
	case reflect.Float32, reflect.Float64:
		// Built-in floats and anything based on them
		return Float64IsInt(v.Float())
	case reflect.Complex64, reflect.Complex128:
		// Built-in complexes and anything based on them
		return Complex128IsInt(v.Complex())
	case reflect.String:
		// Could also do strconv.ParseFloat then FloatIsInt but
		// big.Rat handles everything ParseFloat can plus more.
		// Note, there is no strconv.ParseComplex.
		if r, ok := new(big.Rat).SetString(v.String()); ok {
			return r.IsInt()
		}
	case reflect.Ptr:
		// Special case for math/big.Int
		if v.Type() == bigIntT {
			return true
		}
	}
	return false
}

// The rest is just demonstration and display

type intbased int16
type complexbased complex64
type customIntegerType struct {
	// Anything that stores or represents a sub-set
	// of integer values in any way desired.
}

func (customIntegerType) IsInt() bool    { return true }
func (customIntegerType) String() string { return "<…>" }

func main() {
	hdr := fmt.Sprintf("%27s  %-6s %s\n", "Input", "IsInt", "Type")
	show2 := func(t bool, i interface{}, args ...interface{}) {
		istr := fmt.Sprint(i)
		fmt.Printf("%27s  %-6t %T ", istr, t, i)
		fmt.Println(args...)
	}
	show := func(i interface{}, args ...interface{}) {
		show2(IsInt(i), i, args...)
	}

	fmt.Print("Using Float64IsInt with float64:\n", hdr)
	neg1 := -1.
	for _, f := range []float64{
		0, neg1 * 0, -2, -2.000000000000001, 10. / 2, 22. / 3,
		math.Pi,
		math.MinInt64, math.MaxUint64,
		math.SmallestNonzeroFloat64, math.MaxFloat64,
		math.NaN(), math.Inf(1), math.Inf(-1),
	} {
		show2(Float64IsInt(f), f)
	}

	fmt.Print("\nUsing Complex128IsInt with complex128:\n", hdr)
	for _, c := range []complex128{
		3, 1i, 0i, 3.4,
	} {
		show2(Complex128IsInt(c), c)
	}

	fmt.Println("\nUsing reflection:")
	fmt.Print(hdr)
	show("hello")
	show(math.MaxFloat64)
	show("9e100")
	f := new(big.Float)
	show(f)
	f.SetString("1e-3000")
	show(f)
	show("(4+0i)", "(complex strings not parsed)")
	show(4 + 0i)
	show(rune('§'), "or rune")
	show(byte('A'), "or byte")
	var t1 intbased = 5200
	var t2a, t2b complexbased = 5 + 0i, 5 + 1i
	show(t1)
	show(t2a)
	show(t2b)
	x := uintptr(unsafe.Pointer(&t2b))
	show(x)
	show(math.MinInt32)
	show(uint64(math.MaxUint64))
	b, _ := new(big.Int).SetString(strings.Repeat("9", 25), 0)
	show(b)
	r := new(big.Rat)
	show(r)
	r.SetString("2/3")
	show(r)
	show(r.SetFrac(b, new(big.Int).SetInt64(9)))
	show("12345/5")
	show(new(customIntegerType))
}
```

```txt

Using Float64IsInt with float64:
                      Input  IsInt  Type
                          0  true   float64
                         -0  true   float64
                         -2  true   float64
         -2.000000000000001  false  float64
                          5  true   float64
          7.333333333333333  false  float64
          3.141592653589793  false  float64
     -9.223372036854776e+18  true   float64
     1.8446744073709552e+19  true   float64
                     5e-324  false  float64
    1.7976931348623157e+308  true   float64
                        NaN  false  float64
                       +Inf  false  float64
                       -Inf  false  float64

Using Complex128IsInt with complex128:
                      Input  IsInt  Type
                     (3+0i)  true   complex128
                     (0+1i)  false  complex128
                     (0+0i)  true   complex128
                   (3.4+0i)  false  complex128

Using reflection:
                      Input  IsInt  Type
                      hello  false  string
    1.7976931348623157e+308  true   float64
                      9e100  true   string
                          0  true   *big.Float
                    1e-3000  false  *big.Float
                     (4+0i)  false  string (complex strings not parsed)
                     (4+0i)  true   complex128
                        167  true   int32 or rune
                         65  true   uint8 or byte
                       5200  true   main.intbased
                     (5+0i)  true   main.complexbased
                     (5+1i)  false  main.complexbased
               842350779352  true   uintptr
                -2147483648  true   int
       18446744073709551615  true   uint64
  9999999999999999999999999  true   *big.Int
                        0/1  true   *big.Rat
                        2/3  false  *big.Rat
1111111111111111111111111/1  true   *big.Rat
                    12345/5  true   string
                        <…>  true   *main.customIntegerType

```



## Haskell


Some imports for additional number types

```haskell
import Data.Decimal
import Data.Ratio
import Data.Complex
```


Haskell is statically typed, so in order to get universal integerness test we define a class of numbers, which may contain integers:

```haskell
class ContainsInteger a where
  isInteger :: a -> Bool
```


Laws for this class are simple:


```txt
for integral numbers:
isInteger n ≡ True

for real fractional numbers:
isInteger x ⟺ truncate x = x

```


Here are some instances, which literally express class laws:

Integral numbers:

```haskell
instance ContainsInteger Int where isInteger _ = True
instance ContainsInteger Integer where isInteger _ = True
```


Real fractional numbers:

```haskell
isIntegerF :: (Eq x, RealFrac x) => x -> Bool
isIntegerF x = x == fromInteger (truncate x)

instance ContainsInteger Double where isInteger = isIntegerF
instance Integral i => ContainsInteger (DecimalRaw i) where isInteger = isIntegerF
instance Integral i => ContainsInteger (Ratio i) where isInteger = isIntegerF
```


Complex numbers:

```haskell
instance (Eq a, Num a, ContainsInteger a) => ContainsInteger (Complex a) where
  isInteger z = isInteger (realPart z) && (imagPart z == 0)
```


'''Extra credit'''

Approximate integerness for fractional numbers:

```haskell
x ~~ eps = abs x <= eps

almostInteger :: RealFrac a => a -> a -> Bool
almostInteger eps x = (x - fromInteger (round x)) ~~ eps

almostIntegerC :: RealFrac a => a -> Complex a -> Bool
almostIntegerC eps z = almostInteger eps (realPart z) && (imagPart z) ~~ eps
```


'''Testing'''

```haskell
tests = all (== True)
  [ isInteger (5          :: Integer)
  , isInteger (5.0        :: Decimal)
  , isInteger (-5         :: Integer)
  , isInteger (0          :: Decimal)
  , isInteger (-2.1e120   :: Double)
  , isInteger (5 % 1      :: Rational)
  , isInteger (4 % 2      :: Rational)
  , isInteger (5 :+ 0     :: Complex Integer)
  , isInteger (5.0 :+ 0.0 :: Complex Decimal)
  , isInteger (6 % 3 :+ 0 :: Complex Rational)
  , isInteger (1/0        :: Double) -- Infinity is integer
  , isInteger (1.1/0      :: Double) -- Infinity is integer
  , not $ isInteger (5.01       :: Decimal)
  , not $ isInteger (-5e-2      :: Double)
  , not $ isInteger (5 % 3      :: Rational)
  , not $ isInteger (5 :+ 1     :: Complex Integer)
  , not $ isInteger (6 % 4 :+ 0 :: Complex Rational)
  , not $ isInteger (5.0 :+ 1.0 :: Complex Decimal)
  , almostInteger 0.01 2.001
  , almostInteger 0.01 (-1.999999)
  , almostInteger (1 % 10) (24 % 23)
  , not $ almostInteger 0.01 2.02
  , almostIntegerC 0.001 (5.999999 :+ 0.000001)
  ]
```


'''Possible use'''

Effective definition of Pithagorean triangles:


```haskell
pithagoreanTriangles :: [[Integer]]
pithagoreanTriangles =
  [ [a, b, round c] | b <- [1..]
                    , a <- [1..b]
                    , let c = sqrt (fromInteger (a^2 + b^2))
                    , isInteger (c :: Double) ]
```


```txt
λ> take 7 pithagoreanTriangles
[[3,4,5],[6,8,10],[5,12,13],[9,12,15],[8,15,17],[12,16,20],[15,20,25]]

λ> pithagoreanTriangles !! 1000
[726,968,1210]

λ> head $ filter ((> 1000). sum) pithagoreanTriangles
[297,304,425]
```



## J

'''Solution''':
```j
   isInt =:  (= <.) *. (= {.@+.)
```

'''Alternative solution''' (remainder after diving by 1?):
```j
   isInt=:  (0 = 1&|) *. (0 = {:@+.)
```

'''Example''':
```j
   isInt 3.14 7 1.4j0 4j0 5j3 5r3 6r3
0 1 0 1 0 0 1
```



## Java

```Java
import java.math.BigDecimal;
import java.util.List;

public class TestIntegerness {
    private static boolean isLong(double d) {
        return isLong(d, 0.0);
    }

    private static boolean isLong(double d, double tolerance) {
        return (d - Math.floor(d)) <= tolerance || (Math.ceil(d) - d) <= tolerance;
    }

    @SuppressWarnings("ResultOfMethodCallIgnored")
    private static boolean isBigInteger(BigDecimal bd) {
        try {
            bd.toBigIntegerExact();
            return true;
        } catch (ArithmeticException ex) {
            return false;
        }
    }

    private static class Rational {
        long num;
        long denom;

        Rational(int num, int denom) {
            this.num = num;
            this.denom = denom;
        }

        boolean isLong() {
            return num % denom == 0;
        }

        @Override
        public String toString() {
            return String.format("%s/%s", num, denom);
        }
    }

    private static class Complex {
        double real;
        double imag;

        Complex(double real, double imag) {
            this.real = real;
            this.imag = imag;
        }

        boolean isLong() {
            return TestIntegerness.isLong(real) && imag == 0.0;
        }

        @Override
        public String toString() {
            if (imag >= 0.0) {
                return String.format("%s + %si", real, imag);
            }
            return String.format("%s - %si", real, imag);
        }
    }

    public static void main(String[] args) {
        List<Double> da = List.of(25.000000, 24.999999, 25.000100);
        for (Double d : da) {
            boolean exact = isLong(d);
            System.out.printf("%.6f is %s integer%n", d, exact ? "an" : "not an");
        }
        System.out.println();

        double tolerance = 0.00001;
        System.out.printf("With a tolerance of %.5f:%n", tolerance);
        for (Double d : da) {
            boolean fuzzy = isLong(d, tolerance);
            System.out.printf("%.6f is %s integer%n", d, fuzzy ? "an" : "not an");
        }
        System.out.println();

        List<Double> fa = List.of(-2.1e120, -5e-2, Double.NaN, Double.POSITIVE_INFINITY);
        for (Double f : fa) {
            boolean exact = !f.isNaN() && !f.isInfinite() && isBigInteger(new BigDecimal(f.toString()));
            System.out.printf("%s is %s integer%n", f, exact ? "an" : "not an");
        }
        System.out.println();

        List<Complex> ca = List.of(new Complex(5.0, 0.0), new Complex(5.0, -5.0));
        for (Complex c : ca) {
            boolean exact = c.isLong();
            System.out.printf("%s is %s integer%n", c, exact ? "an" : "not an");
        }
        System.out.println();

        List<Rational> ra = List.of(new Rational(24, 8), new Rational(-5, 1), new Rational(17, 2));
        for (Rational r : ra) {
            boolean exact = r.isLong();
            System.out.printf("%s is %s integer%n", r, exact ? "an" : "not an");
        }
    }
}
```

```txt
25.000000 is an integer
24.999999 is not an integer
25.000100 is not an integer

With a tolerance of 0.00001:
25.000000 is an integer
24.999999 is an integer
25.000100 is not an integer

-2.1E120 is an integer
-0.05 is not an integer
NaN is not an integer
Infinity is not an integer

5.0 + 0.0i is an integer
5.0 - -5.0i is not an integer

24/8 is an integer
-5/1 is an integer
17/2 is not an integer
```



## jq

jq does not have builtin support for complex numbers or rationals, but in conformity with
the Rosetta Code page [[Arithmetic/Complex#jq]], we shall assume in the following that the complex number x+iy
has been identified with the array [x,y].  To illustrate how the task can be solved for rationals,
we shall also identify the rational numbers p/q with JSON objects that have the form:
{"type": "rational", "p": p, "q": q}.

```jq
def is_integral:
  if type == "number" then . == floor
  elif type == "array" then
       length == 2 and .[1] == 0 and (.[0] | is_integral)
  else type == "object"
       and .type == "rational"
       and  .q != 0
       and (.q | is_integral)
       and ((.p / .q) | is_integral)
  end ;
```

'''Example''':

```jq
(
   0,   -1, [3,0], {"p": 4, "q": 2, "type": "rational"},
 1.1, -1.1, [3,1], {"p": 5, "q": 2, "type": "rational"}
 ) | "\(.) => \(if is_integral then "integral" else "" end)"
```

```sh
$ jq -r -n -f is_integral.jq
0 => integral
-1 => integral
[3,0] => integral
{"p":4,"q":2,"type":"rational"} => integral
1.1 =>
-1.1 =>
[3,1] =>
{"p":5,"q":2,"type":"rational"} =>
```



## Julia


```julia
# v0.6.0

@show isinteger(25.000000)
@show isinteger(24.999999)
@show isinteger(25.000100)
@show isinteger(-2.1e120)
@show isinteger(-5e-2)
@show isinteger(NaN)
@show isinteger(Inf)
@show isinteger(complex(5.0, 0.0))
@show isinteger(complex(5, 5))

```


```txt
isinteger(25.0) = true
isinteger(24.999999) = false
isinteger(25.0001) = false
isinteger(-2.1e120) = true
isinteger(-0.05) = false
isinteger(NaN) = false
isinteger(Inf) = false
isinteger(complex(5.0, 0.0)) = true
isinteger(complex(5, 5)) = false
```



## Kotlin

As Kotlin doesn't have built in rational or complex number classes, we create 'bare bones' classes for the purposes of this task:

```scala
// version 1.1.2

import java.math.BigInteger
import java.math.BigDecimal

fun Double.isLong(tolerance: Double = 0.0) =
    (this - Math.floor(this)) <= tolerance || (Math.ceil(this) - this) <= tolerance

fun BigDecimal.isBigInteger() =
    try {
        this.toBigIntegerExact()
        true
    }
    catch (ex: ArithmeticException) {
        false
    }

class Rational(val num: Long, val denom: Long) {
    fun isLong() = num % denom == 0L

    override fun toString() = "$num/$denom"
}

class Complex(val real: Double, val imag: Double) {
    fun isLong() = real.isLong() && imag == 0.0

    override fun toString() =
        if (imag >= 0.0)
            "$real + ${imag}i"
        else
            "$real - ${-imag}i"
}

fun main(args: Array<String>) {
    val da = doubleArrayOf(25.000000, 24.999999, 25.000100)
    for (d in da) {
        val exact = d.isLong()
        println("${"%.6f".format(d)} is ${if (exact) "an" else "not an"} integer")
    }
    val tolerance = 0.00001
    println("\nWith a tolerance of ${"%.5f".format(tolerance)}:")
    for (d in da) {
        val fuzzy = d.isLong(tolerance)
        println("${"%.6f".format(d)} is ${if (fuzzy) "an" else "not an"} integer")
    }

    println()
    val fa = doubleArrayOf(-2.1e120, -5e-2, Double.NaN, Double.POSITIVE_INFINITY)
    for (f in fa) {
        val exact = if (f.isNaN() || f.isInfinite()) false
                    else BigDecimal(f.toString()).isBigInteger()
        println("$f is ${if (exact) "an" else "not an"} integer")
    }

    println()
    val ca = arrayOf(Complex(5.0, 0.0), Complex(5.0, -5.0))
    for (c in ca) {
        val exact = c.isLong()
        println("$c is ${if (exact) "an" else "not an"} integer")
    }

    println()
    val ra = arrayOf(Rational(24, 8), Rational(-5, 1), Rational(17, 2))
    for (r in ra) {
        val exact = r.isLong()
        println("$r is ${if (exact) "an" else "not an"} integer")
    }
}
```


```txt

25.000000 is an integer
24.999999 is not an integer
25.000100 is not an integer

With a tolerance of 0.00001:
25.000000 is an integer
24.999999 is an integer
25.000100 is not an integer

-2.1E120 is an integer
-0.05 is not an integer
NaN is not an integer
Infinity is not an integer

5.0 + 0.0i is an integer
5.0 - 5.0i is not an integer

24/8 is an integer
-5/1 is an integer
17/2 is not an integer

```



## Lua


```lua
function isInt (x) return type(x) == "number" and x == math.floor(x) end

print("Value\tInteger?")
print("
### ==\t=====
")
local testCases = {2, 0, -1, 3.5, "String!", true}
for _, input in pairs(testCases) do print(input, isInt(input)) end
```

```txt
Value   Integer?

### ==   =====

2       true
0       true
-1      true
3.5     false
String! false
true    false
```



## ooRexx


```oorexx
/* REXX ---------------------------------------------------------------
* 22.06.2014 Walter Pachl using a complex data class
* ooRexx Distribution contains an elaborate complex class
* parts of which are used here
* see REXX for Extra Credit implementation
*--------------------------------------------------------------------*/
Numeric Digits 1000
Call test_integer .complex~new(1e+12,0e-3)
Call test_integer .complex~new(3.14)
Call test_integer .complex~new(1.00000)
Call test_integer .complex~new(33)
Call test_integer .complex~new(999999999)
Call test_integer .complex~new(99999999999)
Call test_integer .complex~new(1e272)
Call test_integer .complex~new(0)
Call test_integer .complex~new(1.000,-3)
Call test_integer .complex~new(1.000,-3.3)
Call test_integer .complex~new(,4)
Call test_integer .complex~new(2.00000000,+0)
Call test_integer .complex~new(,0)
Call test_integer .complex~new(333)
Call test_integer .complex~new(-1,-1)
Call test_integer .complex~new(1,1)
Call test_integer .complex~new(,.00)
Call test_integer .complex~new(,1)
Call test_integer .complex~new(0003,00.0)
Exit

test_integer:
Use Arg cpx
cpxa=left(changestr('+-',cpx,'-'),13)  -- beautify representation
Select
  When cpx~imaginary<>0 Then
    Say cpxa 'is not an integer'
  When datatype(cpx~real,'W') Then
    Say cpxa 'is an integer'
  Otherwise
    Say cpxa 'is not an integer'
  End
Return

::class complex

::method init                               /* initialize a complex number    */
expose real imaginary                       /* expose the state data          */
use Strict arg first=0, second=0            /* access the two numbers         */
real = first + 0                            /* force rounding                 */
imaginary = second + 0                      /* force rounding on the second   */

::method real                               /* return real part of a complex  */
expose real                                 /* access the state information   */
return real                                 /* return that value              */

::method imaginary                          /* return imaginary part          */
expose imaginary                            /* access the state information   */
return imaginary                            /* return the value               */

::method string                             /* format as a string value       */
expose real imaginary                       /* get the state info             */
return real'+'imaginary'i'                  /* format as real+imaginaryi      */
```

'''output'''

```txt
1E+12+0i      is an integer
3.14+0i       is not an integer
1.00000+0i    is an integer
33+0i         is an integer
999999999+0i  is an integer
1.00000000E+1 is an integer
1E+272+0i     is an integer
0+0i          is an integer
1.000-3i      is not an integer
1.000-3.3i    is not an integer
0+4i          is not an integer
2.00000000+0i is an integer
0+0i          is an integer
333+0i        is an integer
-1-1i         is not an integer
1+1i          is not an integer
0+0i          is an integer
0+1i          is not an integer
3+0i          is an integer
```


=={{header|Mathematica}} / {{header|Wolfram Language}}==
The built-in function IntegerQ performs the required test

```Mathematica
IntegerQ /@ {E, 2.4, 7, 9/2}
```

```txt
{False,False,True,False}
```


## PARI/GP


The operator <code>==</code> does what we want here, comparing a number mathematically regardless of how it's stored. <code>===</code> checks literal equivalence instead.


```parigp
isInteger(z)=real(z)==real(z)\1 && imag(z)==imag(z)\1;
apply(isInteger, [7, I, 1.7 + I, 10.0 + I, 1.0 - 7.0 * I])
```

```txt
%1 = [1, 1, 0, 1, 1]
```



## Perl



```perl6
use Math::Complex;

sub is_int {
    my $number = shift;

    if (ref $number eq 'Math::Complex') {
        return 0 if $number->Im != 0;
        $number = $number->Re;
    }

    return int($number) == $number;
}

for (5, 4.1, sqrt(2), sqrt(4), 1.1e10, 3.0-0.0*i, 4-3*i, 5.6+0*i) {
    printf "%20s is%s an integer\n", $_, (is_int($_) ? "" : " NOT");
}
```


```txt

                   5 is an integer
                 4.1 is NOT an integer
     1.4142135623731 is NOT an integer
                   2 is an integer
         11000000000 is an integer
                   3 is an integer
                4-3i is NOT an integer
                 5.6 is NOT an integer

```



## Perl 6


In Perl 6, all numeric types have a method called <tt>narrow</tt>, which returns an object with the same value but of the most appropriate type.  So we can just check if ''that'' object is an <tt>Int</tt>. This works even with floats with large exponents, because the <tt>Int</tt> type supports arbitrarily large integers.

For the extra credit task, we can add another multi candidate that checks the distance between the number and it's nearest integer, but then we'll have to handle complex numbers specially.


```perl6
multi is-int ($n) { $n.narrow ~~ Int }

multi is-int ($n, :$tolerance!) {
    abs($n.round - $n) <= $tolerance
}

multi is-int (Complex $n, :$tolerance!) {
    is-int($n.re, :$tolerance) && abs($n.im) < $tolerance
}

# Testing:

for 25.000000, 24.999999, 25.000100, -2.1e120, -5e-2, Inf, NaN, 5.0+0.0i, 5-5i {
    printf "%-7s  %-9s  %-5s  %-5s\n", .^name, $_,
        is-int($_),
        is-int($_, :tolerance<0.00001>);
}
```


```txt

Rat      25         True   True
Rat      24.999999  False  True
Rat      25.0001    False  False
Num      -2.1e+120  True   True
Num      -0.05      False  False
Num      Inf        False  False
Num      NaN        False  False
Complex  5+0i       True   True
Complex  5-5i       False  False

```



## Phix

In most cases the builtin works prety well, with Phix automatically storing integer results as such.

```Phix
?integer(3.5+3.5) -- true
?integer(3.5+3.4) -- false
```

The round function takes an inverted precision, so 1000000 means to the nearest 0.000001 and 100000 means the nearest 0.00001

```Phix
?integer(round(24.999999,1000000))  -- false
?integer(round(24.999999,100000))   -- true
```

By default the inverted precision of round is 1, and that does exactly what you'd expect.

```Phix
?equal(-2.1e120,round(-2.1e120)) -- true
?equal(-2.15,round(-2.15))       -- false
```

Technically though, -2.1e120 is way past precision limits, as next, so declaring it integer is deeply flawed...
It is not only way too big to fit in an integer, but also simply too big to actually have a fractional part.
Obviously using bigatoms would "solve" this, as long as I was prepared to wait for it to wade through the 120+
digits of precision needed, that is compared to the mere 19 or so that the raw physical hardware can manage.

```Phix
?equal(-2.1e120,-2.1e120+PI)     -- true!!
```

Phix considers both nan and inf as not an integer, and does not support complex numbers (as a primitive type). Two final examples:

```Phix
?integer(-5e-2)                  -- false
?integer(25.000000)              -- true
```



## PicoLisp

Pico Lisp scaled fixed-point numbers. Every number is stored an an Integer and  a Non-integer only relative to the scale applied. For this example we assume that all numbers are generated with the same scale. This is the common case.

```PicoLisp

(de int? (N)
  (= N (* 1.0 (/ N 1.0)))) #returns T or NIL

(de integer? (N)
  (and (= N (* 1.0 (/ N 1.0))) N)) #returns value of N or NIL

(scl 4) #-> 4 # *Scl the global which holds
1.0 #-> 10000
(int? 1.0) #-> T
(int? 1) #-> NIL # 1 with a scale of 4 is same as 0.0001 which is not an Integer
(int? -1.0) #-> T
(int? -0.0) #-> T
(int? "RE") #-> "RE" -- Number expected
(int? (*/ 2.0 1.0 3.0)) #-> NIL # 6667 is not an integer of the scale of 4, use of */ because of the scale

```



## PowerShell


```PowerShell

function Test-Integer ($Number)
{
    try
    {
        $Number = [System.Numerics.Complex]$Number

        if (($Number.Real -eq [int]$Number.Real) -and ($Number.Imaginary -eq 0))
        {
            return $true
        }
        else
        {
            return $false
        }
    }
    catch
    {
        Write-Host "Parameter was not a number."
    }
}

```


```PowerShell

Test-Integer 9
Test-Integer 9.9
Test-Integer (New-Object System.Numerics.Complex(14,0))
Test-Integer (New-Object System.Numerics.Complex(14,56))
Test-Integer "abc"

```

```txt

True
False
True
False
Parameter was not a number.

```



## Python



```python>>>
 def isint(f):
    return complex(f).imag == 0 and complex(f).real.is_integer()

>>> [isint(f) for f in (1.0, 2, (3.0+0.0j), 4.1, (3+4j), (5.6+0j))]
[True, True, True, False, False, False]

>>> # Test cases
...
>>> isint(25.000000)
True
>>> isint(24.999999)
False
>>> isint(25.000100)
False
>>> isint(-2.1e120)
True
>>> isint(-5e-2)
False
>>> isint(float('nan'))
False
>>> isint(float('inf'))
False
>>> isint(5.0+0.0j)
True
>>> isint(5-5j)
False

```



## Racket

The scheme/racket number pyramid is notoriously difficult to navigate.
The following are integers representations that *I* know of, but I'm
sure there are plenty more!

See [http://docs.racket-lang.org/reference/number-types.html?q=integer%3F#%28def._%28%28quote._~23~25kernel%29._integer~3f%29%29 documentation for <code>integer?</code>]


```racket
#lang racket
(require tests/eli-tester)

(test ;; known representations of integers:
 ;; - as exacts
 (integer? -1) => #t
 (integer?  0) => #t
 (integer?  1) => #t
 (integer?   1234879378539875943875937598379587539875498792424323432432343242423432432) => #t
 (integer?  -1234879378539875943875937598379587539875498792424323432432343242423432432) => #t
 (integer?  #xff) => #t

 ;; - as inexacts
 (integer? -1.) => #t
 (integer?  0.) => #t
 (integer?  1.) => #t
 (integer?  1234879378539875943875937598379587539875498792424323432432343242423432432.) => #t
 (integer?  #xff.0) => #t
 ;; - but without a decimal fractional part
 (integer? -1.1) => #f

 ;; - fractional representation
 (integer? -42/3) => #t
 (integer?   0/1) => #t
 (integer?  27/9) => #t
 (integer?  #xff/f) => #t
 (integer?  #b11111111/1111) => #t
 ;; - but obviously not fractions
 (integer? 5/7) => #f

 ; - as scientific
 (integer?  1.23e2) => #t
 (integer?  1.23e120) => #t
 ; - but not with a small exponent
 (integer?  1.23e1) => #f

 ; - complex representations with 0 imaginary component
 ;   ℤ is a subset of the sets of rational and /real/ numbers and
 (integer? 1+0i) => #t
 (integer? (sqr 0+1i)) => #t
 (integer? 0+1i) => #f

 ;; oh, there's so much else that isn't an integer:
 (integer? "woo") => #f
 (integer? "100") => #f
 (integer? (string->number "22/11")) => #t ; just cast it!
 (integer? +inf.0) => #f
 (integer? -inf.0) => #f
 (integer? +nan.0) => #f ; duh! it's not even a number!
 (integer? -NaN.0) => #f
 (integer? pi) => #f
 )

```

All tests pass.


## REXX


### version 1


```rexx
/* REXX ---------------------------------------------------------------
* 20.06.2014 Walter Pachl
* 22.06.2014 WP add complex numbers such as 13-12j etc.
* (using 13e-12 or so is not (yet) supported)
*--------------------------------------------------------------------*/
Call test_integer 3.14
Call test_integer 1.00000
Call test_integer 33
Call test_integer 999999999
Call test_integer 99999999999
Call test_integer 1e272
Call test_integer 'AA'
Call test_integer '0'
Call test_integer '1.000-3i'
Call test_integer '1.000-3.3i'
Call test_integer '4j'
Call test_integer '2.00000000+0j'
Call test_integer '0j'
Call test_integer '333'
Call test_integer '-1-i'
Call test_integer '1+i'
Call test_integer '.00i'
Call test_integer 'j'
Call test_integer '0003-00.0j'
Exit

test_integer:
Parse Arg xx
Numeric Digits 1000
Parse Value parse_number(xx) With x imag
If imag<>0 Then Do
  Say left(xx,13) 'is not an integer (imaginary part is not zero)'
  Return
  End
Select
  When datatype(x)<>'NUM' Then
    Say left(xx,13) 'is not an integer (not even a number)'
  Otherwise Do
    If datatype(x,'W') Then
      Say left(xx,13) 'is an integer'
    Else
      Say left(xx,13) 'isn''t an integer'
    End
  End
Return
parse_number: Procedure
  Parse Upper Arg x
  x=translate(x,'I','J')
  If pos('I',x)>0 Then Do
    pi=verify(x,'+-','M')
    Select
      When pi>1 Then Do
        real=left(x,pi-1)
        imag=substr(x,pi)
        End
      When pi=0 Then Do
        real=0
        imag=x
        End
      Otherwise /*pi=1*/Do
        p2=verify(substr(x,2),'+-','M')
        If p2>0 Then Do
          real=left(x,p2)
          imag=substr(x,p2+1)
          End
        Else Do
          real=0
          imag=x
          End
        End
      End
    End
  Else Do
    real=x
    imag='0I'
    End
  pi=verify(imag,'+-','M')
  If pi=0 Then Do
    Parse Var imag imag_v 'I'
    imag_sign='+'
    End
  Else
    Parse Var imag imag_sign 2 imag_v 'I'
  If imag_v='' Then
    imag_v=1
  imag=imag_sign||imag_v

  Return real imag
```

'''output'''

```txt
3.14          isn't an integer
1.00000       is an integer
33            is an integer
999999999     is an integer
99999999999   is an integer
1E272         is an integer
AA            is not an integer (not even a number)
0             is an integer
1.000-3i      is not an integer (imaginary part is not zero)
1.000-3.3i    is not an integer (imaginary part is not zero)
4j            is not an integer (imaginary part is not zero)
2.00000000+0j is an integer
0j            is an integer
333           is an integer
-1-i          is not an integer (imaginary part is not zero)
1+i           is not an integer (imaginary part is not zero)
.00i          is an integer
j             is not an integer (imaginary part is not zero)
0003-00.0j    is an integer
```



### version 1a  Extra Credit


```rexx
/* REXX ---------------------------------------------------------------
* Extra credit
* Instead of using the datatype built-in function one could use this
*--------------------------------------------------------------------*/
Call testi 25.000000
Call testi 24.999999
Call testi 25.000100
Call testi  0.9999999
Call testi -0.9999999
Exit

testi:
Parse Arg x
If pos('.',x)>0 Then Do
  xx=abs(x)
  Parse Value abs(xx) With '.' d
  d5=left(d,5,0)
  End
Else d5=''
If d5='' | wordpos(d5,'00000 99999')>0 Then
  Say x 'is an integer'
Else
  Say x 'isn''t an integer'
Return
```

```txt
25.000000 is an integer
24.999999 is an integer
25.000100 isn't an integer
0.9999999 is an integer
-0.9999999 is an integer
```



### version 2

This REXX version handles an exponent indicator of   '''E''',   '''D''',   or   '''Q'''   (either lower or uppercase),   and

it also supports a trailing   '''I'''   or   '''J'''   imaginary indicator.

('''E''',   '''D''',   and   '''Q'''   indicate an exponent for a single precision, double precision, and quad precision numbers, respectively.)

This version also handles numbers   larger   than can be stored (within REXX) as simple integers within the limits of   '''numeric digits'''.

Also, most REXXes have a limit on the minimum/maximum value of the power in exponentiated numbers.

```rexx
/*REXX program  tests  if a number  (possibly complex)  is  equivalent  to an integer.  */
numeric digits 3000                              /*be able to handle gihugic integers.  */
parse arg #s                                     /*obtain optional numbers list from CL.*/
if #s=''  then #s= '3.14   1.00000   33   999999999   99999999999   1e272   AA   0'    ,
                   '1.000-3i   1.000-3.3i   4j   2.00000000+0j   0j   333   -1-i'      ,
                   '1+i   .00i   j   0003-00.0j   1.2d1   2e55666   +0003-00.0j   +0j' ,
                   '-.3q+2   -0i   +03.0e+01+0.00e+20j   -030.0e-001+0.0e-020j'
                                                 /* [↑]  use these numbers for defaults.*/
  do j=1  for words(#s);    ox=word(#s, j)       /*obtain a number from the numbers list*/
  parse  upper  var  ox  x                       /*obtain an uppercase version of  OX.  */
  x=translate(x, 'EEI', "QDJ")                   /*translate exponent and imag indicator*/
  if right(x, 1)=='I'  then call tImag           /*has the  X  number an imaginary part?*/
  if isInt(x)  then say  right(ox, 55)  "     is an integer."      /*yuppers, it does.  */
               else say  right(ox, 55)  "  isn't an integer."      /*noppers, it doesn't*/
  end   /*j*/                                    /* [↑]  process each number in the list*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
isInt:  procedure;  parse arg n                  /*obtain the number in question.       */
        if datatype(n, 'Whole')  then return 1   /*it's a simple integer (small).       */
        parse  var  n   m  'E'  p                /*separate base from the  10's  power. */
        if \datatype(p, 'Numb')  then return 0   /*Not an integer if  P  not an integer.*/
        return  p>0  |  m=0                      /*is   power>0   or   mantissa = zero? */
/*──────────────────────────────────────────────────────────────────────────────────────*/
isSign: parse arg ? 2;   return ?=='+' | ?=="-"  /*a method to test for a leading sign. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
tImag:  x=left(x, length(x) -1)                  /*strip the trailing I or J from number*/
        if isInt(x)   then do                    /*is what's remaining an integer ?     */
                           if x\=0  then x=.     /*what's remaining isn't equal to zero.*/
                           return                /*return to invoker in either case.    */
                           end                   /* [↑]  handle simple imaginary case.  */
        if isSign(x)  then x=substr(x, 2)        /*X has a sign?  Strip the leading sign*/
        e=verify(x, .0123456789)                 /*find 1st char not a digit or a  dot. */
        if e==0       then do; x=.; return; end  /*Nothing?  Then it's not an integer.  */
        y=substr(x, e, 1)                        /*Y  is the suspect character.         */
        if isSign(y)  then do                    /*is suspect character a plus or minus?*/
                           z=substr(x, e+1)      /*obtain the imaginary part of  X.     */
                           x=  left(x, e-1)      /*   "    "    real      "   "  "      */
                           if isInt(z)  then if z=0  then return   /*imaginary part is 0*/
                           x=.                   /*the imaginary part isn't zero.       */
                           end                   /* [↑]  end of imaginary part of  X.   */
        if y\=='E'    then return                /*real part of X doesn't have an expon.*/
        p=substr(x, e+1)                         /*obtain power of real part of  X.     */
        _=  left(p, 1)                           /*obtain the possible sign of the power*/
        if isSign(_)  then p=substr(p, 2)        /*strip the sign from the exponent.    */
        s=verify(p, '-+', "M")                   /*is there an imaginary separator char?*/
        if s==0       then do; x=.; return; end  /*No sign?   Then isn't not an integer.*/
        z=substr(p, s+1)                         /*obtain the the imaginary part of  X. */
        x=  left(x, e+s)                         /*   "    "   "     real     "   "  "  */
        if isInt(z)   then if z\=0  then x=.     /*Not imaginary part=0? Not an integer.*/
        return                                   /*return to the invoker of this sub.   */
```

```txt

                                                   3.14   isn't an integer.
                                                1.00000      is an integer.
                                                     33      is an integer.
                                              999999999      is an integer.
                                            99999999999      is an integer.
                                                  1e272      is an integer.
                                                     AA   isn't an integer.
                                                      0      is an integer.
                                               1.000-3i   isn't an integer.
                                             1.000-3.3i   isn't an integer.
                                                     4j   isn't an integer.
                                          2.00000000+0j      is an integer.
                                                     0j      is an integer.
                                                    333      is an integer.
                                                   -1-i   isn't an integer.
                                                    1+i   isn't an integer.
                                                   .00i      is an integer.
                                                      j   isn't an integer.
                                             0003-00.0j      is an integer.
                                                  1.2d1      is an integer.
                                                2e55666      is an integer.
                                            +0003-00.0j      is an integer.
                                                    +0j      is an integer.
                                                 -.3q+2      is an integer.
                                                    -0i      is an integer.
                                    +03.0e+01+0.00e+20j      is an integer.
                                  -030.0e-001+0.0e-020j      is an integer.

```



### version 3

This REXX version is the same as the 2<sup>nd</sup> version, but it also supports multiple (abutted) unary operators.

I.E.:   ++30e-1   -   +0j

would be considered an integer   (extra blanks were added to show the number with more clarity).

```rexx
/*REXX program  tests  if a number  (possibly complex)  is  equivalent  to an integer.  */
numeric digits 3000                              /*be able to handle gihugic integers.  */
unaB= '++ -- -+ +-'                              /*a list of            unary operators.*/
unaA= '+  +  -  -'                               /*"   "   " translated   "       "     */
parse arg #s                                     /*obtain optional numbers list from CL.*/
if #s=''  then #s= '245+-00.0e-12i   245++++++0e+12j   --3450d-1----0.0d-1j' ,
                   '4.5e11111222223333344444555556666677777888889999900'
                                                 /* [↑]  use these numbers for defaults.*/
  do j=1  for words(#s);   ox=word(#s, j)        /*obtain a number from the numbers list*/
  parse  upper  var  ox  x                       /*obtain an uppercase version of  OX.  */
  x=translate(x, 'EEJ', "QDI")                   /*translate exponent and imag indicator*/

    do k=1  for words(unaB)                      /*process every possible unary operator*/
    _=word(unaB, k)                              /*a unary operator to be changed, maybe*/

      do  while  pos(_, x) \== 0                 /*keep changing until no more are left.*/
      x=changestr(_, x, word(unaA, k) )          /*reduce all unary operators  (if any).*/
      end   /*while*/
    end     /*k*/

  if right(x, 1)=='J'  then call tImag           /*has the  X  number an imaginary part?*/
  if isInt(x)  then say  right(ox, 55)  "     is an integer."      /*yuppers, it does.  */
               else say  right(ox, 55)  "  isn't an integer."      /*noppers, it doesn't*/
  end   /*j*/                                    /* [↑]  process each number in the list*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
isInt:  procedure;  parse arg n                  /*obtain the number in question.       */
        if datatype(n, 'Whole')  then return 1   /*it's a simple integer (small).       */
        parse  var  n   m  'E'  p                /*separate base from the  10's  power. */
        if \datatype(p, 'Numb')  then return 0   /*Not an integer if  P  not an integer.*/
        return  p>0  |  m=0                      /*is   power>0   or   mantissa = zero? */
/*──────────────────────────────────────────────────────────────────────────────────────*/
isSign: parse arg ? 2;   return ?=='+' | ?=="-"  /*a method to test for a leading sign. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
tImag:  x=left(x, length(x) -1)                  /*strip the trailing I or J from number*/
        if isInt(x)   then do                    /*is what's remaining an integer ?     */
                           if x\=0  then x=.     /*what's remaining isn't equal to zero.*/
                           return                /*return to invoker in either case.    */
                           end                   /* [↑]  handle simple imaginary case.  */
        if isSign(x)  then x=substr(x, 2)        /*X has a sign?  Strip the leading sign*/
        e=verify(x, .0123456789)                 /*find 1st char not a digit or a  dot. */
        if e==0       then do; x=.; return; end  /*Nothing?  Then it's not an integer.  */
        y=substr(x, e, 1)                        /*Y  is the suspect character.         */
        if isSign(y)  then do                    /*is suspect character a plus or minus?*/
                           z=substr(x, e+1)      /*obtain the imaginary part of  X.     */
                           x=  left(x, e-1)      /*   "    "    real      "   "  "      */
                           if isInt(z)  then if z=0  then return   /*imaginary part is 0*/
                           x=.                   /*the imaginary part isn't zero.       */
                           end                   /* [↑]  end of imaginary part of  X.   */
        if y\=='E'    then return                /*real part of X doesn't have an expon.*/
        p=substr(x, e+1)                         /*obtain power of real part of  X.     */
        _=  left(p, 1)                           /*obtain the possible sign of the power*/
        if isSign(_)  then p=substr(p, 2)        /*strip the sign from the exponent.    */
        s=verify(p, '-+', "M")                   /*is there an imaginary separator char?*/
        if s==0       then do; x=.; return; end  /*No sign?   Then isn't not an integer.*/
        z=substr(p, s+1)                         /*obtain the the imaginary part of  X. */
        x=  left(x, e+s)                         /*   "    "   "     real     "   "  "  */
        if isInt(z)   then if z\=0  then x=.     /*Not imaginary part=0? Not an integer.*/
        return                                   /*return to the invoker of this sub.   */
```

```txt

                                         245+-00.0e-12i      is an integer.
                                        245++++++0e+12j   isn't an integer.
                                   --3450d-1----0.0d-1j      is an integer.
    4.5e11111222223333344444555556666677777888889999900      is an integer.

```



## Ruby


Testing for integerness of floats, rationals and complex numbers:

```ruby

class Numeric
  def to_i?
    self == self.to_i rescue false
   end
end

# Demo
ar = [25.000000, 24.999999, 25.000100, -2.1e120, -5e-2,  # Floats
      Float::NAN, Float::INFINITY,                       # more Floats
      2r, 2.5r,                                          # Rationals
      2+0i, 2+0.0i, 5-5i]                                # Complexes

ar.each{|num| puts "#{num} integer? #{num.to_i?}" }

```

```txt

25.0 integer? true
24.999999 integer? false
25.0001 integer? false
-2.1e+120 integer? true
-0.05 integer? false
NaN integer? false
Infinity integer? false
2/1 integer? true
5/2 integer? false
2+0i integer? true
2+0.0i integer? false
5-5i integer? false

```


Ruby considers 2+0.0i to be inexact and raises an exception when the to_i method attempts to convert it to an integer. 2+0i is considered exact and converts to integer.


## Scheme


The '''Racket''' solution covers tests for integer? with the different numbers, and these all apply to Scheme.

Examples:


```txt

sash[r7rs]> (integer? 1)
#t
sash[r7rs]> (integer? 2/3)
#f
sash[r7rs]> (integer? 4/2)
#t
sash[r7rs]> (integer? 1+3i)
#f
sash[r7rs]> (integer? 1+0i)
#t
sash[r7rs]> (exact? 3.0)
#f
sash[r7rs]> (integer? 3.0)
#t
sash[r7rs]> (integer? 3.5)
#f
sash[r7rs]> (integer? 1.23e3)
#t
sash[r7rs]> (integer? 1.23e1)
#f
sash[r7rs]> (integer? 1e120)
#t

```



## Sidef


```ruby
func is_int (n, tolerance=0) {
    !!(abs(n.real.round + n.imag - n) <= tolerance)
}

%w(25.000000 24.999999 25.000100 -2.1e120 -5e-2 Inf NaN 5.0+0.0i 5-5i).each {|s|
    var n = Number(s)
    printf("%-10s  %-8s  %-5s\n", s,
        is_int(n),
        is_int(n, tolerance: 0.00001))
}
```

```txt

25.000000   true      true
24.999999   false     true
25.000100   false     false
-2.1e120    true      true
-5e-2       false     false
Inf         false     false
NaN         false     false
5.0+0.0i    true      true
5-5i        false     false

```



## Tcl


The simplest way is to test whether the value is (numerically) equal to itself cast as an integer.  entier() performs this cast without imposing any word-size limits (as int() or wide() would).


```tcl
proc isNumberIntegral {x} {
    expr {$x == entier($x)}
}
# test with various kinds of numbers:
foreach x {1e100 3.14 7 1.000000000000001 1000000000000000000000 -22.7 -123.000} {
    puts [format "%s: %s" $x [expr {[isNumberIntegral $x] ? "yes" : "no"}]]
}
```

```txt

1e100: yes
3.14: no
7: yes
1.000000000000001: no
1000000000000000000000: yes
-22.7: no
-123.000: yes

```


Note that 1.0000000000000001 will pass this integer test, because its difference from 1.0 is beyond the precision of an IEEE binary64 float.  This discrepancy will be visible in other languages, but perhaps more obvious in Tcl as such a value's string representation will persist:


```Tcl
% set a 1.0000000000000001
1.0000000000000001
% expr $a
1.0
% IsNumberIntegral $a
1
% puts $a
1.0000000000000001
```


compare Python:


```Python>>>
 a = 1.0000000000000001
>>> a
1.0
>>> 1.0 == 1.0000000000000001
True
```


.. this is a fairly benign illustration of why comparing floating point values with == is usually a bad idea.


## zkl

No complex type.

```zkl
T(1, 2.0,4.1,"nope",self).apply((1).isType)
```

```txt
L(True,False,False,False,False)
```

All is not golden as BigInts (lib GMP) don't consider themselves to be integers so the above test would fail. For that case:

```zkl
fcn isInt(x){ try{x==x.toInt()}catch{False}}
var BN=Import("zklBigNum");
 T(1, 2.0,4.1,"nope",self,BN(5)).apply(isInt);
```

```txt
L(True,True,False,False,False,True)
```

Note that the first float is now considered to have an integer equivalent.
