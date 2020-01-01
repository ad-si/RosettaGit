+++
title = "Welch's t-test"
description = ""
date = 2019-10-17T03:29:22Z
aliases = []
[extra]
id = 19182
[taxonomies]
categories = []
tags = []
+++

{{draft task|Probability and statistics}}

Given two lists of data, calculate the [[wp:p-value|p-value]] used for [[wp:Welch's_t_test|Welch's t-test]]. This is meant to translate R's <code>t.test(vector1, vector2, alternative="two.sided", var.equal=FALSE)</code> for calculation of the p-value.

'''Task Description'''


Given two sets of data, calculate the p-value:
    x = {3.0,4.0,1.0,2.1}
    y = {490.2,340.0,433.9}


Your task is to discern whether or not the difference in means between the two sets is statistically significant and worth further investigation.  P-values are significance tests to gauge the probability that the difference in means between two data sets is significant, or due to chance.  A threshold level, alpha, is usually chosen, 0.01 or 0.05, where p-values below alpha are worth further investigation and p-values above alpha are considered not significant.  The p-value is not considered a final test of significance, [http://www.nature.com/news/scientific-method-statistical-errors-1.14700 only whether the given variable should be given further consideration].

There is more than on way of calculating the [[wp:Student's_t-test|t-statistic]], and you must choose which method is appropriate for you.  Here we use [[wp:Welch's_t_test|Welch's t-test]], which assumes that the variances between the two sets <code>x</code> and <code>y</code> are not equal.  Welch's t-test statistic can be computed:

<math>t \quad = \quad {\; \overline{X}_1 - \overline{X}_2 \; \over \sqrt{ \; {s_1^2 \over N_1} \; + \; {s_2^2 \over N_2} \quad }} </math>

where

<math>\overline{X}_n </math> is the mean of set <math>n</math>,

and

<math>N_n</math> is the number of observations in set <math>n</math>,

and

<math>s_n </math> is the square root of the [[wp:Variance#Population_variance_and_sample_variance|unbiased sample variance]] of set <math>n</math>, i.e.

<math>s_n = \sqrt{\frac{1}{N_n-1} \sum_{i=1}^{N_n} \left(X_i - \overline{X}_n\right)^2} </math>

and the degrees of freedom, <math>\nu</math> can be approximated:

<math>\nu \quad  \approx \quad
 {{\left( \; {s_1^2 \over N_1} \; + \; {s_2^2 \over N_2} \; \right)^2 } \over
 { \quad {s_1^4 \over N_1^2 (N_1-1)} \; + \; {s_2^4 \over N_2^2 (N_2-1) } \quad }}</math>

The [[wp:One-_and_two-tailed_tests|two-tailed]] p-value, <math>p</math>, can be computed as a [[wp:Student's_t-distribution#Cumulative_distribution_function|cumulative distribution function]]

<math> p_{2-tail} = I_{\frac{\nu}{t^2+\nu}}\left(\frac{\nu}{2}, \frac{1}{2}\right) </math>

where I is the [[wp:Beta_function#Incomplete_beta_function|regularized incomplete beta function]].  This is the same as:

<math>p_{2-tail} = \frac{\Beta(\frac{\nu}{t^2+\nu};\frac{\nu}{2}, \frac{1}{2})}{\Beta(\frac{\nu}{2}, \frac{1}{2})}  </math>

Keeping in mind that

<math> \Beta(x;a,b) = \int_0^x r^{a-1}\,(1-r)^{b-1}\,\mathrm{d}r. \!</math>

and

<math>
 \Beta(x,y) = \dfrac{\Gamma(x)\,\Gamma(y)}{\Gamma(x+y)} =\exp(\ln\dfrac{\Gamma(x)\,\Gamma(y)}{\Gamma(x+y)}) = \exp((\ln(\Gamma(x)) + \ln(\Gamma(y)) - \ln(\Gamma(x+y)))
\!</math>

<math> p_{2-tail} </math> can be calculated in terms of [[wp:Gamma_function|gamma functions]] and integrals more simply:

<math> p_{2-tail}=\frac{\int_0^\frac{\nu}{t^2+\nu} r^{\frac{\nu}{2}-1}\,(1-r)^{-0.5}\,\mathrm{d}r}{\exp((\ln(\Gamma(\frac{\nu}{2})) + \ln(\Gamma(0.5)) - \ln(\Gamma(\frac{\nu}{2}+0.5)))} </math>

which simplifies to

<math> p_{2-tail} = \frac{\int_0^\frac{\nu}{t^2+\nu} \frac{r^{\frac{\nu}{2}-1}}{\sqrt{1-r}}\,\mathrm{d}r}{ \exp((\ln(\Gamma(\frac{\nu}{2})) + \ln(\Gamma(0.5)) - \ln(\Gamma(\frac{\nu}{2}+0.5))) }</math>

The definite integral can be approximated with [[wp:Simpson's_rule|Simpson's Rule]] but [http://rosettacode.org/wiki/Numerical_integration other methods] are also acceptable.

The <math>\ln(\Gamma(x))</math>, or <code>lgammal(x)</code> function is necessary for the program to work with large <code>a</code> values, as [http://rosettacode.org/wiki/Gamma_function Gamma functions] can often return values larger than can be handled by <code>double</code> or <code>long double</code> data types.   The <code>lgammal(x)</code> function is standard in <code>math.h</code> with C99 and C11 standards.


## C

{{works with|C99}}

Link with <code>-lm</code>

This program, for example, pvalue.c, can be compiled by

<code>clang -o pvalue pvalue.c -Wall -pedantic -std=c11 -lm -O3</code>

or

<code>gcc -o pvalue pvalue.c -Wall -pedantic -std=c11 -lm -O4</code>.

This shows how pvalue can be calculated from any two arrays, using Welch's 2-sided t-test, which doesn't assume equal variance.
This is the equivalent of R's<code>t.test(vector1,vector2, alternative="two.sided", var.equal=FALSE)</code>  and as such, it is compared against R's pvalues with the same vectors/arrays to show that the differences are very small (here 10^-14).

```c
#include <stdio.h>
#include <math.h>
#include <stdlib.h>

double Pvalue (const double *restrict ARRAY1, const size_t ARRAY1_SIZE, const double *restrict ARRAY2, const size_t ARRAY2_SIZE) {//calculate a p-value based on an array
	if (ARRAY1_SIZE <= 1) {
		return 1.0;
	} else if (ARRAY2_SIZE <= 1) {
		return 1.0;
	}
	double fmean1 = 0.0, fmean2 = 0.0;
	for (size_t x = 0; x < ARRAY1_SIZE; x++) {//get sum of values in ARRAY1
		if (isfinite(ARRAY1[x]) == 0) {//check to make sure this is a real numbere
			puts("Got a non-finite number in 1st array, can't calculate P-value.");
			exit(EXIT_FAILURE);
		}
		fmean1 += ARRAY1[x];
	}
	fmean1 /= ARRAY1_SIZE;
	for (size_t x = 0; x < ARRAY2_SIZE; x++) {//get sum of values in ARRAY2
		if (isfinite(ARRAY2[x]) == 0) {//check to make sure this is a real number
			puts("Got a non-finite number in 2nd array, can't calculate P-value.");
			exit(EXIT_FAILURE);
		}
		fmean2 += ARRAY2[x];
	}
	fmean2 /= ARRAY2_SIZE;
//	printf("mean1 = %lf	mean2 = %lf\n", fmean1, fmean2);
	if (fmean1 == fmean2) {
		return 1.0;//if the means are equal, the p-value is 1, leave the function
	}
	double unbiased_sample_variance1 = 0.0, unbiased_sample_variance2 = 0.0;
	for (size_t x = 0; x < ARRAY1_SIZE; x++) {//1st part of added unbiased_sample_variance
		unbiased_sample_variance1 += (ARRAY1[x]-fmean1)*(ARRAY1[x]-fmean1);
	}
	for (size_t x = 0; x < ARRAY2_SIZE; x++) {
		unbiased_sample_variance2 += (ARRAY2[x]-fmean2)*(ARRAY2[x]-fmean2);
	}
//	printf("unbiased_sample_variance1 = %lf\tunbiased_sample_variance2 = %lf\n",unbiased_sample_variance1,unbiased_sample_variance2);//DEBUGGING
	unbiased_sample_variance1 = unbiased_sample_variance1/(ARRAY1_SIZE-1);
	unbiased_sample_variance2 = unbiased_sample_variance2/(ARRAY2_SIZE-1);
	const double WELCH_T_STATISTIC = (fmean1-fmean2)/sqrt(unbiased_sample_variance1/ARRAY1_SIZE+unbiased_sample_variance2/ARRAY2_SIZE);
	const double DEGREES_OF_FREEDOM = pow((unbiased_sample_variance1/ARRAY1_SIZE+unbiased_sample_variance2/ARRAY2_SIZE),2.0)//numerator
	 /
	(
		(unbiased_sample_variance1*unbiased_sample_variance1)/(ARRAY1_SIZE*ARRAY1_SIZE*(ARRAY1_SIZE-1))+
		(unbiased_sample_variance2*unbiased_sample_variance2)/(ARRAY2_SIZE*ARRAY2_SIZE*(ARRAY2_SIZE-1))
	);
//	printf("Welch = %lf	DOF = %lf\n", WELCH_T_STATISTIC, DEGREES_OF_FREEDOM);
		const double a = DEGREES_OF_FREEDOM/2;
	double value = DEGREES_OF_FREEDOM/(WELCH_T_STATISTIC*WELCH_T_STATISTIC+DEGREES_OF_FREEDOM);
	if ((isinf(value) != 0) || (isnan(value) != 0)) {
		return 1.0;
	}
	if ((isinf(value) != 0) || (isnan(value) != 0)) {
		return 1.0;
	}

/*  Purpose:

    BETAIN computes the incomplete Beta function ratio.

  Licensing:

    This code is distributed under the GNU LGPL license.

  Modified:

    05 November 2010

  Author:

    Original FORTRAN77 version by KL Majumder, GP Bhattacharjee.
    C version by John Burkardt.

  Reference:

    KL Majumder, GP Bhattacharjee,
    Algorithm AS 63:
    The incomplete Beta Integral,
    Applied Statistics,
    Volume 22, Number 3, 1973, pages 409-411.

  Parameters:
https://www.jstor.org/stable/2346797?seq=1#page_scan_tab_contents
    Input, double X, the argument, between 0 and 1.

    Input, double P, Q, the parameters, which
    must be positive.

    Input, double BETA, the logarithm of the complete
    beta function.

    Output, int *IFAULT, error flag.
    0, no error.
    nonzero, an error occurred.

    Output, double BETAIN, the value of the incomplete
    Beta function ratio.
*/
	const double beta = lgammal(a)+0.57236494292470009-lgammal(a+0.5);
	const double acu = 0.1E-14;
  double ai;
  double cx;
  int indx;
  int ns;
  double pp;
  double psq;
  double qq;
  double rx;
  double temp;
  double term;
  double xx;

//  ifault = 0;
//Check the input arguments.
  if ( (a <= 0.0)) {// || (0.5 <= 0.0 )){
//    *ifault = 1;
//    return value;
  }
  if ( value < 0.0 || 1.0 < value )
  {
//    *ifault = 2;
    return value;
  }
/*
  Special cases.
*/
  if ( value == 0.0 || value == 1.0 )   {
    return value;
  }
  psq = a + 0.5;
  cx = 1.0 - value;

  if ( a < psq * value )
  {
    xx = cx;
    cx = value;
    pp = 0.5;
    qq = a;
    indx = 1;
  }
  else
  {
    xx = value;
    pp = a;
    qq = 0.5;
    indx = 0;
  }

  term = 1.0;
  ai = 1.0;
  value = 1.0;
  ns = ( int ) ( qq + cx * psq );
/*
  Use the Soper reduction formula.
*/
  rx = xx / cx;
  temp = qq - ai;
  if ( ns == 0 )
  {
    rx = xx;
  }

  for ( ; ; )
  {
    term = term * temp * rx / ( pp + ai );
    value = value + term;;
    temp = fabs ( term );

    if ( temp <= acu && temp <= acu * value )
    {
      value = value * exp ( pp * log ( xx )
      + ( qq - 1.0 ) * log ( cx ) - beta ) / pp;

      if ( indx )
      {
        value = 1.0 - value;
      }
      break;
    }

    ai = ai + 1.0;
    ns = ns - 1;

    if ( 0 <= ns )
    {
      temp = qq - ai;
      if ( ns == 0 )
      {
        rx = xx;
      }
    }
    else
    {
      temp = psq;
      psq = psq + 1.0;
    }
  }
  return value;
}
//-------------------
int main(void) {

	const double d1[] = {27.5,21.0,19.0,23.6,17.0,17.9,16.9,20.1,21.9,22.6,23.1,19.6,19.0,21.7,21.4};
	const double d2[] = {27.1,22.0,20.8,23.4,23.4,23.5,25.8,22.0,24.8,20.2,21.9,22.1,22.9,20.5,24.4};
	const double d3[] = {17.2,20.9,22.6,18.1,21.7,21.4,23.5,24.2,14.7,21.8};
	const double d4[] = {21.5,22.8,21.0,23.0,21.6,23.6,22.5,20.7,23.4,21.8,20.7,21.7,21.5,22.5,23.6,21.5,22.5,23.5,21.5,21.8};
	const double d5[] = {19.8,20.4,19.6,17.8,18.5,18.9,18.3,18.9,19.5,22.0};
	const double d6[] = {28.2,26.6,20.1,23.3,25.2,22.1,17.7,27.6,20.6,13.7,23.2,17.5,20.6,18.0,23.9,21.6,24.3,20.4,24.0,13.2};
	const double d7[] = {30.02,29.99,30.11,29.97,30.01,29.99};
	const double d8[] = {29.89,29.93,29.72,29.98,30.02,29.98};
	const double x[] = {3.0,4.0,1.0,2.1};
	const double y[] = {490.2,340.0,433.9};
	const double v1[] = {0.010268,0.000167,0.000167};
	const double v2[] = {0.159258,0.136278,0.122389};
	const double s1[] = {1.0/15,10.0/62.0};
	const double s2[] = {1.0/10,2/50.0};
	const double z1[] = {9/23.0,21/45.0,0/38.0};
	const double z2[] = {0/44.0,42/94.0,0/22.0};

	const double CORRECT_ANSWERS[] = {0.021378001462867,
0.148841696605327,
0.0359722710297968,
0.090773324285671,
0.0107515611497845,
0.00339907162713746,
0.52726574965384,
0.545266866977794};

//calculate the pvalues and show that they're the same as the R values

	double pvalue = Pvalue(d1,sizeof(d1)/sizeof(*d1),d2,sizeof(d2)/sizeof(*d2));
	double error = fabs(pvalue - CORRECT_ANSWERS[0]);
	printf("Test sets 1 p-value = %g\n", pvalue);

	pvalue = Pvalue(d3,sizeof(d3)/sizeof(*d3),d4,sizeof(d4)/sizeof(*d4));
	error += fabs(pvalue - CORRECT_ANSWERS[1]);
	printf("Test sets 2 p-value = %g\n",pvalue);

	pvalue = Pvalue(d5,sizeof(d5)/sizeof(*d5),d6,sizeof(d6)/sizeof(*d6));
	error += fabs(pvalue - CORRECT_ANSWERS[2]);
	printf("Test sets 3 p-value = %g\n", pvalue);

	pvalue = Pvalue(d7,sizeof(d7)/sizeof(*d7),d8,sizeof(d8)/sizeof(*d8));
	printf("Test sets 4 p-value = %g\n", pvalue);
	error += fabs(pvalue - CORRECT_ANSWERS[3]);

	pvalue = Pvalue(x,sizeof(x)/sizeof(*x),y,sizeof(y)/sizeof(*y));
	error += fabs(pvalue - CORRECT_ANSWERS[4]);
	printf("Test sets 5 p-value = %g\n", pvalue);

	pvalue = Pvalue(v1,sizeof(v1)/sizeof(*v1),v2,sizeof(v2)/sizeof(*v2));
	error += fabs(pvalue - CORRECT_ANSWERS[5]);
	printf("Test sets 6 p-value = %g\n", pvalue);

	pvalue = Pvalue(s1,sizeof(s1)/sizeof(*s1),s2,sizeof(s2)/sizeof(*s2));
	error += fabs(pvalue - CORRECT_ANSWERS[6]);
	printf("Test sets 7 p-value = %g\n", pvalue);

	pvalue = Pvalue(z1, 3, z2, 3);
	error += fabs(pvalue - CORRECT_ANSWERS[7]);
	printf("Test sets z p-value = %g\n", pvalue);

	printf("the cumulative error is %g\n", error);
	return 0;
}

```


{{out}}

```txt
Test sets 1 p-value = 0.021378
Test sets 2 p-value = 0.148842
Test sets 3 p-value = 0.0359723
Test sets 4 p-value = 0.0907733
Test sets 5 p-value = 0.0107516
Test sets 6 p-value = 0.00339907
Test sets 7 p-value = 0.527266
Test sets z p-value = 0.545267
the cumulative error is 1.06339e-14
```


'''If''' your computer does not have <code>lgammal</code>, add the following function before <code>main</code> and replace <code>lgammal</code> with <code>lngammal</code> in the <code>calculate_Pvalue</code> function:


```c
#include <stdio.h>
#include <math.h>

long double lngammal(const double xx) {
   unsigned int j;
   double x,y,tmp,ser;
   const double cof[6] = {
      76.18009172947146,    -86.50532032941677,
      24.01409824083091,    -1.231739572450155,
      0.1208650973866179e-2,-0.5395239384953e-5
   };

   y = x = xx;
   tmp = x + 5.5 - (x + 0.5) * logl(x + 5.5);
   ser = 1.000000000190015;
   for (j=0;j<=5;j++)
      ser += (cof[j] / ++y);
   return(log(2.5066282746310005 * ser / x) - tmp);
}


```



## Fortran


###  Using IMSL

Using IMSL '''TDF''' function. With Absoft Pro Fortran, compile with <code>af90 %FFLAGS% %LINK_FNL% pvalue.f90</code>.
Alternatively, the program shows the p-value computed using the IMSL '''BETAI''' function.


```fortran
subroutine welch_ttest(n1, x1, n2, x2, t, df, p)
    use tdf_int
    implicit none
    integer :: n1, n2
    double precision :: x1(n1), x2(n2)
    double precision :: m1, m2, v1, v2, t, df, p
    m1 = sum(x1) / n1
    m2 = sum(x2) / n2
    v1 = sum((x1 - m1)**2) / (n1 - 1)
    v2 = sum((x2 - m2)**2) / (n2 - 1)
    t = (m1 - m2) / sqrt(v1 / n1 + v2 / n2)
    df = (v1 / n1 + v2 / n2)**2 / &
         (v1**2 / (n1**2 * (n1 - 1)) + v2**2 / (n2**2 * (n2 - 1)))
    p = 2d0 * tdf(-abs(t), df)
end subroutine

program pvalue
    use betai_int
    implicit none
    double precision :: x(4) = [3d0, 4d0, 1d0, 2.1d0]
    double precision :: y(3) = [490.2d0, 340.0d0, 433.9d0]
    double precision :: t, df, p
    call welch_ttest(4, x, 3, y, t, df, p)
    print *, t, df, p
    print *, betai(df / (t**2 + df), 0.5d0 * df, 0.5d0)
end program
```


'''Output'''

```txt
  -9.55949772193266  2.00085234885628  1.075156114978449E-002
   1.075156114978449E-002
```



###  Using SLATEC


With Absoft Pro Fortran, compile with <code>af90 -m64 pvalue.f90 %SLATEC_LINK%</code>.


```fortran
subroutine welch_ttest(n1, x1, n2, x2, t, df, p)
    implicit none
    integer :: n1, n2
    double precision :: x1(n1), x2(n2)
    double precision :: m1, m2, v1, v2, t, df, p
    double precision :: dbetai

    m1 = sum(x1) / n1
    m2 = sum(x2) / n2
    v1 = sum((x1 - m1)**2) / (n1 - 1)
    v2 = sum((x2 - m2)**2) / (n2 - 1)
    t = (m1 - m2) / sqrt(v1 / n1 + v2 / n2)
    df = (v1 / n1 + v2 / n2)**2 / &
         (v1**2 / (n1**2 * (n1 - 1)) + v2**2 / (n2**2 * (n2 - 1)))
    p = dbetai(df / (t**2 + df), 0.5d0 * df, 0.5d0)
end subroutine

program pvalue
    implicit none
    double precision :: x(4) = [3d0, 4d0, 1d0, 2.1d0]
    double precision :: y(3) = [490.2d0, 340.0d0, 433.9d0]
    double precision :: t, df, p

    call welch_ttest(4, x, 3, y, t, df, p)
    print *, t, df, p
end program
```


'''Output'''


```txt
  -9.55949772193266  2.00085234885628  1.075156114978449E-002
```



## Go


```go
package main

import (
  "fmt"
  "math"
)

var (
  d1 = []float64{27.5, 21.0, 19.0, 23.6, 17.0, 17.9, 16.9, 20.1, 21.9, 22.6,
    23.1, 19.6, 19.0, 21.7, 21.4}
  d2 = []float64{27.1, 22.0, 20.8, 23.4, 23.4, 23.5, 25.8, 22.0, 24.8, 20.2,
    21.9, 22.1, 22.9, 20.5, 24.4}
  d3 = []float64{17.2, 20.9, 22.6, 18.1, 21.7, 21.4, 23.5, 24.2, 14.7, 21.8}
  d4 = []float64{21.5, 22.8, 21.0, 23.0, 21.6, 23.6, 22.5, 20.7, 23.4, 21.8,
    20.7, 21.7, 21.5, 22.5, 23.6, 21.5, 22.5, 23.5, 21.5, 21.8}
  d5 = []float64{19.8, 20.4, 19.6, 17.8, 18.5, 18.9, 18.3, 18.9, 19.5, 22.0}
  d6 = []float64{28.2, 26.6, 20.1, 23.3, 25.2, 22.1, 17.7, 27.6, 20.6, 13.7,
    23.2, 17.5, 20.6, 18.0, 23.9, 21.6, 24.3, 20.4, 24.0, 13.2}
  d7 = []float64{30.02, 29.99, 30.11, 29.97, 30.01, 29.99}
  d8 = []float64{29.89, 29.93, 29.72, 29.98, 30.02, 29.98}
  x  = []float64{3.0, 4.0, 1.0, 2.1}
  y  = []float64{490.2, 340.0, 433.9}
)

func main() {
  fmt.Printf("%.6f\n", pValue(d1, d2))
  fmt.Printf("%.6f\n", pValue(d3, d4))
  fmt.Printf("%.6f\n", pValue(d5, d6))
  fmt.Printf("%.6f\n", pValue(d7, d8))
  fmt.Printf("%.6f\n", pValue(x, y))
}

func mean(a []float64) float64 {
  sum := 0.
  for _, x := range a {
    sum += x
  }
  return sum / float64(len(a))
}

func sv(a []float64) float64 {
  m := mean(a)
  sum := 0.
  for _, x := range a {
    d := x - m
    sum += d * d
  }
  return sum / float64(len(a)-1)
}

func welch(a, b []float64) float64 {
  return (mean(a) - mean(b)) /
    math.Sqrt(sv(a)/float64(len(a))+sv(b)/float64(len(b)))
}

func dof(a, b []float64) float64 {
  sva := sv(a)
  svb := sv(b)
  n := sva/float64(len(a)) + svb/float64(len(b))
  return n * n /
    (sva*sva/float64(len(a)*len(a)*(len(a)-1)) +
      svb*svb/float64(len(b)*len(b)*(len(b)-1)))
}

func simpson0(n int, upper float64, f func(float64) float64) float64 {
  sum := 0.
  nf := float64(n)
  dx0 := upper / nf
  sum += f(0) * dx0
  sum += f(dx0*.5) * dx0 * 4
  x0 := dx0
  for i := 1; i < n; i++ {
    x1 := float64(i+1) * upper / nf
    xmid := (x0 + x1) * .5
    dx := x1 - x0
    sum += f(x0) * dx * 2
    sum += f(xmid) * dx * 4
    x0 = x1
  }
  return (sum + f(upper)*dx0) / 6
}

func pValue(a, b []float64) float64 {
  ν := dof(a, b)
  t := welch(a, b)
  g1, _ := math.Lgamma(ν / 2)
  g2, _ := math.Lgamma(.5)
  g3, _ := math.Lgamma(ν/2 + .5)
  return simpson0(2000, ν/(t*t+ν),
    func(r float64) float64 { return math.Pow(r, ν/2-1) / math.Sqrt(1-r) }) /
    math.Exp(g1+g2-g3)
}
```

{{out}}

```txt

0.021378
0.148842
0.035972
0.090773
0.010751

```



## J


Implementation:


```J
integrate=: adverb define
  'a b steps'=. 3{.y,128
  size=. (b - a)%steps
  size * +/ u |: 2 ]\ a + size * i.>:steps
)
simpson  =: adverb def '6 %~ +/ 1 1 4 * u y, -:+/y'

lngamma=: ^.@!@<:`(^.@!@(1 | ]) + +/@:^.@(1 + 1&| + i.@<.)@<:)@.(1&<:)"0
mean=: +/ % #
nu=: # - 1:
sampvar=: +/@((- mean) ^ 2:) % nu
ssem=: sampvar % #
welch_T=: -&mean % 2 %: +&ssem
nu=: nu f. : ((+&ssem ^ 2:) % +&((ssem^2:)%nu))
B=: ^@(+&lngamma - lngamma@+)

p2_tail=:dyad define
  t=. x welch_T y  NB. need numbers for numerical integration
  v=. x nu y
  F=. ^&(_1+v%2) % 2 %: 1&-
  lo=. 0
  hi=. v%(t^2)+v
  (F f. simpson integrate lo,hi) % 0.5 B v%2
)
```


<code>integrate</code> and <code>simpson</code> are from the [[Numerical_integration#J|Numerical integration]] task.

<code>lngamma</code> is  from http://www.jsoftware.com/pipermail/programming/2015-July/042174.html -- for values less than some convenient threshold (we use 1, but we could use a modestly higher threshold), we calculate it directly. For larger values we compute the fractional part directly and rebuild the log of the factorial using the sum of the logs.

<code>mean</code> is classic J - most J tutorials will include this

The initial definition of <code>nu</code> (degrees of freedom of a data set), as well as the combining form (approximating degrees of freedom for two sets of data) is from [[wp:Welch%27s_t_test#Calculations|Welch's t test]]. (Verb definitions can be forward referenced, even in J's tacit definitions, but it seems clearer to specify these definitions so they only depend on previously declared definitions.)

<code>sampvar</code> is sample variance (or: standard deviation squared)

<code>ssem</code> is squared standard error of the mean

Also... please ignore the highlighting of <code>v</code> in the definition of p2_tail. In this case, it's F that's the verb, v is just another number (the degrees of freedom for our two data sets. (But this is a hint that in explicit conjunction definitions, v would be the right verb argument. Unfortunately, the wiki's highlighting implementation is not capable of distinguishing that particular context from other contexts.)

Data for task examples:

```J
d1=: 27.5 21 19 23.6 17 17.9 16.9 20.1 21.9 22.6 23.1 19.6 19 21.7 21.4
d2=: 27.1 22 20.8 23.4 23.4 23.5 25.8 22 24.8 20.2 21.9 22.1 22.9 20.5 24.4
d3=: 17.2 20.9 22.6 18.1 21.7 21.4 23.5 24.2 14.7 21.8
d4=: 21.5 22.8 21 23 21.6 23.6 22.5 20.7 23.4 21.8 20.7 21.7 21.5 22.5 23.6 21.5 22.5 23.5 21.5 21.8
d5=: 19.8 20.4 19.6 17.8 18.5 18.9 18.3 18.9 19.5 22
d6=: 28.2 26.6 20.1 23.3 25.2 22.1 17.7 27.6 20.6 13.7 23.2 17.5 20.6 18 23.9 21.6 24.3 20.4 24 13.2
d7=: 30.02 29.99 30.11 29.97 30.01 29.99
d8=: 29.89 29.93 29.72 29.98 30.02 29.98
d9=: 3 4 1 2.1
da=: 490.2 340 433.9
```


Task examples:

```J
   d1 p2_tail d2
0.021378
   d3 p2_tail d4
0.148842
   d5 p2_tail d6
0.0359723
   d7 p2_tail d8
0.0907733
   d9 p2_tail da
0.0107377
```



## Java

Using the '''[http://commons.apache.org/proper/commons-math/ Apache Commons Mathematics Library]'''.

```java
import org.apache.commons.math3.distribution.TDistribution;

public class WelchTTest {
    public static double[] meanvar(double[] a) {
        double m = 0.0, v = 0.0;
        int n = a.length;

        for (double x: a) {
            m += x;
        }
        m /= n;

        for (double x: a) {
            v += (x - m) * (x - m);
        }
        v /= (n - 1);

        return new double[] {m, v};

    }

    public static double[] welch_ttest(double[] x, double[] y) {
        double mx, my, vx, vy, t, df, p;
        double[] res;
        int nx = x.length, ny = y.length;

        res = meanvar(x);
        mx = res[0];
        vx = res[1];

        res = meanvar(y);
        my = res[0];
        vy = res[1];

        t = (mx-my)/Math.sqrt(vx/nx+vy/ny);
        df = Math.pow(vx/nx+vy/ny, 2)/(vx*vx/(nx*nx*(nx-1))+vy*vy/(ny*ny*(ny-1)));
        TDistribution dist = new TDistribution(df);
        p = 2.0*dist.cumulativeProbability(-Math.abs(t));
        return new double[] {t, df, p};
    }

    public static void main(String[] args) {
        double x[] = {3.0, 4.0, 1.0, 2.1};
        double y[] = {490.2, 340.0, 433.9};
        double res[] = welch_ttest(x, y);
        System.out.println("t = " + res[0]);
        System.out.println("df = " + res[1]);
        System.out.println("p = " + res[2]);
    }
}
```


'''Result'''


```txt
javac -cp .;L:\java\commons-math3-3.6.1.jar WelchTTest.java
java -cp .;L:\java\commons-math3-3.6.1.jar WelchTTest
t = -9.559497721932658
df = 2.0008523488562844
p = 0.010751561149784485
```



## Julia

{{works with|Julia|0.6}}


```julia
using HypothesisTests

d1 = [27.5, 21.0, 19.0, 23.6, 17.0, 17.9, 16.9, 20.1, 21.9, 22.6, 23.1, 19.6, 19.0, 21.7, 21.4]
d2 = [27.1, 22.0, 20.8, 23.4, 23.4, 23.5, 25.8, 22.0, 24.8, 20.2, 21.9, 22.1, 22.9, 20.5, 24.4]

d3 = [17.2, 20.9, 22.6, 18.1, 21.7, 21.4, 23.5, 24.2, 14.7, 21.8]
d4 = [21.5, 22.8, 21.0, 23.0, 21.6, 23.6, 22.5, 20.7, 23.4, 21.8, 20.7, 21.7, 21.5, 22.5, 23.6, 21.5, 22.5, 23.5, 21.5, 21.8]

d5 = [19.8, 20.4, 19.6, 17.8, 18.5, 18.9, 18.3, 18.9, 19.5, 22.0]
d6 = [28.2, 26.6, 20.1, 23.3, 25.2, 22.1, 17.7, 27.6, 20.6, 13.7, 23.2, 17.5, 20.6, 18.0, 23.9, 21.6, 24.3, 20.4, 24.0, 13.2]

d7 = [30.02, 29.99, 30.11, 29.97, 30.01, 29.99]
d8 = [29.89, 29.93, 29.72, 29.98, 30.02, 29.98]

x = [  3.0,   4.0,   1.0, 2.1]
y = [490.2, 340.0, 433.9]

for (y1, y2) in ((d1, d2), (d3, d4), (d5, d6), (d7, d8), (x, y))
    ttest = UnequalVarianceTTest(y1, y2)
    println("\nData:\n  y1 = $y1\n  y2 = $y2\nP-value for unequal variance TTest: ", round(pvalue(ttest), 4))
end
```


{{out}}

```txt

Data:
  y1 = [27.5, 21.0, 19.0, 23.6, 17.0, 17.9, 16.9, 20.1, 21.9, 22.6, 23.1, 19.6, 19.0, 21.7, 21.4]
  y2 = [27.1, 22.0, 20.8, 23.4, 23.4, 23.5, 25.8, 22.0, 24.8, 20.2, 21.9, 22.1, 22.9, 20.5, 24.4]
P-value for unequal variance TTest: 0.0214

Data:
  y1 = [17.2, 20.9, 22.6, 18.1, 21.7, 21.4, 23.5, 24.2, 14.7, 21.8]
  y2 = [21.5, 22.8, 21.0, 23.0, 21.6, 23.6, 22.5, 20.7, 23.4, 21.8, 20.7, 21.7, 21.5, 22.5, 23.6, 21.5, 22.5, 23.5, 21.5, 21.8]
P-value for unequal variance TTest: 0.1488

Data:
  y1 = [19.8, 20.4, 19.6, 17.8, 18.5, 18.9, 18.3, 18.9, 19.5, 22.0]
  y2 = [28.2, 26.6, 20.1, 23.3, 25.2, 22.1, 17.7, 27.6, 20.6, 13.7, 23.2, 17.5, 20.6, 18.0, 23.9, 21.6, 24.3, 20.4, 24.0, 13.2]
P-value for unequal variance TTest: 0.036

Data:
  y1 = [30.02, 29.99, 30.11, 29.97, 30.01, 29.99]
  y2 = [29.89, 29.93, 29.72, 29.98, 30.02, 29.98]
P-value for unequal variance TTest: 0.0908

Data:
  y1 = [3.0, 4.0, 1.0, 2.1]
  y2 = [490.2, 340.0, 433.9]
P-value for unequal variance TTest: 0.0108

```



## Kotlin

This program brings in code from other tasks for gamma functions and integration by Simpson's rule as Kotlin doesn't have these built-in:

```scala
// version 1.1.4-3

typealias Func = (Double) -> Double

fun square(d: Double) = d * d

fun sampleVar(da: DoubleArray): Double {
    if (da.size < 2) throw IllegalArgumentException("Array must have at least 2 elements")
    val m = da.average()
    return da.map { square(it - m) }.sum() / (da.size - 1)
}

fun welch(da1: DoubleArray, da2: DoubleArray): Double {
    val temp = sampleVar(da1) / da1.size + sampleVar(da2) / da2.size
    return (da1.average() - da2.average()) / Math.sqrt(temp)
}

fun degreesFreedom(da1: DoubleArray, da2: DoubleArray): Double {
    val s1 = sampleVar(da1)
    val s2 = sampleVar(da2)
    val n1 = da1.size
    val n2 = da2.size
    val temp1 = square(s1 / n1 + s2 / n2)
    val temp2 = square(s1) / (n1 * n1 * (n1 - 1)) + square(s2) / (n2 * n2 * (n2 - 1))
    return temp1 / temp2
}

fun gamma(d: Double): Double {
    var dd = d
    val p = doubleArrayOf(
        0.99999999999980993,
      676.5203681218851,
    -1259.1392167224028,
      771.32342877765313,
     -176.61502916214059,
       12.507343278686905,
       -0.13857109526572012,
        9.9843695780195716e-6,
        1.5056327351493116e-7
    )
    val g = 7
    if (dd < 0.5) return Math.PI / (Math.sin(Math.PI * dd) * gamma(1.0 - dd))
    dd--
    var a = p[0]
    val t = dd + g + 0.5
    for (i in 1 until p.size) a += p[i] / (dd + i)
    return Math.sqrt(2.0 * Math.PI) * Math.pow(t, dd + 0.5) * Math.exp(-t) * a
}

fun lGamma(d: Double) = Math.log(gamma(d))

fun simpson(a: Double, b: Double, n: Int, f: Func): Double {
    val h = (b - a) / n
    var sum = 0.0
    for (i in 0 until n) {
        val x = a + i * h
        sum += (f(x) + 4.0 * f(x + h / 2.0) + f(x + h)) / 6.0
    }
    return sum * h
}

fun p2Tail(da1: DoubleArray, da2: DoubleArray): Double {
    val nu = degreesFreedom(da1, da2)
    val t = welch(da1, da2)
    val g = Math.exp(lGamma(nu / 2.0) + lGamma(0.5) - lGamma(nu / 2.0 + 0.5))
    val b = nu / (t * t + nu)
    val f: Func = { r ->  Math.pow(r, nu / 2.0 - 1.0) / Math.sqrt(1.0 - r) }
    return simpson(0.0, b, 10000, f) / g   // n = 10000 seems more than enough here
}

fun main(args: Array<String>) {
    val da1 = doubleArrayOf(
        27.5, 21.0, 19.0, 23.6, 17.0, 17.9, 16.9, 20.1, 21.9, 22.6,
        23.1, 19.6, 19.0, 21.7, 21.4
    )
    val da2 = doubleArrayOf(
        27.1, 22.0, 20.8, 23.4, 23.4, 23.5, 25.8, 22.0, 24.8, 20.2,
        21.9, 22.1, 22.9, 20.5, 24.4
    )
    val da3 = doubleArrayOf(
        17.2, 20.9, 22.6, 18.1, 21.7, 21.4, 23.5, 24.2, 14.7, 21.8
    )
    val da4 = doubleArrayOf(
        21.5, 22.8, 21.0, 23.0, 21.6, 23.6, 22.5, 20.7, 23.4, 21.8,
        20.7, 21.7, 21.5, 22.5, 23.6, 21.5, 22.5, 23.5, 21.5, 21.8
    )
    val da5 = doubleArrayOf(
        19.8, 20.4, 19.6, 17.8, 18.5, 18.9, 18.3, 18.9, 19.5, 22.0
    )
    val da6 = doubleArrayOf(
        28.2, 26.6, 20.1, 23.3, 25.2, 22.1, 17.7, 27.6, 20.6, 13.7,
        23.2, 17.5, 20.6, 18.0, 23.9, 21.6, 24.3, 20.4, 24.0, 13.2
    )
    val da7 = doubleArrayOf(30.02, 29.99, 30.11, 29.97, 30.01, 29.99)
    val da8 = doubleArrayOf(29.89, 29.93, 29.72, 29.98, 30.02, 29.98)

    val x = doubleArrayOf(3.0, 4.0, 1.0, 2.1)
    val y = doubleArrayOf(490.2, 340.0, 433.9)

    val f = "%.6f"
    println(f.format(p2Tail(da1, da2)))
    println(f.format(p2Tail(da3, da4)))
    println(f.format(p2Tail(da5, da6)))
    println(f.format(p2Tail(da7, da8)))
    println(f.format(p2Tail(x, y)))
}
```


{{out}}

```txt

0.021378
0.148842
0.035972
0.090773
0.010751

```



## Octave

{{trans|Stata}}

```octave
x = [3.0,4.0,1.0,2.1];
y = [490.2,340.0,433.9];
n1 = length(x);
n2 = length(y);
v1 = var(x);
v2 = var(y);
t = (mean(x)-mean(y))/(sqrt(v1/n1+v2/n2));
df = (v1/n1+v2/n2)^2/(v1^2/(n1^2*(n1-1))+v2^2/(n2^2*(n2-1)));
p = betainc(df/(t^2+df),df/2,1/2);
[t df p]

ans =

  -9.559498   2.000852   0.010752
```



## PARI/GP


```parigp
B2(x,y)=exp(lngamma(x)+lngamma(y)-lngamma(x+y))
B3(x,a,b)=a--;b--;intnum(r=0,x,r^a*(1-r)^b)
Welch2(u,v)=my(m1=vecsum(u)/#u, m2=vecsum(v)/#v, v1=var(u,m1), v2=var(v,m2), s=v1/#u+v2/#v, t=(m1-m2)/sqrt(s), nu=s^2/(v1^2/#u^2/(#u-1)+v2^2/#v^2/(#v-1))); B3(nu/(t^2+nu),nu/2,1/2)/B2(nu/2,1/2);
Welch2([3,4,1,2.1], [490.2,340,433.9])
```

{{out}}

```txt
%1 = 0.010751561149784496723954539777213062928
```



## Perl


###  Using Math::AnyNum

Uses Math::AnyNum for gamma and pi.  It is possible to use some other modules (e.g. Math::Cephes) if Math::AnyNum has problematic dependencies.
{{trans|Sidef}}

```perl
use utf8;
use List::Util qw(sum);
use Math::AnyNum qw(gamma pi);

sub p_value ($$) {
    my ($A, $B) = @_;

    (@$A > 1 && @$B > 1) || return 1;

    my $x̄_a = sum(@$A) / @$A;
    my $x̄_b = sum(@$B) / @$B;

    my $a_var = sum(map { ($x̄_a - $_)**2 } @$A) / (@$A - 1);
    my $b_var = sum(map { ($x̄_b - $_)**2 } @$B) / (@$B - 1);

    ($a_var && $b_var) || return 1;

    my $Welsh_𝒕_statistic = ($x̄_a - $x̄_b) / sqrt($a_var/@$A + $b_var/@$B);

    my $DoF = ($a_var/@$A + $b_var/@$B)**2 / (
               $a_var**2 / (@$A**3 - @$A**2) +
               $b_var**2 / (@$B**3 - @$B**2));

    my $sa = $DoF / 2 - 1;
    my $x  = $DoF / ($Welsh_𝒕_statistic**2 + $DoF);
    my $N  = 65355;
    my $h  = $x / $N;

    my ($sum1, $sum2) = (0, 0);

    foreach my $k (0 .. $N - 1) {
        my $i = $h * $k;
        $sum1 += ($i + $h/2)**$sa / sqrt(1 - ($i + $h/2));
        $sum2 += $i**$sa / sqrt(1-$i);
    }

    ($h/6 * ($x**$sa / sqrt(1-$x) + 4*$sum1 + 2*$sum2) /
        (gamma($sa + 1) * sqrt(pi) / gamma($sa + 1.5)))->numify;
}

my @tests = (
    [27.5, 21.0, 19.0, 23.6, 17.0, 17.9, 16.9, 20.1, 21.9, 22.6, 23.1, 19.6, 19.0, 21.7, 21.4],
    [27.1, 22.0, 20.8, 23.4, 23.4, 23.5, 25.8, 22.0, 24.8, 20.2, 21.9, 22.1, 22.9, 20.5, 24.4],

    [17.2, 20.9, 22.6, 18.1, 21.7, 21.4, 23.5, 24.2, 14.7, 21.8],
    [21.5, 22.8, 21.0, 23.0, 21.6, 23.6, 22.5, 20.7, 23.4, 21.8, 20.7, 21.7, 21.5, 22.5, 23.6, 21.5, 22.5, 23.5, 21.5, 21.8],

    [19.8, 20.4, 19.6, 17.8, 18.5, 18.9, 18.3, 18.9, 19.5, 22.0],
    [28.2, 26.6, 20.1, 23.3, 25.2, 22.1, 17.7, 27.6, 20.6, 13.7, 23.2, 17.5, 20.6, 18.0, 23.9, 21.6, 24.3, 20.4, 24.0, 13.2],

    [30.02, 29.99, 30.11, 29.97, 30.01, 29.99],
    [29.89, 29.93, 29.72, 29.98, 30.02, 29.98],

    [3.0,   4.0,   1.0, 2.1],
    [490.2, 340.0, 433.9],
);

while (@tests) {
    my ($left, $right) = splice(@tests, 0, 2);
    print p_value($left, $right), "\n";
}
```

{{out}}

```txt

0.0213780014628667
0.148841696605327
0.0359722710297968
0.0907733242856612
0.0107515340333929

```


=== Using Burkardt's betain ===
We use a slightly more accurate lgamma than the C code.  Note that Perl can be compiled with different underlying floating point representations -- double, long double, or quad double.
{{trans|C}}

```perl

use warnings;
use strict;

sub lgamma {
  my $x = shift;
  my $log_sqrt_two_pi = 0.91893853320467274178;
  my @lanczos_coef = (
      0.99999999999980993, 676.5203681218851, -1259.1392167224028,
      771.32342877765313, -176.61502916214059, 12.507343278686905,
      -0.13857109526572012, 9.9843695780195716e-6, 1.5056327351493116e-7 );
  my $base = $x + 7.5;
  my $sum = 0;
  $sum += $lanczos_coef[$_] / ($x + $_)  for reverse (1..8);
  $sum += $lanczos_coef[0];
  $sum = $log_sqrt_two_pi + log($sum/$x) + ( ($x+0.5)*log($base) - $base );
  $sum;
}

use List::Util 'sum';
sub calculate_Pvalue {
	my $array1 = shift;
	my $array2 = shift;
	if (scalar @$array1 <= 1) {
	return 1.0;
	}
	if (scalar @$array2 <= 1) {
	return 1.0;
	}
	my $mean1 = sum(@{ $array1 });
	$mean1 /= scalar @$array1;
	my $mean2 = sum(@{ $array2 });
	$mean2 /= scalar @$array2;
	if ($mean1 == $mean2) {
	return 1.0;
	}
	my $variance1 = 0.0;
	my $variance2 = 0.0;
	foreach my $x (@$array1) {
	$variance1 += ($x-$mean1)*($x-$mean1);
	}
	foreach my $x (@$array2) {
	$variance2 += ($x-$mean2)*($x-$mean2);
	}
	if (($variance1 == 0.0) && ($variance2 == 0.0)) {
	return 1.0;
	}
	$variance1 = $variance1/(scalar @$array1-1);
	$variance2 = $variance2/(scalar @$array2-1);
	my $array1_size = scalar @$array1;
	my $array2_size = scalar @$array2;
	my $WELCH_T_STATISTIC = ($mean1-$mean2)/sqrt($variance1/$array1_size+$variance2/$array2_size);
	my $DEGREES_OF_FREEDOM = (($variance1/$array1_size+$variance2/(scalar @$array2))**2)
	/
	(
	($variance1*$variance1)/($array1_size*$array1_size*($array1_size-1))+
	($variance2*$variance2)/($array2_size*$array2_size*($array2_size-1))
	);
	my $A = $DEGREES_OF_FREEDOM/2;
	my $value = $DEGREES_OF_FREEDOM/($WELCH_T_STATISTIC*$WELCH_T_STATISTIC+$DEGREES_OF_FREEDOM);
#from here, translation of John Burkhardt's C
	my $beta = lgamma($A)+0.57236494292470009-lgamma($A+0.5);
	my $acu = 10**(-15);
	my($ai,$cx,$indx,$ns,$pp,$psq,$qq,$rx,$temp,$term,$xx);
# Check the input arguments.
	return $value if $A <= 0.0;# || $q <= 0.0;
	return $value if $value < 0.0 || 1.0 < $value;
# Special cases
	return $value if $value == 0.0 || $value == 1.0;
	$psq = $A + 0.5;
	$cx = 1.0 - $value;
	if ($A < $psq * $value) {
		($xx, $cx, $pp, $qq, $indx) = ($cx, $value, 0.5, $A, 1);
	} else {
		($xx, $pp, $qq, $indx) = ($value, $A, 0.5, 0);
	}
	$term = 1.0;
	$ai = 1.0;
	$value = 1.0;
	$ns = int($qq + $cx * $psq);
#Soper reduction formula.
	$rx = $xx / $cx;
	$temp = $qq - $ai;
	$rx = $xx if $ns == 0;
	while (1) {
		$term = $term * $temp * $rx / ( $pp + $ai );
		$value = $value + $term;
		$temp = abs ($term);
		if ($temp <= $acu && $temp <= $acu * $value) {
	   	$value = $value * exp ($pp * log($xx)
	                          + ($qq - 1.0) * log($cx) - $beta) / $pp;
	   	$value = 1.0 - $value if $indx;
	   	last;
		}
	 	$ai = $ai + 1.0;
		$ns = $ns - 1;
		if (0 <= $ns) {
			$temp = $qq - $ai;
			$rx = $xx if $ns == 0;
		} else {
			$temp = $psq;
			$psq = $psq + 1.0;
		}
	}
	return $value;
}

my @answers = (
0.021378001462867,
0.148841696605327,
0.0359722710297968,
0.090773324285671,
0.0107515611497845,
0.00339907162713746,
0.52726574965384,
0.545266866977794,
);

my @tests = (
    [27.5,21.0,19.0,23.6,17.0,17.9,16.9,20.1,21.9,22.6,23.1,19.6,19.0,21.7,21.4],
    [27.1,22.0,20.8,23.4,23.4,23.5,25.8,22.0,24.8,20.2,21.9,22.1,22.9,20.5,24.4],

    [17.2,20.9,22.6,18.1,21.7,21.4,23.5,24.2,14.7,21.8],
    [21.5,22.8,21.0,23.0,21.6,23.6,22.5,20.7,23.4,21.8,20.7,21.7,21.5,22.5,23.6,21.5,22.5,23.5,21.5,21.8],

    [19.8,20.4,19.6,17.8,18.5,18.9,18.3,18.9,19.5,22.0],
    [28.2,26.6,20.1,23.3,25.2,22.1,17.7,27.6,20.6,13.7,23.2,17.5,20.6,18.0,23.9,21.6,24.3,20.4,24.0,13.2],

    [30.02,29.99,30.11,29.97,30.01,29.99],
    [29.89,29.93,29.72,29.98,30.02,29.98],

    [3.0,4.0,1.0,2.1],
    [490.2,340.0,433.9],

    [0.010268,0.000167,0.000167],
    [0.159258,0.136278,0.122389],

    [1.0/15,10.0/62.0],
    [1.0/10,2/50.0],

    [9/23.0,21/45.0,0/38.0],
    [0/44.0,42/94.0,0/22.0],
);

my $error = 0;
while (@tests) {
    my ($left, $right) = splice(@tests, 0, 2);
    my $pvalue = calculate_Pvalue($left,$right);
    $error += abs($pvalue - shift @answers);
    printf("p-value = %.14g\n",$pvalue);
}
printf("cumulative error is %g\n", $error);
```

{{out}}

```txt
p-value = 0.021378001462867
p-value = 0.14884169660533
p-value = 0.035972271029797
p-value = 0.090773324285661
p-value = 0.010751561149784
p-value = 0.0033990716271375
p-value = 0.52726574965384
p-value = 0.54526686697779
cumulative error is 1.11139e-14
```



## Perl 6


=== Integration using Simpson's Rule ===

{{works with|Rakudo|2017.08}}
{{trans|C}}
Perhaps "inspired by C example" may be more accurate. Gamma subroutine from [[Gamma_function#Perl_6 |Gamma function task]].


```perl6
sub Γ(\z) {
    constant g = 9;
    z < .5 ?? π / sin(π * z) / Γ(1 - z) !!
    τ.sqrt * (z + g - 1/2)**(z - 1/2) *
    exp(-(z + g - 1/2)) *
    [+] <
        1.000000000000000174663
     5716.400188274341379136
   -14815.30426768413909044
    14291.49277657478554025
    -6348.160217641458813289
     1301.608286058321874105
     -108.1767053514369634679
        2.605696505611755827729
       -0.7423452510201416151527e-2
        0.5384136432509564062961e-7
       -0.4023533141268236372067e-8
    > Z* 1, |map 1/(z + *), 0..*
}

sub p-value (@A, @B) {
    return 1 if @A <= 1 or @B <= 1;

    my $a-mean = @A.sum / @A;
    my $b-mean = @B.sum / @B;
    my $a-variance = @A.map( { ($a-mean - $_)² } ).sum / (@A - 1);
    my $b-variance = @B.map( { ($b-mean - $_)² } ).sum / (@B - 1);
    return 1 unless $a-variance && $b-variance;

    my \Welsh-𝒕-statistic = ($a-mean - $b-mean)/($a-variance/@A + $b-variance/@B).sqrt;

    my $DoF = ($a-variance / @A + $b-variance / @B)² /
              (($a-variance² / (@A³ - @A²)) + ($b-variance² / (@B³ - @B²)));

    my $sa = $DoF / 2 - 1;
    my $x = $DoF / (Welsh-𝒕-statistic² + $DoF);
    my $N = 65355;
    my $h = $x / $N;
    my ( $sum1, $sum2 );

    for ^$N »*» $h -> $i {
        $sum1 += (($i + $h / 2) ** $sa) / (1 - ($i + $h / 2)).sqrt;
        $sum2 +=   $i           ** $sa  / (1 -  $i).sqrt;
    }

    (($h / 6) * ( $x ** $sa / (1 - $x).sqrt + 4 * $sum1 + 2 * $sum2)) /
     ( Γ($sa + 1) * π.sqrt / Γ($sa + 1.5) );
}

# Testing
for (
  [<27.5 21.0 19.0 23.6 17.0 17.9 16.9 20.1 21.9 22.6 23.1 19.6 19.0 21.7 21.4>],
  [<27.1 22.0 20.8 23.4 23.4 23.5 25.8 22.0 24.8 20.2 21.9 22.1 22.9 20.5 24.4>],

  [<17.2 20.9 22.6 18.1 21.7 21.4 23.5 24.2 14.7 21.8>],
  [<21.5 22.8 21.0 23.0 21.6 23.6 22.5 20.7 23.4 21.8 20.7 21.7 21.5 22.5 23.6 21.5 22.5 23.5 21.5 21.8>],

  [<19.8 20.4 19.6 17.8 18.5 18.9 18.3 18.9 19.5 22.0>],
  [<28.2 26.6 20.1 23.3 25.2 22.1 17.7 27.6 20.6 13.7 23.2 17.5 20.6 18.0 23.9 21.6 24.3 20.4 24.0 13.2>],

  [<30.02 29.99 30.11 29.97 30.01 29.99>],
  [<29.89 29.93 29.72 29.98 30.02 29.98>],

  [<3.0 4.0 1.0 2.1>],
  [<490.2 340.0 433.9>]
) -> @left, @right { say p-value @left, @right }
```

{{out}}

```txt
0.0213780014628669
0.148841696605328
0.0359722710297969
0.0907733242856673
0.010751534033393

```


=== Using Burkardt's betain ===

{{works with|Rakudo|2018.10}}
{{trans|Perl}}

This uses the Soper reduction formula to evaluate the integral, which converges much more quickly than Simpson's formula.


```perl6
sub lgamma ( Num(Real) \n --> Num ){
  use NativeCall;
  sub lgamma (num64 --> num64) is native {}
  lgamma( n )
}

sub pvalue (@a, @b) {
  if @a.elems <= 1 {
    return 1.0;
  }
  if @b.elems <= 1 {
    return 1.0;
  }
  my Rat $mean1 = @a.sum / @a.elems;
  my Rat $mean2 = @b.sum / @b.elems;
  if $mean1 == $mean2 {
    return 1.0;
  }
  my Rat $variance1 = 0.0;
  my Rat $variance2 = 0.0;

  for @a -> $i {
    $variance1 += ($mean1 - $i)**2#";" unnecessary for last statement in block
  }
  for @b -> $i {
    $variance2 += ($mean2 - $i)**2
  }
  if ($variance1 == 0 && $variance2 == 0) {
    return 1.0;
  }
  $variance1 /= (@a.elems - 1);
  $variance2 /= (@b.elems - 1);

  my $WELCH_T_STATISTIC = ($mean1-$mean2)/sqrt($variance1/@a.elems+$variance2/@b.elems);
	my $DEGREES_OF_FREEDOM = (($variance1/@a.elems+$variance2/@b.elems)**2)
	/
	(
	($variance1*$variance1)/(@a.elems*@a.elems*(@a.elems-1))+
	($variance2*$variance2)/(@b.elems*@b.elems*(@b.elems-1))
	);
	my $A = $DEGREES_OF_FREEDOM/2;
	my $value = $DEGREES_OF_FREEDOM/($WELCH_T_STATISTIC*$WELCH_T_STATISTIC+$DEGREES_OF_FREEDOM);
  my $beta = lgamma($A)+0.57236494292470009-lgamma($A+0.5);
	my Rat $acu = 10**(-15);
	my ($ai,$cx,$indx,$ns,$pp,$psq,$qq,$rx,$temp,$term,$xx);
# Check the input arguments.
	return $value if $A <= 0.0;# || $q <= 0.0;
	return $value if $value < 0.0 || 1.0 < $value;
# Special cases
	return $value if $value == 0.0 || $value == 1.0;
	$psq = $A + 0.5;
	$cx = 1.0 - $value;
	if $A < $psq * $value {
		($xx, $cx, $pp, $qq, $indx) = ($cx, $value, 0.5, $A, 1);
	} else {
		($xx, $pp, $qq, $indx) = ($value, $A, 0.5, 0);
	}
	$term = 1.0;
	$ai = 1.0;
	$value = 1.0;
	$ns = $qq + $cx * $psq;
        $ns = $ns.Int;
#Soper reduction formula.
	$rx = $xx / $cx;
	$temp = $qq - $ai;
	$rx = $xx if $ns == 0;
	while (True) {
		$term = $term * $temp * $rx / ( $pp + $ai );
		$value = $value + $term;
		$temp = $term.abs;
		if $temp <= $acu && $temp <= $acu * $value {
	   	$value = $value * ($pp * $xx.log + ($qq - 1.0) * $cx.log - $beta).exp / $pp;
	   	$value = 1.0 - $value if $indx;
	   	last;
		}
	 	$ai = $ai + 1.0;
		$ns--;
		if 0 <= $ns {
			$temp = $qq - $ai;
			$rx = $xx if $ns == 0;
		} else {
			$temp = $psq;
			$psq = $psq + 1.0;
		}
	}
	return $value;
}

my $error = 0;
my @answers = (
0.021378001462867,
0.148841696605327,
0.0359722710297968,
0.090773324285671,
0.0107515611497845,
0.00339907162713746,
0.52726574965384,
0.545266866977794,
);

for (
    [< 27.5 21.0 19.0 23.6 17.0 17.9 16.9 20.1 21.9 22.6 23.1 19.6 19.0 21.7 21.4>],
    [< 27.1 22.0 20.8 23.4 23.4 23.5 25.8 22.0 24.8 20.2 21.9 22.1 22.9 20.5 24.4>],

    [< 17.2 20.9 22.6 18.1 21.7 21.4 23.5 24.2 14.7 21.8>],
    [< 21.5 22.8 21.0 23.0 21.6 23.6 22.5 20.7 23.4 21.8 20.7 21.7 21.5 22.5 23.6 21.5 22.5 23.5 21.5 21.8>],

    [< 19.8 20.4 19.6 17.8 18.5 18.9 18.3 18.9 19.5 22.0>],
    [< 28.2 26.6 20.1 23.3 25.2 22.1 17.7 27.6 20.6 13.7 23.2 17.5 20.6 18.0 23.9 21.6 24.3 20.4 24.0 13.2>],

    [< 30.02 29.99 30.11 29.97 30.01 29.99>],
    [< 29.89 29.93 29.72 29.98 30.02 29.98>],

    [< 3.0 4.0 1.0 2.1>],
    [< 490.2 340.0 433.9>],

    [< 0.010268 0.000167 0.000167>],
    [< 0.159258 0.136278 0.122389>],

    [< 1.0/15 10.0/62.0>],
    [< 1.0/10 2/50.0>],

    [< 9/23.0 21/45.0 0/38.0>],
    [< 0/44.0 42/94.0 0/22.0>],
) -> @left, @right {
    my $pvalue = p-value @left, @right;
    printf("p-value = %.14g\n",$pvalue);
    $error += abs($pvalue - shift @answers);
}
printf("cumulative error is %g\n", $error);
```

{{out}}

```txt
p-value = 0.021378001462867
p-value = 0.14884169660533
p-value = 0.035972271029797
p-value = 0.090773324285667
p-value = 0.010751561149784
p-value = 0.0033990716271375
p-value = 0.52726574965384
p-value = 0.54526686697779
cumulative error is 5.30131e-15
```



## Phix

{{trans|Go}}
{{trans|Kotlin}}

```Phix
function mean(sequence a)
    return sum(a) / length(a)
end function

function sv(sequence a)
    integer la = length(a)
    atom m := mean(a),
         tot := 0
    for i=1 to la do
        atom d = a[i] - m
        tot += d * d
    end for
    return tot / (la-1)
end function

function welch(sequence a, b)
    integer la = length(a),
            lb = length(b)
    return (mean(a) - mean(b)) / sqrt(sv(a)/la+sv(b)/lb)
end function

function dof(sequence a, b)
    integer la = length(a),
            lb = length(b)
    atom sva := sv(a),
         svb := sv(b),
         n := sva/la + svb/lb
    return n * n / (sva*sva/(la*la*(la-1)) +
                    svb*svb/(lb*lb*(lb-1)))
end function

function f(atom r, v)
    return power(r, v/2-1) / sqrt(1-r)
end function

function simpson0(integer n, atom high, v)
    atom tot := 0,
         dx0 := high / n,
         x0 := dx0, x1, xmid, dx
    tot += f(0,v) * dx0
    tot += f(dx0*.5,v) * dx0 * 4
    for i=1 to n-1 do
        x1 := (i+1) * high / n
        xmid := (x0 + x1) * .5
        dx := x1 - x0
        tot += f(x0,v) * dx * 2
        tot += f(xmid,v) * dx * 4
        x0 = x1
    end for
    return (tot + f(high,v)*dx0) / 6
end function

constant p = {
                0.99999999999980993,
              676.5203681218851,
            -1259.1392167224028,
              771.32342877765313,
             -176.61502916214059,
               12.507343278686905,
               -0.13857109526572012,
                9.9843695780195716e-6,
                1.5056327351493116e-7
             }

function gamma(atom d)
    atom dd = d,
          g = 7
    if dd<0.5 then
        return PI / (sin(PI*dd) * gamma(1-dd))
    end if
    dd -= 1
    atom a = p[1],
         t = dd + g + 0.5
    for i=2 to length(p) do a += p[i] / (dd + i - 1) end for
    return sqrt(2*PI) * power(t, dd + 0.5) * exp(-t) * a
end function

function lGamma(atom d)
    return log(gamma(d))
end function

function pValue(sequence ab)
    sequence {a, b} = ab
    atom v := dof(a, b),
         t := welch(a, b),
         g1 := lGamma(v / 2),
         g2 := lGamma(.5),
         g3 := lGamma(v/2 + .5)
    return simpson0(2000, v/(t*t+v), v) /   exp(g1+g2-g3)
end function

constant tests = {{{27.5, 21.0, 19.0, 23.6, 17.0, 17.9, 16.9, 20.1, 21.9, 22.6, 23.1, 19.6, 19.0, 21.7, 21.4},
                   {27.1, 22.0, 20.8, 23.4, 23.4, 23.5, 25.8, 22.0, 24.8, 20.2, 21.9, 22.1, 22.9, 20.5, 24.4}},
                  {{17.2, 20.9, 22.6, 18.1, 21.7, 21.4, 23.5, 24.2, 14.7, 21.8},
                   {21.5, 22.8, 21.0, 23.0, 21.6, 23.6, 22.5, 20.7, 23.4, 21.8, 20.7, 21.7, 21.5, 22.5, 23.6, 21.5, 22.5, 23.5, 21.5, 21.8}},
                  {{19.8, 20.4, 19.6, 17.8, 18.5, 18.9, 18.3, 18.9, 19.5, 22.0},
                   {28.2, 26.6, 20.1, 23.3, 25.2, 22.1, 17.7, 27.6, 20.6, 13.7, 23.2, 17.5, 20.6, 18.0, 23.9, 21.6, 24.3, 20.4, 24.0, 13.2}},
                  {{30.02, 29.99, 30.11, 29.97, 30.01, 29.99},
                   {29.89, 29.93, 29.72, 29.98, 30.02, 29.98}},
                  {{3.0, 4.0, 1.0, 2.1},
                   {490.2, 340.0, 433.9}}
                 }

for i=1 to length(tests) do
    ?pValue(tests[i])
end for
```

{{out}}

```txt

0.02137800146
0.1488416966
0.03597227103
0.09077332429
0.01075067374

```

{{trans|Python}}
The above was a bit off on the fifth test, so I also tried this.

using gamma() from [[Gamma_function#Phix]] (the one from above is probably also fine, but I didn't test that)

```Phix
function lgamma(atom d)
    return log(gamma(d))
end function

function betain(atom x, p, q)
    if p<=0 or q<=0 or x<0 or x>1 then ?9/0 end if
    if x == 0 or x == 1 then return x end if

    atom acu = 1e-15,
         lnbeta = lgamma(p) + lgamma(q) - lgamma(p + q),
         psq = p + q, cx = 1-x
    bool indx = (p<psq*x)
    if indx then
        {cx,x,p,q} = {x,1-x,q,p}
    end if

    atom term = 1,
         ai = 1,
         val = 1,
         ns = floor(q + cx*psq),
         rx = iff(ns=0?x:x/cx),
         temp = q - ai

    while true do
        term *= temp * rx / (p + ai)
        val += term
        temp = abs(term)

        if temp<=acu and temp<=acu*val then
            val *= exp(p*log(x) + (q-1)*log(cx) - lnbeta) / p
            return iff(indx?1-val:val)
        end if

        ai += 1
        ns -= 1
        if ns>=0 then
            temp = q - ai
            if ns == 0 then
                rx = x
            end if
        else
            temp = psq
            psq += 1
        end if
    end while
end function

function welch_ttest(sequence ab)
    sequence {a, b} = ab
    integer la = length(a),
            lb = length(b)
    atom ma = sum(a)/la,
         mb = sum(b)/lb,
         va = sum(sq_power(sq_sub(a,ma),2))/(la-1),
         vb = sum(sq_power(sq_sub(b,mb),2))/(lb-1),
         n = va/la + vb/lb,
         t = (ma-mb)/sqrt(n),
         df = (n*n) / (va*va/(la*la*(la-1)) + vb*vb/(lb*lb*(lb-1)))
    return betain(df/(t*t+df), df/2, 1/2)
end function

constant tests = {{{27.5, 21.0, 19.0, 23.6, 17.0, 17.9, 16.9, 20.1, 21.9, 22.6, 23.1, 19.6, 19.0, 21.7, 21.4},
                   {27.1, 22.0, 20.8, 23.4, 23.4, 23.5, 25.8, 22.0, 24.8, 20.2, 21.9, 22.1, 22.9, 20.5, 24.4}},
                  {{17.2, 20.9, 22.6, 18.1, 21.7, 21.4, 23.5, 24.2, 14.7, 21.8},
                   {21.5, 22.8, 21.0, 23.0, 21.6, 23.6, 22.5, 20.7, 23.4, 21.8, 20.7, 21.7, 21.5, 22.5, 23.6, 21.5, 22.5, 23.5, 21.5, 21.8}},
                  {{19.8, 20.4, 19.6, 17.8, 18.5, 18.9, 18.3, 18.9, 19.5, 22.0},
                   {28.2, 26.6, 20.1, 23.3, 25.2, 22.1, 17.7, 27.6, 20.6, 13.7, 23.2, 17.5, 20.6, 18.0, 23.9, 21.6, 24.3, 20.4, 24.0, 13.2}},
                  {{30.02, 29.99, 30.11, 29.97, 30.01, 29.99},
                   {29.89, 29.93, 29.72, 29.98, 30.02, 29.98}},
                  {{3.0, 4.0, 1.0, 2.1},
                   {490.2, 340.0, 433.9}},
                  {{0.010268,0.000167,0.000167},
                   {0.159258,0.136278,0.122389}},
                  {{1.0/15,10.0/62.0},
                   {1.0/10,2/50.0}},
                  {{9/23.0,21/45.0,0/38.0},
                   {0/44.0,42/94.0,0/22.0}}},
         correct = {0.021378001462867,
                    0.148841696605327,
                    0.0359722710297968,
                    0.090773324285671,
                    0.0107515611497845,
                    0.00339907162713746,
                    0.52726574965384,
                    0.545266866977794}

atom cerr = 0
for i=1 to length(tests) do
    atom r = welch_ttest(tests[i])
    ?r
    cerr += abs(r-correct[i])
end for
?{"cumulative error",cerr}
```

{{out}}

```txt

0.02137800146
0.1488416966
0.03597227103
0.09077332429
0.01075156115
0.003399071627
0.5272657497
0.545266867
{"cumulative error",1.989380882e-14}    -- (32 bit)
{"cumulative error",4.915115776e-15}    -- (64-bit)

```



## Python


=== Using NumPy & SciPy ===

```python
import numpy as np
import scipy as sp
import scipy.stats

def welch_ttest(x1, x2):
    n1 = x1.size
    n2 = x2.size
    m1 = np.mean(x1)
    m2 = np.mean(x2)
    v1 = np.var(x1, ddof=1)
    v2 = np.var(x2, ddof=1)
    t = (m1 - m2) / np.sqrt(v1 / n1 + v2 / n2)
    df = (v1 / n1 + v2 / n2)**2 / (v1**2 / (n1**2 * (n1 - 1)) + v2**2 / (n2**2 * (n2 - 1)))
    p = 2 * sp.stats.t.cdf(-abs(t), df)
    return t, df, p

welch_ttest(np.array([3.0, 4.0, 1.0, 2.1]), np.array([490.2, 340.0, 433.9]))
(-9.559497721932658, 2.0008523488562844, 0.01075156114978449)
```


###  Using betain from AS 63

First, the implementation of betain (translated from the Stata program in the discussion page). The original Fortran code is under copyrighted by the Royal Statistical Society. The C translation is under GPL, written by John Burkardt. The exact statement of the RSS license is unclear.


```python
import math

def betain(x, p, q):
    if p <= 0 or q <= 0 or x < 0 or x > 1:
        raise ValueError

    if x == 0 or x == 1:
        return x

    acu = 1e-15
    lnbeta = math.lgamma(p) + math.lgamma(q) - math.lgamma(p + q)

    psq = p + q
    if p < psq * x:
        xx = 1 - x
        cx = x
        pp = q
        qq = p
        indx = True
    else:
        xx = x
        cx = 1 - x
        pp = p
        qq = q
        indx = False

    term = ai = value = 1
    ns = math.floor(qq + cx * psq)
    rx = xx / cx
    temp = qq - ai
    if ns == 0:
        rx = xx

    while True:
        term *= temp * rx / (pp + ai)
        value += term
        temp = abs(term)

        if temp <= acu and temp <= acu * value:
            value *= math.exp(pp * math.log(xx) + (qq - 1) * math.log(cx) - lnbeta) / pp
            return 1 - value if indx else value

        ai += 1
        ns -= 1
        if ns >= 0:
            temp = qq - ai
            if ns == 0:
                rx = xx
        else:
            temp = psq
            psq += 1
```


The Python code is then straightforward:


```python
import math

def welch_ttest(a1, a2):
    n1 = len(a1)
    n2 = len(a2)
    if n1 <= 1 or n2 <= 1:
        raise ValueError

    mean1 = sum(a1) / n1
    mean2 = sum(a2) / n2

    var1 = sum((x - mean1)**2 for x in a1) / (n1 - 1)
    var2 = sum((x - mean2)**2 for x in a2) / (n2 - 1)

    t = (mean1 - mean2) / math.sqrt(var1 / n1 + var2 / n2)
    df = (var1 / n1 + var2 / n2)**2 / (var1**2 / (n1**2 * (n1 - 1)) + var2**2 / (n2**2 * (n2 - 1)))
    p = betain(df / (t**2 + df), df / 2, 1 / 2)

    return t, df, p
```


'''Example'''


```python
a1 = [3, 4, 1, 2.1]
a2 = [490.2, 340, 433.9]
print(welch_ttest(a1, a2))
```


'''Output'''

```txt
(-9.559497721932658, 2.0008523488562844, 0.01075156114978449)
```



## R


```R
#!/usr/bin/R

printf <- function(...) cat(sprintf(...))
#allows printing to greater number of digits #https://stackoverflow.com/questions/13023274/how-to-do-printf-in-r#13023329
d1 <- c(27.5,21.0,19.0,23.6,17.0,17.9,16.9,20.1,21.9,22.6,23.1,19.6,19.0,21.7,21.4)
d2 <- c(27.1,22.0,20.8,23.4,23.4,23.5,25.8,22.0,24.8,20.2,21.9,22.1,22.9,20.5,24.4)
d3 <- c(17.2,20.9,22.6,18.1,21.7,21.4,23.5,24.2,14.7,21.8)
d4 <- c(21.5,22.8,21.0,23.0,21.6,23.6,22.5,20.7,23.4,21.8,20.7,21.7,21.5,22.5,23.6,21.5,22.5,23.5,21.5,21.8)
d5 <- c(19.8,20.4,19.6,17.8,18.5,18.9,18.3,18.9,19.5,22.0)
d6 <- c(28.2,26.6,20.1,23.3,25.2,22.1,17.7,27.6,20.6,13.7,23.2,17.5,20.6,18.0,23.9,21.6,24.3,20.4,24.0,13.2)
d7 <- c(30.02,29.99,30.11,29.97,30.01,29.99)
d8 <- c(29.89,29.93,29.72,29.98,30.02,29.98)
x <- c(3.0,4.0,1.0,2.1)
y <- c(490.2,340.0,433.9)
v1 <- c(0.010268,0.000167,0.000167);
v2<- c(0.159258,0.136278,0.122389);
s1<- c(1.0/15,10.0/62.0);
s2<- c(1.0/10,2/50.0);
z1<- c(9/23.0,21/45.0,0/38.0);
z2<- c(0/44.0,42/94.0,0/22.0);

results <- t.test(d1,d2, alternative="two.sided", var.equal=FALSE)
printf("%.15g\n", results$p.value);
results <- t.test(d3,d4, alternative="two.sided", var.equal=FALSE)
printf("%.15g\n", results$p.value);
results <- t.test(d5,d6, alternative="two.sided", var.equal=FALSE)
printf("%.15g\n", results$p.value);
results <- t.test(d7,d8, alternative="two.sided", var.equal=FALSE)
printf("%.15g\n", results$p.value);
results <- t.test(x,y, alternative="two.sided", var.equal=FALSE)
printf("%.15g\n", results$p.value);
results <- t.test(v1,v2, alternative="two.sided", var.equal=FALSE)
printf("%.15g\n", results$p.value);
results <- t.test(s1,s2, alternative="two.sided", var.equal=FALSE)
printf("%.15g\n", results$p.value);
results <- t.test(z1,z2, alternative="two.sided", var.equal=FALSE)
printf("%.15g\n", results$p.value);

```


The output here is used to compare against C's output above.
{{out}}

```txt
0.021378001462867
0.148841696605327
0.0359722710297968
0.090773324285671
0.0107515611497845
0.00339907162713746
0.52726574965384
0.545266866977794

```



## Racket

{{trans|C}}


```racket
#lang racket
(require math/statistics math/special-functions)

(define (p-value S1 S2 #:n (n 11000))
  (define σ²1 (variance S1 #:bias #t))
  (define σ²2 (variance S2 #:bias #t))
  (define N1 (sequence-length S1))
  (define N2 (sequence-length S2))
  (define σ²/sz1 (/ σ²1 N1))
  (define σ²/sz2 (/ σ²2 N2))

  (define degrees-of-freedom
    (/ (sqr (+ σ²/sz1 σ²/sz2))
       (+ (/ (sqr σ²1) (* (sqr N1) (sub1 N1)))
          (/ (sqr σ²2) (* (sqr N2) (sub1 N2))))))

  (define a (/ degrees-of-freedom 2))
  (define a-1 (sub1 a))
  (define x (let ((welch-t-statistic (/ (- (mean S1) (mean S2)) (sqrt (+ σ²/sz1 σ²/sz2)))))
              (/ degrees-of-freedom (+ (sqr welch-t-statistic) degrees-of-freedom))))
  (define h (/ x n))

  (/ (* (/ h 6)
        (+ (* (expt x a-1)
              (expt (- 1 x) -1/2))
           (* 4 (for/sum ((i (in-range 0 n)))
                  (* (expt (+ (* h i) (/ h 2)) a-1)
                     (expt (- 1 (+ (* h i) (/ h 2))) -1/2))))
           (* 2  (for/sum ((i (in-range 0 n)))
                   (* (expt (* h i) a-1) (expt (- 1 (* h i)) -1/2))))))
     (* (gamma a) 1.77245385090551610 (/ (gamma (+ a 1/2))))))

(module+ test
  (list
   (p-value (list 27.5 21.0 19.0 23.6 17.0 17.9 16.9 20.1 21.9 22.6 23.1 19.6 19.0 21.7 21.4)
            (list 27.1 22.0 20.8 23.4 23.4 23.5 25.8 22.0 24.8 20.2 21.9 22.1 22.9 20.5 24.4))

   (p-value (list 17.2 20.9 22.6 18.1 21.7 21.4 23.5 24.2 14.7 21.8)
            (list 21.5 22.8 21.0 23.0 21.6 23.6 22.5 20.7 23.4 21.8
                  20.7 21.7 21.5 22.5 23.6 21.5 22.5 23.5 21.5 21.8))

   (p-value (list 19.8 20.4 19.6 17.8 18.5 18.9 18.3 18.9 19.5 22.0)
            (list 28.2 26.6 20.1 23.3 25.2 22.1 17.7 27.6 20.6 13.7
                  23.2 17.5 20.6 18.0 23.9 21.6 24.3 20.4 24.0 13.2))

   (p-value (list 30.02 29.99 30.11 29.97 30.01 29.99)
            (list 29.89 29.93 29.72 29.98 30.02 29.98))

   (p-value (list 3.0 4.0 1.0 2.1)
            (list 490.2 340.0 433.9))))
```


{{out}}

```txt
(0.021378001462867013 0.14884169660532798 0.035972271029796624 0.09077332428567102 0.01075139991904718)
```



## Ruby

{{trans|Perl}}

```Ruby
def calculate_p_value(array1, array2)
  return 1.0 if array1.size <= 1
  return 1.0 if array2.size <= 1
  mean1 = array1.sum / array1.size
  mean2 = array2.sum / array2.size
  return 1.0 if mean1 == mean2
  variance1 = 0.0
  variance2 = 0.0
  array1.each do |x|
    variance1 += (mean1 - x)**2
  end
  array2.each do |x|
    variance2 += (mean2 - x)**2
  end
  return 1.0 if variance1 == 0.0 && variance2 == 0.0
  variance1 /= (array1.size - 1)
  variance2 /= (array2.size - 1)
  welch_t_statistic = (mean1 - mean2) / Math.sqrt(variance1 / array1.size + variance2 / array2.size)
  degrees_of_freedom = ((variance1 / array1.size + variance2 / array2.size)**2)	/	(
  (variance1 * variance1) / (array1.size * array1.size * (array1.size - 1)) +
  (variance2 * variance2) / (array2.size * array2.size * (array2.size - 1)))
  a = degrees_of_freedom / 2
  value = degrees_of_freedom / (welch_t_statistic**2 + degrees_of_freedom)
  beta = Math.lgamma(a)[0] + 0.57236494292470009 - Math.lgamma(a + 0.5)[0]
  acu = 10**-15
  return value if a <= 0
  return value if value < 0.0 || value > 1.0
  return value if (value == 0) || (value == 1.0)
  psq = a + 0.5
  cx = 1.0 - value
  if a < psq * value
    xx = cx
    cx = value
    pp = 0.5
    qq = a
    indx = 1
  else
    xx = value
    pp = a
    qq = 0.5
    indx = 0
  end
  term = 1.0
  ai = 1.0
  value = 1.0
  ns = (qq + cx * psq).to_i
  # Soper reduction formula
  rx = xx / cx
  temp = qq - ai
  loop do
    term = term * temp * rx / (pp + ai)
    value += term
    temp = term.abs
    if temp <= acu && temp <= acu * value
      value = value * Math.exp(pp * Math.log(xx) + (qq - 1.0) * Math.log(cx) - beta) / pp
      value = 1.0 - value
      value = 1.0 - value if indx == 0
      break
    end
    ai += 1.0
    ns -= 1
    if ns >= 0
      temp = qq - ai
      rx = xx if ns == 0
    else
      temp = psq
      psq += 1.0
    end
  end
  value
end

d1 = [27.5, 21.0, 19.0, 23.6, 17.0, 17.9, 16.9, 20.1, 21.9, 22.6, 23.1, 19.6, 19.0, 21.7, 21.4]
d2 = [27.1, 22.0, 20.8, 23.4, 23.4, 23.5, 25.8, 22.0, 24.8, 20.2, 21.9, 22.1, 22.9, 20.5, 24.4]
d3 = [17.2, 20.9, 22.6, 18.1, 21.7, 21.4, 23.5, 24.2, 14.7, 21.8]
d4 = [21.5, 22.8, 21.0, 23.0, 21.6, 23.6, 22.5, 20.7, 23.4, 21.8, 20.7, 21.7, 21.5, 22.5, 23.6, 21.5, 22.5, 23.5, 21.5, 21.8]
d5 = [19.8, 20.4, 19.6, 17.8, 18.5, 18.9, 18.3, 18.9, 19.5, 22.0]
d6 = [28.2, 26.6, 20.1, 23.3, 25.2, 22.1, 17.7, 27.6, 20.6, 13.7, 23.2, 17.5, 20.6, 18.0, 23.9, 21.6, 24.3, 20.4, 24.0, 13.2]
d7 = [30.02, 29.99, 30.11, 29.97, 30.01, 29.99]
d8 = [29.89, 29.93, 29.72, 29.98, 30.02, 29.98]
x = [3.0, 4.0, 1.0, 2.1]
y = [490.2, 340.0, 433.9]
s1 = [1.0 / 15, 10.0 / 62.0]
s2 = [1.0 / 10, 2 / 50.0]
v1 = [0.010268, 0.000167, 0.000167]
v2 = [0.159258, 0.136278, 0.122389]
z1 = [9 / 23.0, 21 / 45.0, 0 / 38.0]
z2 = [0 / 44.0, 42 / 94.0, 0 / 22.0]

CORRECT_ANSWERS = [0.021378001462867, 0.148841696605327, 0.0359722710297968,
                   0.090773324285671, 0.0107515611497845, 0.00339907162713746, 0.52726574965384, 0.545266866977794].freeze

pvalue = calculate_p_value(d1, d2)
error = (pvalue - CORRECT_ANSWERS[0]).abs
printf("Test sets 1 p-value = %.14g\n", pvalue)

pvalue = calculate_p_value(d3, d4)
error += (pvalue - CORRECT_ANSWERS[1]).abs
printf("Test sets 2 p-value = %.14g\n", pvalue)

pvalue = calculate_p_value(d5, d6)
error += (pvalue - CORRECT_ANSWERS[2]).abs
printf("Test sets 3 p-value = %.14g\n", pvalue)

pvalue = calculate_p_value(d7, d8)
error += (pvalue - CORRECT_ANSWERS[3]).abs
printf("Test sets 4 p-value = %.14g\n", pvalue)

pvalue = calculate_p_value(x, y)
error += (pvalue - CORRECT_ANSWERS[4]).abs
printf("Test sets 5 p-value = %.14g\n", pvalue)

pvalue = calculate_p_value(v1, v2)
error += (pvalue - CORRECT_ANSWERS[5]).abs
printf("Test sets 6 p-value = %.14g\n", pvalue)

pvalue = calculate_p_value(s1, s2)
error += (pvalue - CORRECT_ANSWERS[6]).abs
printf("Test sets 7 p-value = %.14g\n", pvalue)

pvalue = calculate_p_value(z1, z2)
error += (pvalue - CORRECT_ANSWERS[7]).abs
printf("Test sets z p-value = %.14g\n", pvalue)

printf("the cumulative error is %g\n", error)

```

{{out}}

```txt

Test sets 1 p-value = 0.021378001462867
Test sets 2 p-value = 0.14884169660533
Test sets 3 p-value = 0.035972271029797
Test sets 4 p-value = 0.090773324285671
Test sets 5 p-value = 0.010751561149784
Test sets 6 p-value = 0.0033990716271375
Test sets 7 p-value = 0.52726574965384
Test sets z p-value = 0.54526686697779
the cumulative error is 1.34961e-15

```



## SAS

{{trans|Stata}}
<lang>data tbl;
input value group @@;
cards;
3 1 4 1 1 1 2.1 1 490.2 2 340 2 433.9 2
;
run;

proc ttest data=tbl;
class group;
var value;
run;
```


'''Output'''

<table align="center" cellspacing="1" cellpadding="7" rules="all" frame="box" border="1" summary="Procedure Ttest: Statistics">
<tr>
<th scope="col">group</th>
<th scope="col">Method</th>
<th scope="col">N</th>
<th scope="col">Mean</th>
<th scope="col">Std Dev</th>
<th scope="col">Std Err</th>
<th scope="col">Minimum</th>
<th scope="col">Maximum</th>
</tr>
<tr>
<th scope="row">1</th>
<th scope="row"> </th>
<td>4</td>
<td>2.5250</td>
<td>1.2790</td>
<td>0.6395</td>
<td>1.0000</td>
<td>4.0000</td>
</tr>
<tr>
<th scope="row">2</th>
<th scope="row"> </th>
<td>3</td>
<td>421.4</td>
<td>75.8803</td>
<td>43.8095</td>
<td>340.0</td>
<td>490.2</td>
</tr>
<tr>
<th scope="row">Diff (1-2)</th>
<th scope="row">Pooled</th>
<td> </td>
<td nowrap>-418.8</td>
<td>48.0012</td>
<td>36.6615</td>
<td> </td>
<td> </td>
</tr>
<tr>
<th scope="row">Diff (1-2)</th>
<th scope="row">Satterthwaite</th>
<td> </td>
<td nowrap>-418.8</td>
<td> </td>
<td>43.8142</td>
<td> </td>
<td> </td>
</tr>
</table>

<br/>

<table align="center" cellspacing="1" cellpadding="7" rules="all" frame="box" border="1" summary="Procedure Ttest: Confidence Limits">
<tr>
<th scope="col">group</th>
<th scope="col">Method</th>
<th scope="col">Mean</th>
<th colspan="2" scope="colgroup">95% CL Mean</th>
<th scope="col">Std Dev</th>
<th colspan="2" scope="colgroup">95% CL Std Dev</th>
</tr>
<tr>
<th scope="row">1</th>
<th scope="row"> </th>
<td>2.5250</td>
<td>0.4898</td>
<td>4.5602</td>
<td>1.2790</td>
<td>0.7245</td>
<td>4.7688</td>
</tr>
<tr>
<th scope="row">2</th>
<th scope="row"> </th>
<td>421.4</td>
<td>232.9</td>
<td>609.9</td>
<td>75.8803</td>
<td>39.5077</td>
<td>476.9</td>
</tr>
<tr>
<th scope="row">Diff (1-2)</th>
<th scope="row">Pooled</th>
<td nowrap>-418.8</td>
<td nowrap>-513.1</td>
<td nowrap>-324.6</td>
<td>48.0012</td>
<td>29.9627</td>
<td>117.7</td>
</tr>
<tr>
<th scope="row">Diff (1-2)</th>
<th scope="row">Satterthwaite</th>
<td nowrap>-418.8</td>
<td nowrap>-607.3</td>
<td nowrap>-230.4</td>
<td> </td>
<td> </td>
<td> </td>
</tr>
</table>

<br/>

<table align="center" cellspacing="1" cellpadding="7" rules="all" frame="box" border="1" summary="Procedure Ttest: T-Tests">
<tr>
<th scope="col">Method</th>
<th scope="col">Variances</th>
<th scope="col">DF</th>
<th scope="col">t Value</th>
<th scope="col">Pr &gt; |t|</th>
</tr>
<tr>
<th scope="row">Pooled</th>
<td>Equal</td>
<td>5</td>
<td nowrap>-11.42</td>
<td>&lt;.0001</td>
</tr>
<tr>
<th scope="row">Satterthwaite</th>
<td>Unequal</td>
<td>2.0009</td>
<td nowrap>-9.56</td>
<td>0.0108</td>
</tr>
</table>

<br/>

<table align="center" cellspacing="1" cellpadding="7" rules="all" frame="box" border="1" summary="Procedure Ttest: Equality of Variances">
<tr>
<th colspan="5" scope="colgroup">Equality of Variances</th>
</tr>
<tr>
<th scope="col">Method</th>
<th scope="col">Num DF</th>
<th scope="col">Den DF</th>
<th scope="col">F Value</th>
<th scope="col">Pr &gt; F</th>
</tr>
<tr>
<th scope="row">Folded F</th>
<td>2</td>
<td>3</td>
<td>3519.81</td>
<td>&lt;.0001</td>
</tr>
</table>


Implementation in IML:


```sas
proc iml;
use tbl;
read all var {value} into x where(group=1);
read all var {value} into y where(group=2);
close tbl;
n1 = nrow(x);
n2 = nrow(y);
v1 = var(x);
v2 = var(y);
t = (mean(x)-mean(y))/(sqrt(v1/n1+v2/n2));
df = (v1/n1+v2/n2)**2/(v1**2/(n1**2*(n1-1))+v2**2/(n2**2*(n2-1)));
p = 2*probt(-abs(t), df);
print t df p;
quit;
```


'''Output'''


```txt
-9.559498 2.0008523 0.0107516
```



## Scala


```Scala
import org.apache.commons.math3.distribution.TDistribution

object WelchTTest extends App {

  val res = welchTtest(Array(3.0, 4.0, 1.0, 2.1), Array(490.2, 340.0, 433.9))

  def welchTtest(x: Array[Double], y: Array[Double]) = {

    def square[T](x: T)(implicit num: Numeric[T]): T = {
      import num._
      x * x
    }

    def count[A](a: Seq[A])(implicit num: Fractional[A]): A =
      a.foldLeft(num.zero) { case (cnt, _) => num.plus(cnt, num.one) }

    def mean[A](a: Seq[A])(implicit num: Fractional[A]): A = num.div(a.sum, count(a))

    def variance[A](a: Seq[A])(implicit num: Fractional[A]) =
      num.div(a.map(xs => square(num.minus(xs, mean(a)))).sum, num.minus(count(a), num.one))

    val (nx, ny) = (x.length, y.length)
    val (vx, vy) = (variance(x), variance(y))
    val qt = vx / nx + vy / ny
    val t = (mean(x) - mean(y)) / math.sqrt(qt)
    val df = square(qt) / (square(vx) / (square(nx) * (nx - 1)) + square(vy) / (square(ny) * (ny - 1)))
    val p = 2.0 * new TDistribution(df).cumulativeProbability(-math.abs(t))
    (t, df, p)
  }

  println(s"t  = ${res._1}\ndf = ${res._2}\np  = ${res._3}")
  println(s"\nSuccessfully completed without errors. [total ${scala.compat.Platform.currentTime - executionStart} ms]")

}
```


## Scilab

{{trans|Stata}}

Scilab will print a warning because the number of degrees of freedom is not an integer. However, the underlying implementation makes use of the [http://www.netlib.org/random/ dcdflib] Fortran library, which happily accepts a noninteger df.

<lang>x = [3.0,4.0,1.0,2.1];
y = [490.2,340.0,433.9];
n1 = length(x);
n2 = length(y);
v1 = variance(x);
v2 = variance(y);
t = (mean(x)-mean(y))/(sqrt(v1/n1+v2/n2));
df = (v1/n1+v2/n2)^2/(v1^2/(n1^2*(n1-1))+v2^2/(n2^2*(n2-1)));
[p, q] = cdft("PQ", -abs(t), df);
[t df 2*p]
```


'''Output'''


```txt
 ans  =

  - 9.5594977    2.0008523    0.0107516
```



## Sidef

{{trans|Perl 6}}

```ruby
func p_value (A, B) {
    [A.len, B.len].all { _ > 1 } || return 1

    var x̄_a = Math.avg(A...)
    var x̄_b = Math.avg(B...)

    var a_var = (A.map {|n| (x̄_a - n)**2 }.sum / A.end)
    var b_var = (B.map {|n| (x̄_b - n)**2 }.sum / B.end)

    (a_var && b_var) || return 1

    var Welsh_𝒕_statistic = ((x̄_a - x̄_b) / √(a_var/A.len + b_var/B.len))

    var DoF = ((a_var/A.len + b_var/B.len)**2 /
              ((a_var**2 / (A.len**3 - A.len**2)) + (b_var**2 / (B.len**3 - B.len**2))))

    var sa = (DoF/2 - 1)
    var x  = (DoF/(Welsh_𝒕_statistic**2 + DoF))
    var N  = 65355
    var h  = x/N

    var (sum1=0, sum2=0)

    ^N -> lazy.map { _ * h }.each { |i|
        sum1 += (((i + h/2) ** sa) / √(1 - (i + h/2)))
        sum2 += (( i        ** sa) / √(1 - (i      )))
    }

    (h/6 * (x**sa / √(1-x) + 4*sum1 + 2*sum2)) /
        (gamma(sa + 1) * √(Num.pi) / gamma(sa + 1.5))
}

# Testing
var tests = [
  %n<27.5 21.0 19.0 23.6 17.0 17.9 16.9 20.1 21.9 22.6 23.1 19.6 19.0 21.7 21.4>,
  %n<27.1 22.0 20.8 23.4 23.4 23.5 25.8 22.0 24.8 20.2 21.9 22.1 22.9 20.5 24.4>,

  %n<17.2 20.9 22.6 18.1 21.7 21.4 23.5 24.2 14.7 21.8>,
  %n<21.5 22.8 21.0 23.0 21.6 23.6 22.5 20.7 23.4 21.8 20.7 21.7 21.5 22.5 23.6 21.5 22.5 23.5 21.5 21.8>,

  %n<19.8 20.4 19.6 17.8 18.5 18.9 18.3 18.9 19.5 22.0>,
  %n<28.2 26.6 20.1 23.3 25.2 22.1 17.7 27.6 20.6 13.7 23.2 17.5 20.6 18.0 23.9 21.6 24.3 20.4 24.0 13.2>,

  %n<30.02 29.99 30.11 29.97 30.01 29.99>,
  %n<29.89 29.93 29.72 29.98 30.02 29.98>,

  %n<3.0 4.0 1.0 2.1>,
  %n<490.2 340.0 433.9>
]

tests.each_slice(2, {|left, right|
    say p_value(left, right)
})
```

{{out}}

```txt

0.0213780014628670325061113281387220205111519317756
0.148841696605327985083613019511085971435711697961
0.0359722710297967180871367618538977446933248150651
0.0907733242856668878840956275523536083406692525656
0.0107515340333929755465323718028856669932912031012

```



## Stata

Here is a straightforward solution using the '''ttest''' command. If one does not want the output but only the p-value, prepend the command with '''qui''' and use the result r(p) as shown below. The t statistic is r(t). Notice the data are stored in a single variable, using a group variable to distinguish the two series.

Notice that here we use the option '''unequal''' of the '''ttest''' command, and not '''welch''', so that Stata uses the Welch-Satterthwaite approximation.


```stata
mat a=(3,4,1,2.1,490.2,340,433.9\1,1,1,1,2,2,2)'
clear
svmat double a
rename (a1 a2) (x group)
ttest x, by(group) unequal

    Two-sample t test with unequal variances
    ------------------------------------------------------------------------------
       Group |     Obs        Mean    Std. Err.   Std. Dev.   [95% Conf. Interval]
    ---------+--------------------------------------------------------------------
           1 |       4       2.525    .6394985    1.278997    .4898304     4.56017
           2 |       3    421.3667    43.80952    75.88032    232.8695    609.8638
    ---------+--------------------------------------------------------------------
    combined |       7    182.0286    86.22435    228.1282   -28.95482     393.012
    ---------+--------------------------------------------------------------------
        diff |           -418.8417    43.81419                -607.282   -230.4014
    ------------------------------------------------------------------------------
        diff = mean(1) - mean(2)                                      t =  -9.5595
    Ho: diff = 0                     Satterthwaite's degrees of freedom =  2.00085

        Ha: diff < 0                 Ha: diff != 0                 Ha: diff > 0
     Pr(T < t) = 0.0054         Pr(|T| > |t|) = 0.0108          Pr(T > t) = 0.9946

di r(t)
    -9.5594977

di r(p)
    .01075156
```


The computation can easily be implemented in Mata. Here is how to compute the t statistic (t), the approximate degrees of freedom (df) and the p-value (p).


```stata
st_view(a=., ., .)
x = select(a[., 1], a[., 2] :== 1)
y = select(a[., 1], a[., 2] :== 2)
n1 = length(x)
n2 = length(y)
v1 = variance(x)
v2 = variance(y)
t = (mean(x)-mean(y))/sqrt(v1/n1+v2/n2)
df = (v1/n1+v2/n2)^2/(v1^2/(n1^2*(n1-1))+v2^2/(n2^2*(n2-1)))
p = 2*t(df, -abs(t))
t,df,p
                  1              2              3
    +----------------------------------------------+
  1 |  -9.559497722    2.000852349    .0107515611  |
    +----------------------------------------------+
```



## Tcl

{{trans|Racket}}
{{works with|Tcl|8.6}}
{{tcllib|math::statistics}}
{{tcllib|math::special}}

This is not particularly idiomatic Tcl, but perhaps illustrates some of the language's relationship with the Lisp family.


```Tcl
#!/usr/bin/tclsh

package require math::statistics
package require math::special
namespace path {::math::statistics ::math::special ::tcl::mathfunc ::tcl::mathop}

proc incf {_var {inc 1.0}} {
    upvar 1 $_var var
    if {![info exists var]} {
        set var 0.0
    }
    set var [expr {$inc + $var}]
}

proc sumfor {_var A B body} {
    upvar 1 $_var var
    set var $A
    set res 0
    while {$var < $B} {
        incf res [uplevel 1 $body]
        incr var
    }
    return $res
}

proc sqr {x} {expr {$x*$x}}

proc pValue {S1 S2 {n 11000}} {
    set σ²1 [var $S1]
    set σ²2 [var $S2]
    set N1  [llength $S1]
    set N2  [llength $S2]
    set σ²/sz1 [/ ${σ²1} $N1]
    set σ²/sz2 [/ ${σ²2} $N2]

    set d1 [/ [sqr ${σ²1}] [* [sqr $N1] [- $N1 1]]]
    set d2 [/ [sqr ${σ²2}] [* [sqr $N2] [- $N2 1]]]
    set DoF [/ [sqr [+ ${σ²/sz1} ${σ²/sz2}]] [+ $d1 $d2]]

    set a [/ $DoF 2.0]

    set welchTstat [/ [- [mean $S1] [mean $S2]] [sqrt [+ ${σ²/sz1} ${σ²/sz2}]]]
    set x [/ $DoF [+ [sqr $welchTstat] $DoF]]
    set h [/ $x $n]

    / [* [/ $h 6] \
         [+ [* [** $x [- $a 1]] \
               [** [- 1 $x] -0.5]] \
            [* 4 [sumfor i 0 $n {
                    * [** [+ [* $h $i] [/ $h 2]] [- $a 1]] \
                      [** [- 1 [* $h $i] [/ $h 2]] -0.5]}]] \
            [* 2 [sumfor i 0 $n {
                    * [** [* $h $i] [- $a 1]] [** [- 1 [* $h $i]] -0.5]}]]]] \
      [* [Gamma $a] 1.77245385090551610 [/ 1.0 [Gamma [+ $a 0.5]]]]
}


foreach {left right} {
    { 27.5 21.0 19.0 23.6 17.0 17.9 16.9 20.1 21.9 22.6 23.1 19.6 19.0 21.7 21.4 }
    { 27.1 22.0 20.8 23.4 23.4 23.5 25.8 22.0 24.8 20.2 21.9 22.1 22.9 20.5 24.4 }

    { 17.2 20.9 22.6 18.1 21.7 21.4 23.5 24.2 14.7 21.8 }
    { 21.5 22.8 21.0 23.0 21.6 23.6 22.5 20.7 23.4 21.8 20.7 21.7 21.5 22.5 23.6 21.5 22.5 23.5 21.5 21.8 }

    { 19.8 20.4 19.6 17.8 18.5 18.9 18.3 18.9 19.5 22.0 }
    { 28.2 26.6 20.1 23.3 25.2 22.1 17.7 27.6 20.6 13.7 23.2 17.5 20.6 18.0 23.9 21.6 24.3 20.4 24.0 13.2 }

    { 30.02 29.99 30.11 29.97 30.01 29.99 }
    { 29.89 29.93 29.72 29.98 30.02 29.98 }

    { 3.0 4.0 1.0 2.1 }
    { 490.2 340.0 433.9 }
} {
    puts [pValue $left $right]
}

```


{{out}}

```txt
0.021378001462853034
0.148841696604164
0.035972271029770915
0.09077332428458083
0.010751399918798182

```



## zkl

{{trans|C}}

```zkl
fcn calculate_Pvalue(array1,array2){
   if (array1.len()<=1 or array2.len()<=1) return(1.0);

   mean1,mean2 := array1.sum(0.0),array2.sum(0.0);
   if(mean1==mean2) return(1.0);
   mean1/=array1.len();
   mean2/=array2.len();

   variance1:=array1.reduce('wrap(sum,x){ sum + (x-mean1).pow(2) },0.0);
   variance2:=array2.reduce('wrap(sum,x){ sum + (x-mean2).pow(2) },0.0);

   variance1/=(array1.len() - 1);
   variance2/=(array2.len() - 1);

   WELCH_T_STATISTIC:=(mean1-mean2)/
                     (variance1/array1.len() + variance2/array2.len()).sqrt();
   DEGREES_OF_FREEDOM:=
      ( variance1/array1.len() + variance2/array2.len() ).pow(2) // numerator
      / (
          (variance1*variance1)/(array1.len().pow(2)*(array1.len() - 1)) +
	  (variance2*variance2)/(array2.len().pow(2)*(array2.len() - 1))
        );
   a:=DEGREES_OF_FREEDOM/2;
   x:=DEGREES_OF_FREEDOM/( WELCH_T_STATISTIC.pow(2) + DEGREES_OF_FREEDOM );
   N,h := 65535, x/N;

   sum1,sum2 := 0.0, 0.0;
   foreach i in (N){
      sum1+=((h*i + h/2.0).pow(a - 1))/(1.0 - (h*i + h/2.0)).sqrt();
      sum2+=((h*i).pow(a - 1))/(1.0 - h*i).sqrt();
   }
   return_value:=((h/6.0)*( x.pow(a - 1)/(1.0 - x).sqrt() +
      4.0*sum1 + 2.0*sum2) ) /
      ((0.0).e.pow(lngammal(a) + 0.57236494292470009 - lngammal(a + 0.5)));

   if(return_value > 1.0) return(1.0);	// or return_value is infinite, throws
   return_value;
}
fcn lngammal(xx){
   var [const] cof=List(	// static
      76.18009172947146,    -86.50532032941677,
      24.01409824083091,    -1.231739572450155,
      0.1208650973866179e-2,-0.5395239384953e-5
   );

   y:=x:=xx;
   tmp:=x + 5.5 - (x + 0.5) * (x + 5.5).log();
   ser:=1.000000000190015;
   foreach x in (cof){ ser+=(x/(y+=1)); }
   return((2.5066282746310005 * ser / x).log() - tmp);
}
```


```zkl
testSets:=T(
T(T(27.5,21.0,19.0,23.6,17.0,17.9,16.9,20.1,21.9,22.6,23.1,19.6,19.0,21.7,21.4),
  T(27.1,22.0,20.8,23.4,23.4,23.5,25.8,22.0,24.8,20.2,21.9,22.1,22.9,20.5,24.4)),
T(T(17.2,20.9,22.6,18.1,21.7,21.4,23.5,24.2,14.7,21.8),
  T(21.5,22.8,21.0,23.0,21.6,23.6,22.5,20.7,23.4,21.8,20.7,21.7,21.5,22.5,23.6,21.5,22.5,23.5,21.5,21.8)),
T(T(19.8,20.4,19.6,17.8,18.5,18.9,18.3,18.9,19.5,22.0),
  T(28.2,26.6,20.1,23.3,25.2,22.1,17.7,27.6,20.6,13.7,23.2,17.5,20.6,18.0,23.9,21.6,24.3,20.4,24.0,13.2)),
T(T(30.02,29.99,30.11,29.97,30.01,29.99),
  T(29.89,29.93,29.72,29.98,30.02,29.98)),
T(T(3.0,4.0,1.0,2.1),T(490.2,340.0,433.9)) );

foreach x,y in (testSets)
   { println("Test set 1 p-value = %f".fmt(calculate_Pvalue(x,y))); }
```

{{out}}

```txt

Test set 1 p-value = 0.021378
Test set 1 p-value = 0.148842
Test set 1 p-value = 0.035972
Test set 1 p-value = 0.090773
Test set 1 p-value = 0.010752

```

