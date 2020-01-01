+++
title = "P-value correction"
description = ""
date = 2019-09-14T17:26:55Z
aliases = []
[extra]
id = 21652
[taxonomies]
categories = []
tags = []
+++

{{task|Probability and statistics}}

Given a list of [[wp:p-value|p-values]], adjust the p-values for multiple comparisons. This is done in order to control the false positive, or Type 1 error rate.

This is also known as the "[[wp:False discovery rate|false discovery rate]]" (FDR). After adjustment, the p-values will be higher but still inside [0,1].

The adjusted p-values are sometimes called "q-values".


;Task:
Given one list of [[Welch's_t-test|p-values]], return the p-values correcting for multiple comparisons

    p = {4.533744e-01, 7.296024e-01, 9.936026e-02, 9.079658e-02, 1.801962e-01,
         8.752257e-01, 2.922222e-01, 9.115421e-01, 4.355806e-01, 5.324867e-01,
         4.926798e-01, 5.802978e-01, 3.485442e-01, 7.883130e-01, 2.729308e-01,
         8.502518e-01, 4.268138e-01, 6.442008e-01, 3.030266e-01, 5.001555e-02,
         3.194810e-01, 7.892933e-01, 9.991834e-01, 1.745691e-01, 9.037516e-01,
         1.198578e-01, 3.966083e-01, 1.403837e-02, 7.328671e-01, 6.793476e-02,
         4.040730e-03, 3.033349e-04, 1.125147e-02, 2.375072e-02, 5.818542e-04,
         3.075482e-04, 8.251272e-03, 1.356534e-03, 1.360696e-02, 3.764588e-04,
         1.801145e-05, 2.504456e-07, 3.310253e-02, 9.427839e-03, 8.791153e-04,
         2.177831e-04, 9.693054e-04, 6.610250e-05, 2.900813e-02, 5.735490e-03}


There are several methods to do this, see:
* Yoav Benjamini, Yosef Hochberg "[http://www.math.tau.ac.il/~ybenja/MyPapers/benjamini_hochberg1995.pdf Controlling the False Discovery Rate: A Practical and Powerful Approach to Multiple Testing]", ''Journal of the Royal Statistical Society. Series B'', Vol. 57, No. 1 (1995), pp. 289-300, JSTOR:[http://www.jstor.org/stable/2346101 2346101]
* Yoav Benjamini, Daniel Yekutieli, "[http://www.math.tau.ac.il/~ybenja/MyPapers/benjamini_yekutieli_ANNSTAT2001.pdf The control of the false discovery rate in multiple testing under dependency]", ''Ann. Statist.'', Vol. 29, No. 4 (2001), pp. 1165-1188, DOI:[https://doi.org/10.1214/aos/1013699998 10.1214/aos/1013699998] JSTOR:[http://www.jstor.org/stable/2674075 2674075]
* Sture Holm, "A Simple Sequentially Rejective Multiple Test Procedure", ''Scandinavian Journal of Statistics'', Vol. 6, No. 2 (1979), pp. 65-70, JSTOR:[https://www.jstor.org/stable/4615733 4615733]
* Yosef Hochberg, "A sharper Bonferroni procedure for multiple tests of significance", ''Biometrika'', Vol. 75, No. 4 (1988), pp 800–802, DOI:[https://doi.org/10.1093/biomet/75.4.800 10.1093/biomet/75.4.800] JSTOR:[https://www.jstor.org/stable/2336325 2336325]
* Gerhard Hommel, "A stagewise rejective multiple test procedure based on a modified Bonferroni test", ''Biometrika'', Vol. 75, No. 2 (1988), pp 383–386, DOI:[https://doi.org/10.1093/biomet/75.2.383 10.1093/biomet/75.2.383] JSTOR:[https://www.jstor.org/stable/2336190 2336190]


Each method has its own advantages and disadvantages.





## C


### Version 1

{{works with|C99}}
{{trans|R}}
''This work is based on R source code covered by the '''GPL''' license. It is thus a modified version, also covered by the GPL. See the [https://www.gnu.org/licenses/gpl-faq.html#GPLRequireSourcePostedPublic FAQ about GNU licenses]''.

This work is a translation of the R source code.  In order to confirm that the new function is working correctly, each value is compared to R's output and a cumulative absolute error is returned.

The C function <code>p_adjust</code> is designed to work as similarly to the R function <code>p.adjust</code> as possible, and is able to do any one of the methods.

This program, for example, fdr.c, can be compiled by

<code>gcc -o fdr fdr.c -Wall -pedantic -std=c11 -lm -O4</code>

or

<code>clang -o fdr fdr.c -Wall -pedantic -std=c11 -lm -O4</code>.

Link with <code>-lm</code>

```c
#include <stdio.h>//printf
#include <stdlib.h>//qsort
#include <math.h>//fabs
#include <stdbool.h>//bool data type
#include <strings.h>//strcasecmp

unsigned int *restrict seq_len(const size_t START, const size_t END) {
//named after R function of same name, but simpler function
	size_t start = START;
	size_t end = END;
	if (START == END) {
		unsigned int *restrict sequence = malloc( (end+1) * sizeof(unsigned int));
		if (sequence == NULL) {
			printf("malloc failed at %s line %u\n", __FILE__, __LINE__);
			perror("");
			exit(EXIT_FAILURE);
		}
		for (size_t i = 0; i < end; i++) {
			sequence[i] = i+1;
		}
		return sequence;
	}
	if (START > END) {
		end = START;
		start = END;
	}
	const size_t LENGTH = end - start ;
	unsigned int *restrict sequence = malloc( (1+LENGTH) * sizeof(unsigned int));
	if (sequence == NULL) {
		printf("malloc failed at %s line %u\n", __FILE__, __LINE__);
		perror("");
		exit(EXIT_FAILURE);
	}
	if (START < END) {
		for (size_t index = 0; index <= LENGTH; index++) {
			sequence[index] = start + index;
		}
	} else {
		for (size_t index = 0; index <= LENGTH; index++) {
			sequence[index] = end - index;
		}
	}
	return sequence;
}

//modified from https://phoxis.org/2012/07/12/get-sorted-index-orderting-of-an-array/

double *restrict base_arr = NULL;

static int compar_increase (const void *restrict a, const void *restrict b) {
	int aa = *((int *restrict ) a), bb = *((int *restrict) b);
	if (base_arr[aa] < base_arr[bb]) {
		return 1;
	} else if (base_arr[aa] == base_arr[bb]) {
		return 0;
	} else {
		return -1;
	}
}

static int compar_decrease (const void *restrict a, const void *restrict b) {
	int aa = *((int *restrict ) a), bb = *((int *restrict) b);
	if (base_arr[aa] < base_arr[bb]) {
		return -1;
	} else if (base_arr[aa] == base_arr[bb]) {
		return 0;
	} else {
		return 1;
	}
}

unsigned int *restrict order (const double *restrict ARRAY, const unsigned int SIZE, const bool DECREASING) {
//this has the same name as the same R function
	unsigned int *restrict idx = malloc(SIZE * sizeof(unsigned int));
	if (idx == NULL) {
			printf("failed to malloc at %s line %u.\n", __FILE__, __LINE__);
			perror("");
			exit(EXIT_FAILURE);
	}
	base_arr = malloc(sizeof(double) * SIZE);
	if (base_arr == NULL) {
			printf("failed to malloc at %s line %u.\n", __FILE__, __LINE__);
			perror("");
			exit(EXIT_FAILURE);
	}
	for (unsigned int i = 0; i < SIZE; i++) {
		base_arr[i] = ARRAY[i];
		idx[i] = i;
	}
	if (DECREASING == false) {
		qsort(idx, SIZE, sizeof(unsigned int), compar_decrease);
	} else if (DECREASING == true) {
		qsort(idx, SIZE, sizeof(unsigned int), compar_increase);
	}
	free(base_arr); base_arr = NULL;
	return idx;
}

double *restrict cummin(const double *restrict ARRAY, const unsigned int NO_OF_ARRAY_ELEMENTS) {
//this takes the same name of the R function which it copies
//this requires a free() afterward where it is used
	if (NO_OF_ARRAY_ELEMENTS < 1) {
		puts("cummin function requires at least one element.\n");
		printf("Failed at %s line %u\n", __FILE__, __LINE__);
		exit(EXIT_FAILURE);
	}
	double *restrict output = malloc(sizeof(double) * NO_OF_ARRAY_ELEMENTS);
	if (output == NULL) {
			printf("failed to malloc at %s line %u.\n", __FILE__, __LINE__);
			perror("");
			exit(EXIT_FAILURE);
	}
	double cumulative_min = ARRAY[0];
	for (unsigned int i = 0; i < NO_OF_ARRAY_ELEMENTS; i++) {
		if (ARRAY[i] < cumulative_min) {
			cumulative_min = ARRAY[i];
		}
		output[i] = cumulative_min;
	}
	return output;
}

double *restrict cummax(const double *restrict ARRAY, const unsigned int NO_OF_ARRAY_ELEMENTS) {
//this takes the same name of the R function which it copies
//this requires a free() afterward where it is used
	if (NO_OF_ARRAY_ELEMENTS < 1) {
		puts("function requires at least one element.\n");
		printf("Failed at %s line %u\n", __FILE__, __LINE__);
		exit(EXIT_FAILURE);
	}
	double *restrict output = malloc(sizeof(double) * NO_OF_ARRAY_ELEMENTS);
	if (output == NULL) {
			printf("failed to malloc at %s line %u.\n", __FILE__, __LINE__);
			perror("");
			exit(EXIT_FAILURE);
	}
	double cumulative_max = ARRAY[0];
	for (size_t i = 0; i < NO_OF_ARRAY_ELEMENTS; i++) {
		if (ARRAY[i] > cumulative_max) {
			cumulative_max = ARRAY[i];
		}
		output[i] = cumulative_max;
	}
	return output;
}

double *restrict pminx(const double *restrict ARRAY, const size_t NO_OF_ARRAY_ELEMENTS, const double X) {
//named after the R function pmin
	if (NO_OF_ARRAY_ELEMENTS < 1) {
		puts("pmin requires at least one element.\n");
		printf("Failed at %s line %u\n", __FILE__, __LINE__);
		exit(EXIT_FAILURE);
	}
	double *restrict pmin_array = malloc(sizeof(double) * NO_OF_ARRAY_ELEMENTS);
	if (pmin_array == NULL) {
			printf("failed to malloc at %s line %u.\n", __FILE__, __LINE__);
			perror("");
			exit(EXIT_FAILURE);
	}
	for (unsigned int index = 0; index < NO_OF_ARRAY_ELEMENTS; index++) {
		if (ARRAY[index] < X) {
			pmin_array[index] = ARRAY[index];
		} else {
			pmin_array[index] = X;
		}
	}
	return pmin_array;
}

void double_say (const double *restrict ARRAY, const size_t NO_OF_ARRAY_ELEMENTS) {
	printf("[1] %e", ARRAY[0]);
	for (size_t i = 1; i < NO_OF_ARRAY_ELEMENTS; i++) {
		printf(" %.10f", ARRAY[i]);
		if (((i+1) % 5) == 0) {
			printf("\n[%zu]", i+1);
		}
	}
	puts("\n");
}

/*void uint_say (const unsigned int *restrict ARRAY, const size_t NO_OF_ARRAY_ELEMENTS) {
//for debugging
	printf("%u", ARRAY[0]);
	for (size_t i = 1; i < NO_OF_ARRAY_ELEMENTS; i++) {
		printf(",%u", ARRAY[i]);
	}
	puts("\n");
}*/

double *restrict uint2double (const unsigned int *restrict ARRAY, const size_t NO_OF_ARRAY_ELEMENTS) {
	double *restrict doubleArray = malloc(sizeof(double) * NO_OF_ARRAY_ELEMENTS);
	if (doubleArray == NULL) {
		printf("Failure to malloc at %s line %u.\n", __FILE__, __LINE__);
		perror("");
		exit(EXIT_FAILURE);
	}
	for (size_t index = 0; index < NO_OF_ARRAY_ELEMENTS; index++) {
		doubleArray[index] = (double)ARRAY[index];
	}
	return doubleArray;
}

double min2 (const double N1, const double N2) {
	if (N1 < N2) {
		return N1;
	} else {
		return N2;
	}
}

double *restrict p_adjust (const double *restrict PVALUES, const size_t NO_OF_ARRAY_ELEMENTS, const char *restrict STRING) {
//this function is a translation of R's p.adjust "BH" method
// i is always i[index] = NO_OF_ARRAY_ELEMENTS - index - 1
	if (NO_OF_ARRAY_ELEMENTS < 1) {
		puts("p_adjust requires at least one element.\n");
		printf("Failed at %s line %u\n", __FILE__, __LINE__);
		exit(EXIT_FAILURE);
	}
	short int TYPE = -1;
	if (strcasecmp(STRING, "BH") == 0) {
		TYPE = 0;
	} else if (strcasecmp(STRING, "fdr") == 0) {
		TYPE = 0;
	} else if (strcasecmp(STRING, "by") == 0) {
		TYPE = 1;
	} else if (strcasecmp(STRING, "Bonferroni") == 0) {
		TYPE = 2;
	} else if (strcasecmp(STRING, "hochberg") == 0) {
		TYPE = 3;
	} else if (strcasecmp(STRING, "holm") == 0) {
		TYPE = 4;
	} else if (strcasecmp(STRING, "hommel") == 0) {
		TYPE = 5;
	} else {
		printf("%s doesn't match any accepted FDR methods.\n", STRING);
		printf("Failed at %s line %u\n", __FILE__, __LINE__);
		exit(EXIT_FAILURE);
	}
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
	if (TYPE == 2) {//Bonferroni method
		double *restrict bonferroni = malloc(sizeof(double) * NO_OF_ARRAY_ELEMENTS);
		if (bonferroni == NULL) {
			printf("failed to malloc at %s line %u.\n", __FILE__, __LINE__);
			perror("");
			exit(EXIT_FAILURE);
		}
		for (size_t index = 0; index < NO_OF_ARRAY_ELEMENTS; index++) {
			const double BONFERRONI = PVALUES[index] * NO_OF_ARRAY_ELEMENTS;
			if (BONFERRONI >= 1.0) {
				bonferroni[index] = 1.0;
			} else if ((0.0 <= BONFERRONI) && (BONFERRONI < 1.0)) {
				bonferroni[index] = BONFERRONI;
			} else {
				printf("%g is outside of the interval I planned.\n", BONFERRONI);
				printf("Failure at %s line %u\n", __FILE__, __LINE__);
				exit(EXIT_FAILURE);
			}
		}
		return bonferroni;
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
	} else if (TYPE == 4) {//Holm method
/*these values are computed separately from BH, BY, and Hochberg because they are
computed differently*/
		unsigned int *restrict o  = order(PVALUES, NO_OF_ARRAY_ELEMENTS, false);
//sorted in reverse of methods 0-3
		double *restrict o2double = uint2double(o, NO_OF_ARRAY_ELEMENTS);
		double *restrict cummax_input = malloc(sizeof(double) * NO_OF_ARRAY_ELEMENTS);
		for (size_t index = 0; index < NO_OF_ARRAY_ELEMENTS; index++) {
			cummax_input[index] = (NO_OF_ARRAY_ELEMENTS - index ) * PVALUES[o[index]];
//			printf("cummax_input[%zu] = %e\n", index, cummax_input[index]);
		}
		free(o); o = NULL;
		unsigned int *restrict ro = order(o2double, NO_OF_ARRAY_ELEMENTS, false);
		free(o2double); o2double = NULL;

		double *restrict cummax_output = cummax(cummax_input, NO_OF_ARRAY_ELEMENTS);
		free(cummax_input); cummax_input = NULL;

		double *restrict pmin = pminx(cummax_output, NO_OF_ARRAY_ELEMENTS, 1);
		free(cummax_output); cummax_output = NULL;
		double *restrict qvalues = malloc(sizeof(double) * NO_OF_ARRAY_ELEMENTS);
		for (size_t index = 0; index < NO_OF_ARRAY_ELEMENTS; index++) {
			qvalues[index] = pmin[ro[index]];
		}
		free(pmin); pmin = NULL;
		free(ro); ro = NULL;
		return qvalues;
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
	} else if (TYPE == 5) {//Hommel method
//i <- seq_len(n)
//o <- order(p)
		unsigned int *restrict o = order(PVALUES, NO_OF_ARRAY_ELEMENTS, false);//false is R's default
//p <- p[o]
		double *restrict p = malloc(sizeof(double) * NO_OF_ARRAY_ELEMENTS);
		if (p == NULL) {
			printf("failed to malloc at %s line %u.\n", __FILE__, __LINE__);
			perror("");
			exit(EXIT_FAILURE);
		}
		for (size_t index = 0; index < NO_OF_ARRAY_ELEMENTS; index++) {
			p[index] = PVALUES[o[index]];
		}
//ro <- order(o)
		double *restrict o2double = uint2double(o, NO_OF_ARRAY_ELEMENTS);
		free(o); o = NULL;
		unsigned int *restrict ro = order(o2double, NO_OF_ARRAY_ELEMENTS, false);
		free(o2double); o2double = NULL;
//		puts("ro");
//q <- pa <- rep.int(min(n * p/i), n)
		double *restrict q   = malloc(sizeof(double) * NO_OF_ARRAY_ELEMENTS);
		if (q == NULL) {
			printf("failed to malloc at %s line %u.\n", __FILE__, __LINE__);
			perror("");
			exit(EXIT_FAILURE);
		}
		double *restrict pa  = malloc(sizeof(double) * NO_OF_ARRAY_ELEMENTS);
		if (pa == NULL) {
			printf("failed to malloc at %s line %u.\n", __FILE__, __LINE__);
			perror("");
			exit(EXIT_FAILURE);
		}
		double min = (double)NO_OF_ARRAY_ELEMENTS * p[0];
		for (size_t index = 1; index < NO_OF_ARRAY_ELEMENTS; index++) {
			const double TEMP = (double)NO_OF_ARRAY_ELEMENTS * p[index] / (1+index);
			if (TEMP < min) {
				min = TEMP;
			}
		}
		for (size_t index = 0; index < NO_OF_ARRAY_ELEMENTS; index++) {
			pa[index] = min;
			 q[index] = min;
		}
//		puts("q & pa");
//		double_say(q, NO_OF_ARRAY_ELEMENTS);
/*for (j in (n - 1):2) {
            ij <- seq_len(n - j + 1)
            i2 <- (n - j + 2):n
            q1 <- min(j * p[i2]/(2:j))
            q[ij] <- pmin(j * p[ij], q1)
            q[i2] <- q[n - j + 1]
            pa <- pmax(pa, q)
        }
*/
		for (size_t j = (NO_OF_ARRAY_ELEMENTS-1); j >= 2; j--) {
//			printf("j = %zu\n", j);
			unsigned int *restrict ij = seq_len(1,NO_OF_ARRAY_ELEMENTS - j + 1);
			for (size_t i = 0; i < NO_OF_ARRAY_ELEMENTS - j + 1; i++) {
				ij[i]--;//R's indices are 1-based, C's are 0-based
			}
			const size_t I2_LENGTH = j - 1;
			unsigned int *restrict i2 = malloc(I2_LENGTH * sizeof(unsigned int));
			for (size_t i = 0; i < I2_LENGTH; i++) {
				i2[i] = NO_OF_ARRAY_ELEMENTS-j+2+i-1;
//R's indices are 1-based, C's are 0-based, I added the -1
			}

			double q1 = j * p[i2[0]] / 2.0;
			for (size_t i = 1; i < I2_LENGTH; i++) {//loop through 2:j
				const double TEMP_Q1 = (double)j * p[i2[i]] / (2 + i);
				if (TEMP_Q1 < q1) {
					q1 = TEMP_Q1;
				}
			}

			for (size_t i = 0; i < (NO_OF_ARRAY_ELEMENTS - j + 1); i++) {//q[ij] <- pmin(j * p[ij], q1)
				q[ij[i]] = min2( j*p[ij[i]], q1);
			}
			free(ij); ij = NULL;

			for (size_t i = 0; i < I2_LENGTH; i++) {//q[i2] <- q[n - j + 1]
				q[i2[i]] = q[NO_OF_ARRAY_ELEMENTS - j];//subtract 1 because of starting index difference
			}
			free(i2); i2 = NULL;

			for (size_t i = 0; i < NO_OF_ARRAY_ELEMENTS; i++) {//pa <- pmax(pa, q)
				if (pa[i] < q[i]) {
					pa[i] = q[i];
				}
			}
//			printf("j = %zu, pa = \n", j);
//				double_say(pa, N);
		}//end j loop
		free(p); p = NULL;
		for (size_t index = 0; index < NO_OF_ARRAY_ELEMENTS; index++) {
			q[index] = pa[ro[index]];//Hommel q-values
		}
//now free memory
		free(ro); ro = NULL;
		free(pa); pa = NULL;
		return q;
	}
//The methods are similarly computed and thus can be combined for clarity
	unsigned int *restrict o = order(PVALUES, NO_OF_ARRAY_ELEMENTS, true);
	if (o == NULL) {
			printf("failed to malloc at %s line %u.\n", __FILE__, __LINE__);
			perror("");
			exit(EXIT_FAILURE);
	}
	double *restrict o_double = uint2double(o, NO_OF_ARRAY_ELEMENTS);
	for (unsigned int index = 0; index < NO_OF_ARRAY_ELEMENTS; index++) {
		if ((PVALUES[index] < 0) || (PVALUES[index] > 1)) {
			printf("array[%u] = %lf, which is outside the interval [0,1]\n", index, PVALUES[index]);
			printf("died at %s line %u\n", __FILE__, __LINE__);
			exit(EXIT_FAILURE);
		}
	}

	unsigned int *restrict ro = order(o_double, NO_OF_ARRAY_ELEMENTS, false);
	if (ro == NULL) {
			printf("failed to malloc at %s line %u.\n", __FILE__, __LINE__);
			perror("");
			exit(EXIT_FAILURE);
	}
	free(o_double); o_double = NULL;
	double *restrict cummin_input = malloc(sizeof(double) * NO_OF_ARRAY_ELEMENTS);
	if (TYPE == 0) {//BH method
		for (size_t index = 0; index < NO_OF_ARRAY_ELEMENTS; index++) {
			const double NI = (double)NO_OF_ARRAY_ELEMENTS / (NO_OF_ARRAY_ELEMENTS - index);// n/i simplified
			cummin_input[index] = NI * PVALUES[o[index]];//PVALUES[o[index]] is p[o]
		}
	} else if (TYPE == 1) {//BY method
		double q = 1.0;
		for (size_t index = 2; index < (1+NO_OF_ARRAY_ELEMENTS); index++) {
			q += (double) 1.0/index;
		}
		for (size_t index = 0; index < NO_OF_ARRAY_ELEMENTS; index++) {
			const double NI = (double)NO_OF_ARRAY_ELEMENTS / (NO_OF_ARRAY_ELEMENTS - index);// n/i simplified
			cummin_input[index] = q * NI * PVALUES[o[index]];//PVALUES[o[index]] is p[o]
		}
	} else if (TYPE == 3) {//Hochberg method
		for (size_t index = 0; index < NO_OF_ARRAY_ELEMENTS; index++) {
// pmin(1, cummin((n - i + 1L) * p[o]))[ro]
			cummin_input[index] = (index + 1) * PVALUES[o[index]];
		}
	}
	free(o); o = NULL;
	double *restrict cummin_array = NULL;
	cummin_array = cummin(cummin_input, NO_OF_ARRAY_ELEMENTS);
	free(cummin_input); cummin_input = NULL;//I don't need this anymore
	double *restrict pmin = pminx(cummin_array, NO_OF_ARRAY_ELEMENTS, 1);
	free(cummin_array); cummin_array = NULL;
	double *restrict q_array = malloc(NO_OF_ARRAY_ELEMENTS*sizeof(double));
	for (unsigned int index = 0; index < NO_OF_ARRAY_ELEMENTS; index++) {
		q_array[index] = pmin[ro[index]];
	}

	free(ro); ro = NULL;
	free(pmin); pmin = NULL;
	return q_array;
}

int main(void) {
	const double PVALUES[] = {4.533744e-01, 7.296024e-01, 9.936026e-02, 9.079658e-02, 1.801962e-01,
8.752257e-01, 2.922222e-01, 9.115421e-01, 4.355806e-01, 5.324867e-01,
4.926798e-01, 5.802978e-01, 3.485442e-01, 7.883130e-01, 2.729308e-01,
8.502518e-01, 4.268138e-01, 6.442008e-01, 3.030266e-01, 5.001555e-02,
3.194810e-01, 7.892933e-01, 9.991834e-01, 1.745691e-01, 9.037516e-01,
1.198578e-01, 3.966083e-01, 1.403837e-02, 7.328671e-01, 6.793476e-02,
4.040730e-03, 3.033349e-04, 1.125147e-02, 2.375072e-02, 5.818542e-04,
3.075482e-04, 8.251272e-03, 1.356534e-03, 1.360696e-02, 3.764588e-04,
1.801145e-05, 2.504456e-07, 3.310253e-02, 9.427839e-03, 8.791153e-04,
2.177831e-04, 9.693054e-04, 6.610250e-05, 2.900813e-02, 5.735490e-03};//just the pvalues
	const double CORRECT_ANSWERS[6][50] = {//each first index is type
	{6.126681e-01, 8.521710e-01, 1.987205e-01, 1.891595e-01, 3.217789e-01,
9.301450e-01, 4.870370e-01, 9.301450e-01, 6.049731e-01, 6.826753e-01,
6.482629e-01, 7.253722e-01, 5.280973e-01, 8.769926e-01, 4.705703e-01,
9.241867e-01, 6.049731e-01, 7.856107e-01, 4.887526e-01, 1.136717e-01,
4.991891e-01, 8.769926e-01, 9.991834e-01, 3.217789e-01, 9.301450e-01,
2.304958e-01, 5.832475e-01, 3.899547e-02, 8.521710e-01, 1.476843e-01,
1.683638e-02, 2.562902e-03, 3.516084e-02, 6.250189e-02, 3.636589e-03,
2.562902e-03, 2.946883e-02, 6.166064e-03, 3.899547e-02, 2.688991e-03,
4.502862e-04, 1.252228e-05, 7.881555e-02, 3.142613e-02, 4.846527e-03,
2.562902e-03, 4.846527e-03, 1.101708e-03, 7.252032e-02, 2.205958e-02},//Benjamini-Hochberg
	{1.000000e+00, 1.000000e+00, 8.940844e-01, 8.510676e-01, 1.000000e+00,
1.000000e+00, 1.000000e+00, 1.000000e+00, 1.000000e+00, 1.000000e+00,
1.000000e+00, 1.000000e+00, 1.000000e+00, 1.000000e+00, 1.000000e+00,
1.000000e+00, 1.000000e+00, 1.000000e+00, 1.000000e+00, 5.114323e-01,
1.000000e+00, 1.000000e+00, 1.000000e+00, 1.000000e+00, 1.000000e+00,
1.000000e+00, 1.000000e+00, 1.754486e-01, 1.000000e+00, 6.644618e-01,
7.575031e-02, 1.153102e-02, 1.581959e-01, 2.812089e-01, 1.636176e-02,
1.153102e-02, 1.325863e-01, 2.774239e-02, 1.754486e-01, 1.209832e-02,
2.025930e-03, 5.634031e-05, 3.546073e-01, 1.413926e-01, 2.180552e-02,
1.153102e-02, 2.180552e-02, 4.956812e-03, 3.262838e-01, 9.925057e-02},//Benjamini & Yekutieli
	{1.000000e+00, 1.000000e+00, 1.000000e+00, 1.000000e+00, 1.000000e+00,
1.000000e+00, 1.000000e+00, 1.000000e+00, 1.000000e+00, 1.000000e+00,
1.000000e+00, 1.000000e+00, 1.000000e+00, 1.000000e+00, 1.000000e+00,
1.000000e+00, 1.000000e+00, 1.000000e+00, 1.000000e+00, 1.000000e+00,
1.000000e+00, 1.000000e+00, 1.000000e+00, 1.000000e+00, 1.000000e+00,
1.000000e+00, 1.000000e+00, 7.019185e-01, 1.000000e+00, 1.000000e+00,
2.020365e-01, 1.516674e-02, 5.625735e-01, 1.000000e+00, 2.909271e-02,
1.537741e-02, 4.125636e-01, 6.782670e-02, 6.803480e-01, 1.882294e-02,
9.005725e-04, 1.252228e-05, 1.000000e+00, 4.713920e-01, 4.395577e-02,
1.088915e-02, 4.846527e-02, 3.305125e-03, 1.000000e+00, 2.867745e-01},//Bonferroni
{9.991834e-01, 9.991834e-01, 9.991834e-01, 9.991834e-01, 9.991834e-01,
9.991834e-01, 9.991834e-01, 9.991834e-01, 9.991834e-01, 9.991834e-01,
9.991834e-01, 9.991834e-01, 9.991834e-01, 9.991834e-01, 9.991834e-01,
9.991834e-01, 9.991834e-01, 9.991834e-01, 9.991834e-01, 9.991834e-01,
9.991834e-01, 9.991834e-01, 9.991834e-01, 9.991834e-01, 9.991834e-01,
9.991834e-01, 9.991834e-01, 4.632662e-01, 9.991834e-01, 9.991834e-01,
1.575885e-01, 1.383967e-02, 3.938014e-01, 7.600230e-01, 2.501973e-02,
1.383967e-02, 3.052971e-01, 5.426136e-02, 4.626366e-01, 1.656419e-02,
8.825610e-04, 1.252228e-05, 9.930759e-01, 3.394022e-01, 3.692284e-02,
1.023581e-02, 3.974152e-02, 3.172920e-03, 8.992520e-01, 2.179486e-01},//Hochberg
	{1.000000e+00, 1.000000e+00, 1.000000e+00, 1.000000e+00, 1.000000e+00,
1.000000e+00, 1.000000e+00, 1.000000e+00, 1.000000e+00, 1.000000e+00,
1.000000e+00, 1.000000e+00, 1.000000e+00, 1.000000e+00, 1.000000e+00,
1.000000e+00, 1.000000e+00, 1.000000e+00, 1.000000e+00, 1.000000e+00,
1.000000e+00, 1.000000e+00, 1.000000e+00, 1.000000e+00, 1.000000e+00,
1.000000e+00, 1.000000e+00, 4.632662e-01, 1.000000e+00, 1.000000e+00,
1.575885e-01, 1.395341e-02, 3.938014e-01, 7.600230e-01, 2.501973e-02,
1.395341e-02, 3.052971e-01, 5.426136e-02, 4.626366e-01, 1.656419e-02,
8.825610e-04, 1.252228e-05, 9.930759e-01, 3.394022e-01, 3.692284e-02,
1.023581e-02, 3.974152e-02, 3.172920e-03, 8.992520e-01, 2.179486e-01},//Holm
{ 9.991834e-01, 9.991834e-01, 9.991834e-01, 9.987624e-01, 9.991834e-01,
9.991834e-01, 9.991834e-01, 9.991834e-01, 9.991834e-01, 9.991834e-01,
9.991834e-01, 9.991834e-01, 9.991834e-01, 9.991834e-01, 9.991834e-01,
9.991834e-01, 9.991834e-01, 9.991834e-01, 9.991834e-01, 9.595180e-01,
9.991834e-01, 9.991834e-01, 9.991834e-01, 9.991834e-01, 9.991834e-01,
9.991834e-01, 9.991834e-01, 4.351895e-01, 9.991834e-01, 9.766522e-01,
1.414256e-01, 1.304340e-02, 3.530937e-01, 6.887709e-01, 2.385602e-02,
1.322457e-02, 2.722920e-01, 5.426136e-02, 4.218158e-01, 1.581127e-02,
8.825610e-04, 1.252228e-05, 8.743649e-01, 3.016908e-01, 3.516461e-02,
9.582456e-03, 3.877222e-02, 3.172920e-03, 8.122276e-01, 1.950067e-01}//Hommel
	};
//the following loop checks each type with R's answers
	const char *restrict TYPES[] = {"bh", "by", "bonferroni", "hochberg", "holm", "hommel"};
	for (unsigned short int type = 0; type <= 5; type++) {
		double *restrict q = p_adjust(PVALUES, sizeof(PVALUES) / sizeof(*PVALUES), TYPES[type]);
		double error = fabs(q[0] - CORRECT_ANSWERS[type][0]);
//		printf("%e	-	%e	=	%g\n", q[0], CORRECT_ANSWERS[type][0], error);
	//	puts("p	q");
	//	printf("%g\t%g\n", pvalues[0], q[0]);
		for (unsigned int i = 1; i < sizeof(PVALUES) / sizeof(*PVALUES); i++) {
			const double this_error = fabs(q[i] - CORRECT_ANSWERS[type][i]);
//			printf("%e	-	%e	=	%g\n", q[i], CORRECT_ANSWERS[type][i], error);
			error += this_error;
		}
		double_say(q, sizeof(PVALUES) / sizeof(*PVALUES));
		free(q); q = NULL;
		printf("\ntype %u = '%s' has cumulative error of %g\n", type, TYPES[type], error);
	}

	return 0;
}

```


{{out}}

```txt
[1] 6.126681e-01 0.8521710465 0.1987205200 0.1891595417 0.3217789286
[5] 0.9301450000 0.4870370000 0.9301450000 0.6049730556 0.6826752564
[10] 0.6482628947 0.7253722500 0.5280972727 0.8769925556 0.4705703448
[15] 0.9241867391 0.6049730556 0.7856107317 0.4887525806 0.1136717045
[20] 0.4991890625 0.8769925556 0.9991834000 0.3217789286 0.9301450000
[25] 0.2304957692 0.5832475000 0.0389954722 0.8521710465 0.1476842609
[30] 0.0168363750 0.0025629017 0.0351608437 0.0625018947 0.0036365888
[35] 0.0025629017 0.0294688286 0.0061660636 0.0389954722 0.0026889914
[40] 0.0004502862 0.0000125223 0.0788155476 0.0314261300 0.0048465270
[45] 0.0025629017 0.0048465270 0.0011017083 0.0725203250 0.0220595769
[50]


type 0 = 'bh' has cumulative error of 8.03053e-07
[1] 1.000000e+00 1.0000000000 0.8940844244 0.8510676197 1.0000000000
[5] 1.0000000000 1.0000000000 1.0000000000 1.0000000000 1.0000000000
[10] 1.0000000000 1.0000000000 1.0000000000 1.0000000000 1.0000000000
[15] 1.0000000000 1.0000000000 1.0000000000 1.0000000000 0.5114323399
[20] 1.0000000000 1.0000000000 1.0000000000 1.0000000000 1.0000000000
[25] 1.0000000000 1.0000000000 0.1754486368 1.0000000000 0.6644618149
[30] 0.0757503083 0.0115310209 0.1581958559 0.2812088585 0.0163617595
[35] 0.0115310209 0.1325863108 0.0277423864 0.1754486368 0.0120983246
[40] 0.0020259303 0.0000563403 0.3546073326 0.1413926119 0.0218055202
[45] 0.0115310209 0.0218055202 0.0049568120 0.3262838334 0.0992505663
[50]


type 1 = 'by' has cumulative error of 3.64072e-07
[1] 1.000000e+00 1.0000000000 1.0000000000 1.0000000000 1.0000000000
[5] 1.0000000000 1.0000000000 1.0000000000 1.0000000000 1.0000000000
[10] 1.0000000000 1.0000000000 1.0000000000 1.0000000000 1.0000000000
[15] 1.0000000000 1.0000000000 1.0000000000 1.0000000000 1.0000000000
[20] 1.0000000000 1.0000000000 1.0000000000 1.0000000000 1.0000000000
[25] 1.0000000000 1.0000000000 0.7019185000 1.0000000000 1.0000000000
[30] 0.2020365000 0.0151667450 0.5625735000 1.0000000000 0.0290927100
[35] 0.0153774100 0.4125636000 0.0678267000 0.6803480000 0.0188229400
[40] 0.0009005725 0.0000125223 1.0000000000 0.4713919500 0.0439557650
[45] 0.0108891550 0.0484652700 0.0033051250 1.0000000000 0.2867745000
[50]


type 2 = 'bonferroni' has cumulative error of 6.5e-08
[1] 9.991834e-01 0.9991834000 0.9991834000 0.9991834000 0.9991834000
[5] 0.9991834000 0.9991834000 0.9991834000 0.9991834000 0.9991834000
[10] 0.9991834000 0.9991834000 0.9991834000 0.9991834000 0.9991834000
[15] 0.9991834000 0.9991834000 0.9991834000 0.9991834000 0.9991834000
[20] 0.9991834000 0.9991834000 0.9991834000 0.9991834000 0.9991834000
[25] 0.9991834000 0.9991834000 0.4632662100 0.9991834000 0.9991834000
[30] 0.1575884700 0.0138396690 0.3938014500 0.7600230400 0.0250197306
[35] 0.0138396690 0.3052970640 0.0542613600 0.4626366400 0.0165641872
[40] 0.0008825610 0.0000125223 0.9930759000 0.3394022040 0.0369228426
[45] 0.0102358057 0.0397415214 0.0031729200 0.8992520300 0.2179486200
[50]


type 3 = 'hochberg' has cumulative error of 2.7375e-07
[1] 1.000000e+00 1.0000000000 1.0000000000 1.0000000000 1.0000000000
[5] 1.0000000000 1.0000000000 1.0000000000 1.0000000000 1.0000000000
[10] 1.0000000000 1.0000000000 1.0000000000 1.0000000000 1.0000000000
[15] 1.0000000000 1.0000000000 1.0000000000 1.0000000000 1.0000000000
[20] 1.0000000000 1.0000000000 1.0000000000 1.0000000000 1.0000000000
[25] 1.0000000000 1.0000000000 0.4632662100 1.0000000000 1.0000000000
[30] 0.1575884700 0.0139534054 0.3938014500 0.7600230400 0.0250197306
[35] 0.0139534054 0.3052970640 0.0542613600 0.4626366400 0.0165641872
[40] 0.0008825610 0.0000125223 0.9930759000 0.3394022040 0.0369228426
[45] 0.0102358057 0.0397415214 0.0031729200 0.8992520300 0.2179486200
[50]


type 4 = 'holm' has cumulative error of 2.8095e-07
[1] 9.991834e-01 0.9991834000 0.9991834000 0.9987623800 0.9991834000
[5] 0.9991834000 0.9991834000 0.9991834000 0.9991834000 0.9991834000
[10] 0.9991834000 0.9991834000 0.9991834000 0.9991834000 0.9991834000
[15] 0.9991834000 0.9991834000 0.9991834000 0.9991834000 0.9595180000
[20] 0.9991834000 0.9991834000 0.9991834000 0.9991834000 0.9991834000
[25] 0.9991834000 0.9991834000 0.4351894700 0.9991834000 0.9766522500
[30] 0.1414255500 0.0130434007 0.3530936533 0.6887708800 0.0238560222
[35] 0.0132245726 0.2722919760 0.0542613600 0.4218157600 0.0158112696
[40] 0.0008825610 0.0000125223 0.8743649143 0.3016908480 0.0351646120
[45] 0.0095824564 0.0387722160 0.0031729200 0.8122276400 0.1950066600
[50]


type 5 = 'hommel' has cumulative error of 4.35302e-07
```



### Version 2

{{works with|C89}}
{{trans|Kotlin}}
To avoid licensing issues, this version is a translation of the Kotlin entry (Version 2) which is itself a partial translation of the Perl 6 entry. If using gcc, you need to link to the math library (-lm).

```c
#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#define SIZE 50
#define each_i(start, end) for (i = start; i < end; ++i)

typedef enum { UP, DOWN } direction;

typedef struct { int index; double value; } iv1;

typedef struct { int index; int value; } iv2;

/* test also for 'Unknown' correction type */
const char *types[8] = {
    "Benjamini-Hochberg", "Benjamini-Yekutieli", "Bonferroni", "Hochberg",
    "Holm", "Hommel", "Šidák", "Unknown"
};

int compare_iv1(const void *a, const void *b) {
    double aa = ((iv1 *)a) -> value;
    double bb = ((iv1 *)b) -> value;
    if (aa > bb) return 1;
    if (aa < bb) return -1;
    return 0;
}

int compare_iv1_desc(const void *a, const void *b) {
    return -compare_iv1(a, b);
}

int compare_iv2(const void *a, const void *b) {
    return ((iv2 *)a) -> value - ((iv2 *)b) -> value;
}

void ratchet(double *pa, direction dir) {
    int i;
    double m = pa[0];
    if (dir == UP) {
        each_i(1, SIZE) {
            if (pa[i] > m) pa[i] = m;
            m = pa[i];
        }
    }
    else {
        each_i(1, SIZE) {
            if (pa[i] < m) pa[i] = m;
            m = pa[i];
        }
    }
    each_i(0, SIZE) if (pa[i] > 1.0) pa[i] = 1.0;
}

void schwartzian(const double *p, double *pa, direction dir) {
    int i;
    int order[SIZE];
    int order2[SIZE];
    iv1 iv1s[SIZE];
    iv2 iv2s[SIZE];
    double pa2[SIZE];
    each_i(0, SIZE) { iv1s[i].index = i; iv1s[i].value = p[i]; }
    if (dir == UP)
        qsort(iv1s, SIZE, sizeof(iv1s[0]), compare_iv1_desc);
    else
        qsort(iv1s, SIZE, sizeof(iv1s[0]), compare_iv1);
    each_i(0, SIZE) order[i] = iv1s[i].index;
    each_i(0, SIZE) pa[i] *= p[order[i]];
    ratchet(pa, dir);
    each_i(0, SIZE) { iv2s[i].index = i; iv2s[i].value = order[i]; }
    qsort(iv2s, SIZE, sizeof(iv2s[0]), compare_iv2);
    each_i(0, SIZE) order2[i] = iv2s[i].index;
    each_i(0, SIZE) pa2[i] = pa[order2[i]];
    each_i(0, SIZE) pa[i] = pa2[i];
}

void adjust(const double *p, double *pa, const char *type) {
    int i;
    if (!strcmp(type, "Benjamini-Hochberg")) {
        each_i(0, SIZE) pa[i] = (double)SIZE / (SIZE - i);
        schwartzian(p, pa, UP);
    }
    else if (!strcmp(type, "Benjamini-Yekutieli")) {
        double q = 0.0;
        each_i(1, SIZE + 1) q += 1.0 / i;
        each_i(0, SIZE) pa[i] = q * SIZE / (SIZE - i);
        schwartzian(p, pa, UP);
    }
    else if (!strcmp(type, "Bonferroni")) {
        each_i(0, SIZE) pa[i] = (p[i] * SIZE > 1.0) ? 1.0 : p[i] * SIZE;
    }
    else if (!strcmp(type, "Hochberg")) {
        each_i(0, SIZE) pa[i]  = i + 1.0;
        schwartzian(p, pa, UP);
    }
    else if (!strcmp(type, "Holm")) {
        each_i(0, SIZE) pa[i] = SIZE - i;
        schwartzian(p, pa, DOWN);
    }
    else if (!strcmp(type, "Hommel")) {
        int i, j;
        int order[SIZE];
        int order2[SIZE];
        iv1 iv1s[SIZE];
        iv2 iv2s[SIZE];
        double s[SIZE];
        double q[SIZE];
        double pa2[SIZE];
        int indices[SIZE];
        each_i(0, SIZE) { iv1s[i].index = i; iv1s[i].value = p[i]; }
        qsort(iv1s, SIZE, sizeof(iv1s[0]), compare_iv1);
        each_i(0, SIZE) order[i] = iv1s[i].index;
        each_i(0, SIZE) s[i] = p[order[i]];
        double min = s[0] * SIZE;
        each_i(1, SIZE) {
            double temp = s[i] / (i + 1.0);
            if (temp < min) min = temp;
        }
        each_i(0, SIZE) q[i] = min;
        each_i(0, SIZE) pa2[i] = min;
        for (j = SIZE - 1; j >= 2; --j) {
            each_i(0, SIZE) indices[i] = i;
            int upper_start = SIZE - j + 1;      /* upper indices start index */
            int upper_size = j - 1;              /* size of upper indices */
            int lower_size = SIZE - upper_size;  /* size of lower indices */
            double qmin = j * s[indices[upper_start]] / 2.0;
            each_i(1, upper_size) {
                double temp = s[indices[upper_start + i]] * j / (2.0 + i);
                if (temp < qmin) qmin = temp;
            }
            each_i(0, lower_size) {
                double temp = s[indices[i]] * j;
                q[indices[i]] = (temp < qmin) ? temp : qmin;
            }
            each_i(0, upper_size) q[indices[upper_start + i]] = q[SIZE - j];
            each_i(0, SIZE) if (pa2[i] < q[i]) pa2[i] = q[i];
        }
        each_i(0, SIZE) { iv2s[i].index = i; iv2s[i].value = order[i]; }
        qsort(iv2s, SIZE, sizeof(iv2s[0]), compare_iv2);
        each_i(0, SIZE) order2[i] = iv2s[i].index;
        each_i(0, SIZE) pa[i] = pa2[order2[i]];
    }
    else if (!strcmp(type, "Šidák")) {
        each_i(0, SIZE) pa[i] = 1.0 - pow(1.0 - p[i], SIZE);
    }
    else {
        printf("\nSorry, do not know how to do '%s' correction.\n", type);
        printf("Perhaps you want one of these?:\n");
        each_i(0, 7) printf("  %s\n", types[i]);
        exit(1);
    }
}

void adjusted(const double *p, const char *type) {
    int i;
    double pa[SIZE] = { 0.0 };
    if (check(p)) {
        adjust(p, pa, type);
        printf("\n%s", type);
        each_i(0, SIZE) {
            if (!(i % 5)) printf("\n[%2d]  ", i);
            printf("%1.10f ", pa[i]);
        }
        printf("\n");
    }
    else {
        printf("p-values must be in range 0.0 to 1.0\n");
        exit(1);
    }
}

int check(const double* p) {
    int i;
    each_i(0, SIZE) {
        if (p[i] < 0.0 || p[i] > 1.0) return 0;
    }
    return 1;
}

int main() {
    int i;
    double p_values[SIZE] = {
        4.533744e-01, 7.296024e-01, 9.936026e-02, 9.079658e-02, 1.801962e-01,
        8.752257e-01, 2.922222e-01, 9.115421e-01, 4.355806e-01, 5.324867e-01,
        4.926798e-01, 5.802978e-01, 3.485442e-01, 7.883130e-01, 2.729308e-01,
        8.502518e-01, 4.268138e-01, 6.442008e-01, 3.030266e-01, 5.001555e-02,
        3.194810e-01, 7.892933e-01, 9.991834e-01, 1.745691e-01, 9.037516e-01,
        1.198578e-01, 3.966083e-01, 1.403837e-02, 7.328671e-01, 6.793476e-02,
        4.040730e-03, 3.033349e-04, 1.125147e-02, 2.375072e-02, 5.818542e-04,
        3.075482e-04, 8.251272e-03, 1.356534e-03, 1.360696e-02, 3.764588e-04,
        1.801145e-05, 2.504456e-07, 3.310253e-02, 9.427839e-03, 8.791153e-04,
        2.177831e-04, 9.693054e-04, 6.610250e-05, 2.900813e-02, 5.735490e-03
    };
    each_i(0, 8) adjusted(p_values, types[i]);
    return 0;
}
```


{{output}}

```txt

Same as Kotlin (Version 2) output.

```



## C++

{{trans|Java}}

```cpp
#include <algorithm>
#include <functional>
#include <iostream>
#include <numeric>
#include <vector>

std::vector<int> seqLen(int start, int end) {
    std::vector<int> result;

    if (start == end) {
        result.resize(end + 1);
        std::iota(result.begin(), result.end(), 1);
    } else if (start < end) {
        result.resize(end - start + 1);
        std::iota(result.begin(), result.end(), start);
    } else {
        result.resize(start - end + 1);
        std::iota(result.rbegin(), result.rend(), end);
    }

    return result;
}

std::vector<int> order(const std::vector<double>& arr, bool decreasing) {
    std::vector<int> idx(arr.size());
    std::iota(idx.begin(), idx.end(), 0);

    std::function<bool(int, int)> cmp;
    if (decreasing) {
        cmp = [&arr](int a, int b) { return arr[b] < arr[a]; };
    } else {
        cmp = [&arr](int a, int b) { return arr[a] < arr[b]; };
    }

    std::sort(idx.begin(), idx.end(), cmp);
    return idx;
}

std::vector<double> cummin(const std::vector<double>& arr) {
    if (arr.empty()) throw std::runtime_error("cummin requries at least one element");
    std::vector<double> output(arr.size());
    double cumulativeMin = arr[0];
    std::transform(arr.cbegin(), arr.cend(), output.begin(), [&cumulativeMin](double a) {
        if (a < cumulativeMin) cumulativeMin = a;
        return cumulativeMin;
    });
    return output;
}

std::vector<double> cummax(const std::vector<double>& arr) {
    if (arr.empty()) throw std::runtime_error("cummax requries at least one element");
    std::vector<double> output(arr.size());
    double cumulativeMax = arr[0];
    std::transform(arr.cbegin(), arr.cend(), output.begin(), [&cumulativeMax](double a) {
        if (cumulativeMax < a) cumulativeMax = a;
        return cumulativeMax;
    });
    return output;
}

std::vector<double> pminx(const std::vector<double>& arr, double x) {
    if (arr.empty()) throw std::runtime_error("pmin requries at least one element");
    std::vector<double> result(arr.size());
    std::transform(arr.cbegin(), arr.cend(), result.begin(), [&x](double a) {
        if (a < x) return a;
        return x;
    });
    return result;
}

void doubleSay(const std::vector<double>& arr) {
    printf("[ 1] %.10f", arr[0]);
    for (size_t i = 1; i < arr.size(); ++i) {
        printf(" %.10f", arr[i]);
        if ((i + 1) % 5 == 0) printf("\n[%2d]", i + 1);
    }
}

std::vector<double> pAdjust(const std::vector<double>& pvalues, const std::string& str) {
    if (pvalues.empty()) throw std::runtime_error("pAdjust requires at least one element");
    size_t size = pvalues.size();

    int type;
    if ("bh" == str || "fdr" == str) {
        type = 0;
    } else if ("by" == str) {
        type = 1;
    } else if ("bonferroni" == str) {
        type = 2;
    } else if ("hochberg" == str) {
        type = 3;
    } else if ("holm" == str) {
        type = 4;
    } else if ("hommel" == str) {
        type = 5;
    } else {
        throw std::runtime_error(str + " doesn't match any accepted FDR types");
    }

    // Bonferroni method
    if (2 == type) {
        std::vector<double> result(size);
        for (size_t i = 0; i < size; ++i) {
            double b = pvalues[i] * size;
            if (b >= 1) {
                result[i] = 1;
            } else if (0 <= b && b < 1) {
                result[i] = b;
            } else {
                throw std::runtime_error("a value is outside [0, 1)");
            }
        }
        return result;
    }
    // Holm method
    else if (4 == type) {
        auto o = order(pvalues, false);
        std::vector<double> o2Double(o.begin(), o.end());
        std::vector<double> cummaxInput(size);
        for (size_t i = 0; i < size; ++i) {
            cummaxInput[i] = (size - i) * pvalues[o[i]];
        }
        auto ro = order(o2Double, false);
        auto cummaxOutput = cummax(cummaxInput);
        auto pmin = pminx(cummaxOutput, 1.0);
        std::vector<double> result(size);
        std::transform(ro.cbegin(), ro.cend(), result.begin(), [&pmin](int a) { return pmin[a]; });
        return result;
    }
    // Hommel
    else if (5 == type) {
        auto indices = seqLen(size, size);
        auto o = order(pvalues, false);
        std::vector<double> p(size);
        std::transform(o.cbegin(), o.cend(), p.begin(), [&pvalues](int a) { return pvalues[a]; });
        std::vector<double> o2Double(o.begin(), o.end());
        auto ro = order(o2Double, false);
        std::vector<double> q(size);
        std::vector<double> pa(size);
        std::vector<double> npi(size);
        for (size_t i = 0; i < size; ++i) {
            npi[i] = p[i] * size / indices[i];
        }
        double min = *std::min_element(npi.begin(), npi.end());
        std::fill(q.begin(), q.end(), min);
        std::fill(pa.begin(), pa.end(), min);
        for (int j = size; j >= 2; --j) {
            auto ij = seqLen(1, size - j + 1);
            std::transform(ij.cbegin(), ij.cend(), ij.begin(), [](int a) { return a - 1; });
            int i2Length = j - 1;
            std::vector<int> i2(i2Length);
            for (int i = 0; i < i2Length; ++i) {
                i2[i] = size - j + 2 + i - 1;
            }
            double q1 = j * p[i2[0]] / 2.0;
            for (int i = 1; i < i2Length; ++i) {
                double temp_q1 = p[i2[i]] * j / (2.0 + i);
                if (temp_q1 < q1) q1 = temp_q1;
            }
            for (size_t i = 0; i < size - j + 1; ++i) {
                q[ij[i]] = std::min(p[ij[i]] * j, q1);
            }
            for (int i = 0; i < i2Length; ++i) {
                q[i2[i]] = q[size - j];
            }
            for (size_t i = 0; i < size; ++i) {
                if (pa[i] < q[i]) {
                    pa[i] = q[i];
                }
            }
        }
        std::transform(ro.cbegin(), ro.cend(), q.begin(), [&pa](int a) { return pa[a]; });
        return q;
    }

    std::vector<double> ni(size);
    std::vector<int> o = order(pvalues, true);
    std::vector<double> od(o.begin(), o.end());
    for (size_t i = 0; i < size; ++i) {
        if (pvalues[i] < 0 || pvalues[i]>1) {
            throw std::runtime_error("a value is outside [0, 1]");
        }
        ni[i] = (double)size / (size - i);
    }
    auto ro = order(od, false);
    std::vector<double> cumminInput(size);
    if (0 == type) {        // BH method
        for (size_t i = 0; i < size; ++i) {
            cumminInput[i] = ni[i] * pvalues[o[i]];
        }
    } else if (1 == type) { // BY method
        double q = 0;
        for (size_t i = 1; i < size + 1; ++i) {
            q += 1.0 / i;
        }
        for (size_t i = 0; i < size; ++i) {
            cumminInput[i] = q * ni[i] * pvalues[o[i]];
        }
    } else if (3 == type) { // Hochberg method
        for (size_t i = 0; i < size; ++i) {
            cumminInput[i] = (i + 1) * pvalues[o[i]];
        }
    }
    auto cumminArray = cummin(cumminInput);
    auto pmin = pminx(cumminArray, 1.0);
    std::vector<double> result(size);
    for (size_t i = 0; i < size; ++i) {
        result[i] = pmin[ro[i]];
    }
    return result;
}

int main() {
    using namespace std;

    vector<double> pvalues{
        4.533744e-01, 7.296024e-01, 9.936026e-02, 9.079658e-02, 1.801962e-01,
        8.752257e-01, 2.922222e-01, 9.115421e-01, 4.355806e-01, 5.324867e-01,
        4.926798e-01, 5.802978e-01, 3.485442e-01, 7.883130e-01, 2.729308e-01,
        8.502518e-01, 4.268138e-01, 6.442008e-01, 3.030266e-01, 5.001555e-02,
        3.194810e-01, 7.892933e-01, 9.991834e-01, 1.745691e-01, 9.037516e-01,
        1.198578e-01, 3.966083e-01, 1.403837e-02, 7.328671e-01, 6.793476e-02,
        4.040730e-03, 3.033349e-04, 1.125147e-02, 2.375072e-02, 5.818542e-04,
        3.075482e-04, 8.251272e-03, 1.356534e-03, 1.360696e-02, 3.764588e-04,
        1.801145e-05, 2.504456e-07, 3.310253e-02, 9.427839e-03, 8.791153e-04,
        2.177831e-04, 9.693054e-04, 6.610250e-05, 2.900813e-02, 5.735490e-03
    };

    vector<vector<double>> correctAnswers{
        // Benjamini-Hochberg
        {
            6.126681e-01, 8.521710e-01, 1.987205e-01, 1.891595e-01, 3.217789e-01,
            9.301450e-01, 4.870370e-01, 9.301450e-01, 6.049731e-01, 6.826753e-01,
            6.482629e-01, 7.253722e-01, 5.280973e-01, 8.769926e-01, 4.705703e-01,
            9.241867e-01, 6.049731e-01, 7.856107e-01, 4.887526e-01, 1.136717e-01,
            4.991891e-01, 8.769926e-01, 9.991834e-01, 3.217789e-01, 9.301450e-01,
            2.304958e-01, 5.832475e-01, 3.899547e-02, 8.521710e-01, 1.476843e-01,
            1.683638e-02, 2.562902e-03, 3.516084e-02, 6.250189e-02, 3.636589e-03,
            2.562902e-03, 2.946883e-02, 6.166064e-03, 3.899547e-02, 2.688991e-03,
            4.502862e-04, 1.252228e-05, 7.881555e-02, 3.142613e-02, 4.846527e-03,
            2.562902e-03, 4.846527e-03, 1.101708e-03, 7.252032e-02, 2.205958e-02
        },
        // Benjamini & Yekutieli
        {
            1.000000e+00, 1.000000e+00, 8.940844e-01, 8.510676e-01, 1.000000e+00,
            1.000000e+00, 1.000000e+00, 1.000000e+00, 1.000000e+00, 1.000000e+00,
            1.000000e+00, 1.000000e+00, 1.000000e+00, 1.000000e+00, 1.000000e+00,
            1.000000e+00, 1.000000e+00, 1.000000e+00, 1.000000e+00, 5.114323e-01,
            1.000000e+00, 1.000000e+00, 1.000000e+00, 1.000000e+00, 1.000000e+00,
            1.000000e+00, 1.000000e+00, 1.754486e-01, 1.000000e+00, 6.644618e-01,
            7.575031e-02, 1.153102e-02, 1.581959e-01, 2.812089e-01, 1.636176e-02,
            1.153102e-02, 1.325863e-01, 2.774239e-02, 1.754486e-01, 1.209832e-02,
            2.025930e-03, 5.634031e-05, 3.546073e-01, 1.413926e-01, 2.180552e-02,
            1.153102e-02, 2.180552e-02, 4.956812e-03, 3.262838e-01, 9.925057e-02
        },
        // Bonferroni
        {
            1.000000e+00, 1.000000e+00, 1.000000e+00, 1.000000e+00, 1.000000e+00,
            1.000000e+00, 1.000000e+00, 1.000000e+00, 1.000000e+00, 1.000000e+00,
            1.000000e+00, 1.000000e+00, 1.000000e+00, 1.000000e+00, 1.000000e+00,
            1.000000e+00, 1.000000e+00, 1.000000e+00, 1.000000e+00, 1.000000e+00,
            1.000000e+00, 1.000000e+00, 1.000000e+00, 1.000000e+00, 1.000000e+00,
            1.000000e+00, 1.000000e+00, 7.019185e-01, 1.000000e+00, 1.000000e+00,
            2.020365e-01, 1.516674e-02, 5.625735e-01, 1.000000e+00, 2.909271e-02,
            1.537741e-02, 4.125636e-01, 6.782670e-02, 6.803480e-01, 1.882294e-02,
            9.005725e-04, 1.252228e-05, 1.000000e+00, 4.713920e-01, 4.395577e-02,
            1.088915e-02, 4.846527e-02, 3.305125e-03, 1.000000e+00, 2.867745e-01
        },
        // Hochberg
        {
            9.991834e-01, 9.991834e-01, 9.991834e-01, 9.991834e-01, 9.991834e-01,
            9.991834e-01, 9.991834e-01, 9.991834e-01, 9.991834e-01, 9.991834e-01,
            9.991834e-01, 9.991834e-01, 9.991834e-01, 9.991834e-01, 9.991834e-01,
            9.991834e-01, 9.991834e-01, 9.991834e-01, 9.991834e-01, 9.991834e-01,
            9.991834e-01, 9.991834e-01, 9.991834e-01, 9.991834e-01, 9.991834e-01,
            9.991834e-01, 9.991834e-01, 4.632662e-01, 9.991834e-01, 9.991834e-01,
            1.575885e-01, 1.383967e-02, 3.938014e-01, 7.600230e-01, 2.501973e-02,
            1.383967e-02, 3.052971e-01, 5.426136e-02, 4.626366e-01, 1.656419e-02,
            8.825610e-04, 1.252228e-05, 9.930759e-01, 3.394022e-01, 3.692284e-02,
            1.023581e-02, 3.974152e-02, 3.172920e-03, 8.992520e-01, 2.179486e-01
        },
        // Holm
        {
            1.000000e+00, 1.000000e+00, 1.000000e+00, 1.000000e+00, 1.000000e+00,
            1.000000e+00, 1.000000e+00, 1.000000e+00, 1.000000e+00, 1.000000e+00,
            1.000000e+00, 1.000000e+00, 1.000000e+00, 1.000000e+00, 1.000000e+00,
            1.000000e+00, 1.000000e+00, 1.000000e+00, 1.000000e+00, 1.000000e+00,
            1.000000e+00, 1.000000e+00, 1.000000e+00, 1.000000e+00, 1.000000e+00,
            1.000000e+00, 1.000000e+00, 4.632662e-01, 1.000000e+00, 1.000000e+00,
            1.575885e-01, 1.395341e-02, 3.938014e-01, 7.600230e-01, 2.501973e-02,
            1.395341e-02, 3.052971e-01, 5.426136e-02, 4.626366e-01, 1.656419e-02,
            8.825610e-04, 1.252228e-05, 9.930759e-01, 3.394022e-01, 3.692284e-02,
            1.023581e-02, 3.974152e-02, 3.172920e-03, 8.992520e-01, 2.179486e-01
        },
        // Hommel
        {
            9.991834e-01, 9.991834e-01, 9.991834e-01, 9.987624e-01, 9.991834e-01,
            9.991834e-01, 9.991834e-01, 9.991834e-01, 9.991834e-01, 9.991834e-01,
            9.991834e-01, 9.991834e-01, 9.991834e-01, 9.991834e-01, 9.991834e-01,
            9.991834e-01, 9.991834e-01, 9.991834e-01, 9.991834e-01, 9.595180e-01,
            9.991834e-01, 9.991834e-01, 9.991834e-01, 9.991834e-01, 9.991834e-01,
            9.991834e-01, 9.991834e-01, 4.351895e-01, 9.991834e-01, 9.766522e-01,
            1.414256e-01, 1.304340e-02, 3.530937e-01, 6.887709e-01, 2.385602e-02,
            1.322457e-02, 2.722920e-01, 5.426136e-02, 4.218158e-01, 1.581127e-02,
            8.825610e-04, 1.252228e-05, 8.743649e-01, 3.016908e-01, 3.516461e-02,
            9.582456e-03, 3.877222e-02, 3.172920e-03, 8.122276e-01, 1.950067e-01
        }
    };

    vector<string> types{ "bh", "by", "bonferroni", "hochberg", "holm", "hommel" };
    for (size_t type = 0; type < types.size(); ++type) {
        auto q = pAdjust(pvalues, types[type]);
        double error = 0.0;
        for (size_t i = 0; i < pvalues.size(); ++i) {
            error += abs(q[i] - correctAnswers[type][i]);
        }
        doubleSay(q);
        printf("\ntype = %d = '%s' has a cumulative error of %g\n\n\n", type, types[type].c_str(), error);
    }

    return 0;
}
```

{{out}}

```txt
[ 1] 0.6126681081 0.8521710465 0.1987205200 0.1891595417 0.3217789286
[ 5] 0.9301450000 0.4870370000 0.9301450000 0.6049730556 0.6826752564
[10] 0.6482628947 0.7253722500 0.5280972727 0.8769925556 0.4705703448
[15] 0.9241867391 0.6049730556 0.7856107317 0.4887525806 0.1136717045
[20] 0.4991890625 0.8769925556 0.9991834000 0.3217789286 0.9301450000
[25] 0.2304957692 0.5832475000 0.0389954722 0.8521710465 0.1476842609
[30] 0.0168363750 0.0025629017 0.0351608437 0.0625018947 0.0036365888
[35] 0.0025629017 0.0294688286 0.0061660636 0.0389954722 0.0026889914
[40] 0.0004502862 0.0000125223 0.0788155476 0.0314261300 0.0048465270
[45] 0.0025629017 0.0048465270 0.0011017083 0.0725203250 0.0220595769
[50]
type = 0 = 'bh' has a cumulative error of 8.03053e-07


[ 1] 1.0000000000 1.0000000000 0.8940844244 0.8510676197 1.0000000000
[ 5] 1.0000000000 1.0000000000 1.0000000000 1.0000000000 1.0000000000
[10] 1.0000000000 1.0000000000 1.0000000000 1.0000000000 1.0000000000
[15] 1.0000000000 1.0000000000 1.0000000000 1.0000000000 0.5114323399
[20] 1.0000000000 1.0000000000 1.0000000000 1.0000000000 1.0000000000
[25] 1.0000000000 1.0000000000 0.1754486368 1.0000000000 0.6644618149
[30] 0.0757503083 0.0115310209 0.1581958559 0.2812088585 0.0163617595
[35] 0.0115310209 0.1325863108 0.0277423864 0.1754486368 0.0120983246
[40] 0.0020259303 0.0000563403 0.3546073326 0.1413926119 0.0218055202
[45] 0.0115310209 0.0218055202 0.0049568120 0.3262838334 0.0992505663
[50]
type = 1 = 'by' has a cumulative error of 3.64072e-07


[ 1] 1.0000000000 1.0000000000 1.0000000000 1.0000000000 1.0000000000
[ 5] 1.0000000000 1.0000000000 1.0000000000 1.0000000000 1.0000000000
[10] 1.0000000000 1.0000000000 1.0000000000 1.0000000000 1.0000000000
[15] 1.0000000000 1.0000000000 1.0000000000 1.0000000000 1.0000000000
[20] 1.0000000000 1.0000000000 1.0000000000 1.0000000000 1.0000000000
[25] 1.0000000000 1.0000000000 0.7019185000 1.0000000000 1.0000000000
[30] 0.2020365000 0.0151667450 0.5625735000 1.0000000000 0.0290927100
[35] 0.0153774100 0.4125636000 0.0678267000 0.6803480000 0.0188229400
[40] 0.0009005725 0.0000125223 1.0000000000 0.4713919500 0.0439557650
[45] 0.0108891550 0.0484652700 0.0033051250 1.0000000000 0.2867745000
[50]
type = 2 = 'bonferroni' has a cumulative error of 6.5e-08


[ 1] 0.9991834000 0.9991834000 0.9991834000 0.9991834000 0.9991834000
[ 5] 0.9991834000 0.9991834000 0.9991834000 0.9991834000 0.9991834000
[10] 0.9991834000 0.9991834000 0.9991834000 0.9991834000 0.9991834000
[15] 0.9991834000 0.9991834000 0.9991834000 0.9991834000 0.9991834000
[20] 0.9991834000 0.9991834000 0.9991834000 0.9991834000 0.9991834000
[25] 0.9991834000 0.9991834000 0.4632662100 0.9991834000 0.9991834000
[30] 0.1575884700 0.0138396690 0.3938014500 0.7600230400 0.0250197306
[35] 0.0138396690 0.3052970640 0.0542613600 0.4626366400 0.0165641872
[40] 0.0008825610 0.0000125223 0.9930759000 0.3394022040 0.0369228426
[45] 0.0102358057 0.0397415214 0.0031729200 0.8992520300 0.2179486200
[50]
type = 3 = 'hochberg' has a cumulative error of 2.7375e-07


[ 1] 1.0000000000 1.0000000000 1.0000000000 1.0000000000 1.0000000000
[ 5] 1.0000000000 1.0000000000 1.0000000000 1.0000000000 1.0000000000
[10] 1.0000000000 1.0000000000 1.0000000000 1.0000000000 1.0000000000
[15] 1.0000000000 1.0000000000 1.0000000000 1.0000000000 1.0000000000
[20] 1.0000000000 1.0000000000 1.0000000000 1.0000000000 1.0000000000
[25] 1.0000000000 1.0000000000 0.4632662100 1.0000000000 1.0000000000
[30] 0.1575884700 0.0139534054 0.3938014500 0.7600230400 0.0250197306
[35] 0.0139534054 0.3052970640 0.0542613600 0.4626366400 0.0165641872
[40] 0.0008825610 0.0000125223 0.9930759000 0.3394022040 0.0369228426
[45] 0.0102358057 0.0397415214 0.0031729200 0.8992520300 0.2179486200
[50]
type = 4 = 'holm' has a cumulative error of 2.8095e-07


[ 1] 0.9991834000 0.9991834000 0.9991834000 0.9987623800 0.9991834000
[ 5] 0.9991834000 0.9991834000 0.9991834000 0.9991834000 0.9991834000
[10] 0.9991834000 0.9991834000 0.9991834000 0.9991834000 0.9991834000
[15] 0.9991834000 0.9991834000 0.9991834000 0.9991834000 0.9595180000
[20] 0.9991834000 0.9991834000 0.9991834000 0.9991834000 0.9991834000
[25] 0.9991834000 0.9991834000 0.4351894700 0.9991834000 0.9766522500
[30] 0.1414255500 0.0130434007 0.3530936533 0.6887708800 0.0238560222
[35] 0.0132245726 0.2722919760 0.0542613600 0.4218157600 0.0158112696
[40] 0.0008825610 0.0000125223 0.8743649143 0.3016908480 0.0351646120
[45] 0.0095824564 0.0387722160 0.0031729200 0.8122276400 0.1950066600
[50]
type = 5 = 'hommel' has a cumulative error of 4.35302e-07
```


## C#
{{trans|Java}}

```c#
using System;
using System.Collections.Generic;
using System.Linq;

namespace PValueCorrection {
    class Program {
        static List<int> SeqLen(int start, int end) {
            var result = new List<int>();
            if (start == end) {
                for (int i = 0; i < end + 1; ++i) {
                    result.Add(i + 1);
                }
            } else if (start < end) {
                for (int i = 0; i < end - start + 1; ++i) {
                    result.Add(start + i);
                }
            } else {
                for (int i = 0; i < start - end + 1; ++i) {
                    result.Add(start - i);
                }
            }
            return result;
        }

        static List<int> Order(List<double> array, bool decreasing) {
            List<int> idx = new List<int>();
            for (int i = 0; i < array.Count; ++i) {
                idx.Add(i);
            }

            IComparer<int> cmp;
            if (decreasing) {
                cmp = Comparer<int>.Create((a, b) => array[a] < array[b] ? 1 : array[b] < array[a] ? -1 : 0);
            } else {
                cmp = Comparer<int>.Create((a, b) => array[b] < array[a] ? 1 : array[a] < array[b] ? -1 : 0);
            }

            idx.Sort(cmp);
            return idx;
        }

        static List<double> Cummin(List<double> array) {
            if (array.Count < 1) throw new ArgumentOutOfRangeException("cummin requires at least one element");
            var output = new List<double>();
            double cumulativeMin = array[0];
            for (int i = 0; i < array.Count; ++i) {
                if (array[i] < cumulativeMin) cumulativeMin = array[i];
                output.Add(cumulativeMin);
            }
            return output;
        }

        static List<double> Cummax(List<double> array) {
            if (array.Count < 1) throw new ArgumentOutOfRangeException("cummax requires at least one element");
            var output = new List<double>();
            double cumulativeMax = array[0];
            for (int i = 0; i < array.Count; ++i) {
                if (array[i] > cumulativeMax) cumulativeMax = array[i];
                output.Add(cumulativeMax);
            }
            return output;
        }

        static List<double> Pminx(List<double> array, double x) {
            if (array.Count < 1) throw new ArgumentOutOfRangeException("pmin requires at least one element");
            var result = new List<double>();
            for (int i = 0; i < array.Count; ++i) {
                if (array[i] < x) {
                    result.Add(array[i]);
                } else {
                    result.Add(x);
                }
            }
            return result;
        }

        static void Say(List<double> array) {
            Console.Write("[ 1] {0:E}", array[0]);
            for (int i = 1; i < array.Count; ++i) {
                Console.Write(" {0:E}", array[i]);
                if ((i + 1) % 5 == 0) Console.Write("\n[{0,2}]", i + 1);
            }
            Console.WriteLine();
        }

        static List<double> PAdjust(List<double> pvalues, string str) {
            var size = pvalues.Count;
            if (size < 1) throw new ArgumentOutOfRangeException("pAdjust requires at least one element");

            int type;
            switch (str.ToLower()) {
                case "bh":
                case "fdr":
                    type = 0;
                    break;
                case "by":
                    type = 1;
                    break;
                case "bonferroni":
                    type = 2;
                    break;
                case "hochberg":
                    type = 3;
                    break;
                case "holm":
                    type = 4;
                    break;
                case "hommel":
                    type = 5;
                    break;
                default:
                    throw new ArgumentException(str + " doesn't match any accepted FDR types");
            }

            if (2 == type) { // Bonferroni method
                var result2 = new List<double>();
                for (int i = 0; i < size; ++i) {
                    double b = pvalues[i] * size;
                    if (b >= 1) {
                        result2.Add(1);
                    } else if (0 <= b && b < 1) {
                        result2.Add(b);
                    } else {
                        throw new Exception(b + " is outside [0, 1)");
                    }
                }
                return result2;
            } else if (4 == type) { // Holm method
                var o4 = Order(pvalues, false);
                var o4d = o4.ConvertAll(x => (double)x);
                var cummaxInput = new List<double>();
                for (int i = 0; i < size; ++i) {
                    cummaxInput.Add((size - i) * pvalues[o4[i]]);
                }
                var ro4 = Order(o4d, false);
                var cummaxOutput = Cummax(cummaxInput);
                var pmin4 = Pminx(cummaxOutput, 1.0);
                var hr = new List<double>();
                for (int i = 0; i < size; ++i) {
                    hr.Add(pmin4[ro4[i]]);
                }
                return hr;
            } else if (5 == type) { // Hommel method
                var indices = SeqLen(size, size);
                var o5 = Order(pvalues, false);
                var p = new List<double>();
                for (int i = 0; i < size; ++i) {
                    p.Add(pvalues[o5[i]]);
                }
                var o5d = o5.ConvertAll(x => (double)x);
                var ro5 = Order(o5d, false);
                var q = new List<double>();
                var pa = new List<double>();
                var npi = new List<double>();
                for (int i = 0; i < size; ++i) {
                    npi.Add(p[i] * size / indices[i]);
                }
                double min = npi.Min();
                q.AddRange(Enumerable.Repeat(min, size));
                pa.AddRange(Enumerable.Repeat(min, size));
                for (int j = size; j >= 2; --j) {
                    var ij = SeqLen(1, size - j + 1);
                    for (int i = 0; i < size - j + 1; ++i) {
                        ij[i]--;
                    }
                    int i2Length = j - 1;
                    var i2 = new List<int>();
                    for (int i = 0; i < i2Length; ++i) {
                        i2.Add(size - j + 2 + i - 1);
                    }
                    double q1 = j * p[i2[0]] / 2.0;
                    for (int i = 1; i < i2Length; ++i) {
                        double temp_q1 = p[i2[i]] * j / (2.0 + i);
                        if (temp_q1 < q1) q1 = temp_q1;
                    }
                    for (int i = 0; i < size - j + 1; ++i) {
                        q[ij[i]] = Math.Min(p[ij[i]] * j, q1);
                    }
                    for (int i = 0; i < i2Length; ++i) {
                        q[i2[i]] = q[size - j];
                    }
                    for (int i = 0; i < size; ++i) {
                        if (pa[i] < q[i]) {
                            pa[i] = q[i];
                        }
                    }
                }
                for (int i = 0; i < size; ++i) {
                    q[i] = pa[ro5[i]];
                }
                return q;
            }

            var ni = new List<double>();
            var o = Order(pvalues, true);
            var od = o.ConvertAll(x => (double)x);
            for (int i = 0; i < size; ++i) {
                if (pvalues[i] < 0 || pvalues[i] > 1) {
                    throw new Exception("array[" + i + "] = " + pvalues[i] + " is outside [0, 1]");
                }
                ni.Add((double)size / (size - i));
            }
            var ro = Order(od, false);
            var cumminInput = new List<double>();
            if (0 == type) {       // BH method
                for (int i = 0; i < size; ++i) {
                    cumminInput.Add(ni[i] * pvalues[o[i]]);
                }
            } else if (1 == type) { // BY method
                double q = 0;
                for (int i = 1; i < size + 1; ++i) {
                    q += 1.0 / i;
                }
                for (int i = 0; i < size; ++i) {
                    cumminInput.Add(q * ni[i] * pvalues[o[i]]);
                }
            } else if (3 == type) { // Hochberg method
                for (int i = 0; i < size; ++i) {
                    cumminInput.Add((i + 1) * pvalues[o[i]]);
                }
            }
            var cumminArray = Cummin(cumminInput);
            var pmin = Pminx(cumminArray, 1.0);
            var result = new List<double>();
            for (int i = 0; i < size; ++i) {
                result.Add(pmin[ro[i]]);
            }
            return result;
        }

        static void Main(string[] args) {
            var pvalues = new List<double> {
                4.533744e-01, 7.296024e-01, 9.936026e-02, 9.079658e-02, 1.801962e-01,
                8.752257e-01, 2.922222e-01, 9.115421e-01, 4.355806e-01, 5.324867e-01,
                4.926798e-01, 5.802978e-01, 3.485442e-01, 7.883130e-01, 2.729308e-01,
                8.502518e-01, 4.268138e-01, 6.442008e-01, 3.030266e-01, 5.001555e-02,
                3.194810e-01, 7.892933e-01, 9.991834e-01, 1.745691e-01, 9.037516e-01,
                1.198578e-01, 3.966083e-01, 1.403837e-02, 7.328671e-01, 6.793476e-02,
                4.040730e-03, 3.033349e-04, 1.125147e-02, 2.375072e-02, 5.818542e-04,
                3.075482e-04, 8.251272e-03, 1.356534e-03, 1.360696e-02, 3.764588e-04,
                1.801145e-05, 2.504456e-07, 3.310253e-02, 9.427839e-03, 8.791153e-04,
                2.177831e-04, 9.693054e-04, 6.610250e-05, 2.900813e-02, 5.735490e-03
            };
            var correctAnswers = new List<List<double>> {
                new List<double> { // Benjamini-Hochberg
                    6.126681e-01, 8.521710e-01, 1.987205e-01, 1.891595e-01, 3.217789e-01,
                    9.301450e-01, 4.870370e-01, 9.301450e-01, 6.049731e-01, 6.826753e-01,
                    6.482629e-01, 7.253722e-01, 5.280973e-01, 8.769926e-01, 4.705703e-01,
                    9.241867e-01, 6.049731e-01, 7.856107e-01, 4.887526e-01, 1.136717e-01,
                    4.991891e-01, 8.769926e-01, 9.991834e-01, 3.217789e-01, 9.301450e-01,
                    2.304958e-01, 5.832475e-01, 3.899547e-02, 8.521710e-01, 1.476843e-01,
                    1.683638e-02, 2.562902e-03, 3.516084e-02, 6.250189e-02, 3.636589e-03,
                    2.562902e-03, 2.946883e-02, 6.166064e-03, 3.899547e-02, 2.688991e-03,
                    4.502862e-04, 1.252228e-05, 7.881555e-02, 3.142613e-02, 4.846527e-03,
                    2.562902e-03, 4.846527e-03, 1.101708e-03, 7.252032e-02, 2.205958e-02
                },
                new List<double> { // Benjamini & Yekutieli
                    1.000000e+00, 1.000000e+00, 8.940844e-01, 8.510676e-01, 1.000000e+00,
                    1.000000e+00, 1.000000e+00, 1.000000e+00, 1.000000e+00, 1.000000e+00,
                    1.000000e+00, 1.000000e+00, 1.000000e+00, 1.000000e+00, 1.000000e+00,
                    1.000000e+00, 1.000000e+00, 1.000000e+00, 1.000000e+00, 5.114323e-01,
                    1.000000e+00, 1.000000e+00, 1.000000e+00, 1.000000e+00, 1.000000e+00,
                    1.000000e+00, 1.000000e+00, 1.754486e-01, 1.000000e+00, 6.644618e-01,
                    7.575031e-02, 1.153102e-02, 1.581959e-01, 2.812089e-01, 1.636176e-02,
                    1.153102e-02, 1.325863e-01, 2.774239e-02, 1.754486e-01, 1.209832e-02,
                    2.025930e-03, 5.634031e-05, 3.546073e-01, 1.413926e-01, 2.180552e-02,
                    1.153102e-02, 2.180552e-02, 4.956812e-03, 3.262838e-01, 9.925057e-02
                },
                new List<double> { // Bonferroni
                    1.000000e+00, 1.000000e+00, 1.000000e+00, 1.000000e+00, 1.000000e+00,
                    1.000000e+00, 1.000000e+00, 1.000000e+00, 1.000000e+00, 1.000000e+00,
                    1.000000e+00, 1.000000e+00, 1.000000e+00, 1.000000e+00, 1.000000e+00,
                    1.000000e+00, 1.000000e+00, 1.000000e+00, 1.000000e+00, 1.000000e+00,
                    1.000000e+00, 1.000000e+00, 1.000000e+00, 1.000000e+00, 1.000000e+00,
                    1.000000e+00, 1.000000e+00, 7.019185e-01, 1.000000e+00, 1.000000e+00,
                    2.020365e-01, 1.516674e-02, 5.625735e-01, 1.000000e+00, 2.909271e-02,
                    1.537741e-02, 4.125636e-01, 6.782670e-02, 6.803480e-01, 1.882294e-02,
                    9.005725e-04, 1.252228e-05, 1.000000e+00, 4.713920e-01, 4.395577e-02,
                    1.088915e-02, 4.846527e-02, 3.305125e-03, 1.000000e+00, 2.867745e-01
                },
                new List<double> { // Hochberg
                    9.991834e-01, 9.991834e-01, 9.991834e-01, 9.991834e-01, 9.991834e-01,
                    9.991834e-01, 9.991834e-01, 9.991834e-01, 9.991834e-01, 9.991834e-01,
                    9.991834e-01, 9.991834e-01, 9.991834e-01, 9.991834e-01, 9.991834e-01,
                    9.991834e-01, 9.991834e-01, 9.991834e-01, 9.991834e-01, 9.991834e-01,
                    9.991834e-01, 9.991834e-01, 9.991834e-01, 9.991834e-01, 9.991834e-01,
                    9.991834e-01, 9.991834e-01, 4.632662e-01, 9.991834e-01, 9.991834e-01,
                    1.575885e-01, 1.383967e-02, 3.938014e-01, 7.600230e-01, 2.501973e-02,
                    1.383967e-02, 3.052971e-01, 5.426136e-02, 4.626366e-01, 1.656419e-02,
                    8.825610e-04, 1.252228e-05, 9.930759e-01, 3.394022e-01, 3.692284e-02,
                    1.023581e-02, 3.974152e-02, 3.172920e-03, 8.992520e-01, 2.179486e-01
                },
                new List<double> { // Holm
                    1.000000e+00, 1.000000e+00, 1.000000e+00, 1.000000e+00, 1.000000e+00,
                    1.000000e+00, 1.000000e+00, 1.000000e+00, 1.000000e+00, 1.000000e+00,
                    1.000000e+00, 1.000000e+00, 1.000000e+00, 1.000000e+00, 1.000000e+00,
                    1.000000e+00, 1.000000e+00, 1.000000e+00, 1.000000e+00, 1.000000e+00,
                    1.000000e+00, 1.000000e+00, 1.000000e+00, 1.000000e+00, 1.000000e+00,
                    1.000000e+00, 1.000000e+00, 4.632662e-01, 1.000000e+00, 1.000000e+00,
                    1.575885e-01, 1.395341e-02, 3.938014e-01, 7.600230e-01, 2.501973e-02,
                    1.395341e-02, 3.052971e-01, 5.426136e-02, 4.626366e-01, 1.656419e-02,
                    8.825610e-04, 1.252228e-05, 9.930759e-01, 3.394022e-01, 3.692284e-02,
                    1.023581e-02, 3.974152e-02, 3.172920e-03, 8.992520e-01, 2.179486e-01
                },
                new List<double> { // Hommel
                    9.991834e-01, 9.991834e-01, 9.991834e-01, 9.987624e-01, 9.991834e-01,
                    9.991834e-01, 9.991834e-01, 9.991834e-01, 9.991834e-01, 9.991834e-01,
                    9.991834e-01, 9.991834e-01, 9.991834e-01, 9.991834e-01, 9.991834e-01,
                    9.991834e-01, 9.991834e-01, 9.991834e-01, 9.991834e-01, 9.595180e-01,
                    9.991834e-01, 9.991834e-01, 9.991834e-01, 9.991834e-01, 9.991834e-01,
                    9.991834e-01, 9.991834e-01, 4.351895e-01, 9.991834e-01, 9.766522e-01,
                    1.414256e-01, 1.304340e-02, 3.530937e-01, 6.887709e-01, 2.385602e-02,
                    1.322457e-02, 2.722920e-01, 5.426136e-02, 4.218158e-01, 1.581127e-02,
                    8.825610e-04, 1.252228e-05, 8.743649e-01, 3.016908e-01, 3.516461e-02,
                    9.582456e-03, 3.877222e-02, 3.172920e-03, 8.122276e-01, 1.950067e-01
                }
            };

            string[] types = { "bh", "by", "bonferroni", "hochberg", "holm", "hommel" };
            for (int type = 0; type < types.Length; ++type) {
                var q = PAdjust(pvalues, types[type]);
                double error = 0.0;
                for (int i = 0; i < pvalues.Count; ++i) {
                    error += Math.Abs(q[i] - correctAnswers[type][i]);
                }
                Say(q);
                Console.WriteLine("type {0} = '{1}' has a cumulative error of {2:E}", type, types[type], error);
                Console.WriteLine();
            }
        }
    }
}
```

{{out}}

```txt
[ 1] 6.126681E-001 8.521710E-001 1.987205E-001 1.891595E-001 3.217789E-001
[ 5] 9.301450E-001 4.870370E-001 9.301450E-001 6.049731E-001 6.826753E-001
[10] 6.482629E-001 7.253723E-001 5.280973E-001 8.769926E-001 4.705703E-001
[15] 9.241867E-001 6.049731E-001 7.856107E-001 4.887526E-001 1.136717E-001
[20] 4.991891E-001 8.769926E-001 9.991834E-001 3.217789E-001 9.301450E-001
[25] 2.304958E-001 5.832475E-001 3.899547E-002 8.521710E-001 1.476843E-001
[30] 1.683638E-002 2.562902E-003 3.516084E-002 6.250189E-002 3.636589E-003
[35] 2.562902E-003 2.946883E-002 6.166064E-003 3.899547E-002 2.688991E-003
[40] 4.502863E-004 1.252228E-005 7.881555E-002 3.142613E-002 4.846527E-003
[45] 2.562902E-003 4.846527E-003 1.101708E-003 7.252033E-002 2.205958E-002
[50]
type 0 = 'bh' has a cumulative error of 8.030529E-007

[ 1] 1.000000E+000 1.000000E+000 8.940844E-001 8.510676E-001 1.000000E+000
[ 5] 1.000000E+000 1.000000E+000 1.000000E+000 1.000000E+000 1.000000E+000
[10] 1.000000E+000 1.000000E+000 1.000000E+000 1.000000E+000 1.000000E+000
[15] 1.000000E+000 1.000000E+000 1.000000E+000 1.000000E+000 5.114323E-001
[20] 1.000000E+000 1.000000E+000 1.000000E+000 1.000000E+000 1.000000E+000
[25] 1.000000E+000 1.000000E+000 1.754486E-001 1.000000E+000 6.644618E-001
[30] 7.575031E-002 1.153102E-002 1.581959E-001 2.812089E-001 1.636176E-002
[35] 1.153102E-002 1.325863E-001 2.774239E-002 1.754486E-001 1.209832E-002
[40] 2.025930E-003 5.634031E-005 3.546073E-001 1.413926E-001 2.180552E-002
[45] 1.153102E-002 2.180552E-002 4.956812E-003 3.262838E-001 9.925057E-002
[50]
type 1 = 'by' has a cumulative error of 3.640716E-007

[ 1] 1.000000E+000 1.000000E+000 1.000000E+000 1.000000E+000 1.000000E+000
[ 5] 1.000000E+000 1.000000E+000 1.000000E+000 1.000000E+000 1.000000E+000
[10] 1.000000E+000 1.000000E+000 1.000000E+000 1.000000E+000 1.000000E+000
[15] 1.000000E+000 1.000000E+000 1.000000E+000 1.000000E+000 1.000000E+000
[20] 1.000000E+000 1.000000E+000 1.000000E+000 1.000000E+000 1.000000E+000
[25] 1.000000E+000 1.000000E+000 7.019185E-001 1.000000E+000 1.000000E+000
[30] 2.020365E-001 1.516675E-002 5.625735E-001 1.000000E+000 2.909271E-002
[35] 1.537741E-002 4.125636E-001 6.782670E-002 6.803480E-001 1.882294E-002
[40] 9.005725E-004 1.252228E-005 1.000000E+000 4.713920E-001 4.395577E-002
[45] 1.088916E-002 4.846527E-002 3.305125E-003 1.000000E+000 2.867745E-001
[50]
type 2 = 'bonferroni' has a cumulative error of 6.500000E-008

[ 1] 9.991834E-001 9.991834E-001 9.991834E-001 9.991834E-001 9.991834E-001
[ 5] 9.991834E-001 9.991834E-001 9.991834E-001 9.991834E-001 9.991834E-001
[10] 9.991834E-001 9.991834E-001 9.991834E-001 9.991834E-001 9.991834E-001
[15] 9.991834E-001 9.991834E-001 9.991834E-001 9.991834E-001 9.991834E-001
[20] 9.991834E-001 9.991834E-001 9.991834E-001 9.991834E-001 9.991834E-001
[25] 9.991834E-001 9.991834E-001 4.632662E-001 9.991834E-001 9.991834E-001
[30] 1.575885E-001 1.383967E-002 3.938015E-001 7.600230E-001 2.501973E-002
[35] 1.383967E-002 3.052971E-001 5.426136E-002 4.626366E-001 1.656419E-002
[40] 8.825611E-004 1.252228E-005 9.930759E-001 3.394022E-001 3.692284E-002
[45] 1.023581E-002 3.974152E-002 3.172920E-003 8.992520E-001 2.179486E-001
[50]
type 3 = 'hochberg' has a cumulative error of 2.737500E-007

[ 1] 1.000000E+000 1.000000E+000 1.000000E+000 1.000000E+000 1.000000E+000
[ 5] 1.000000E+000 1.000000E+000 1.000000E+000 1.000000E+000 1.000000E+000
[10] 1.000000E+000 1.000000E+000 1.000000E+000 1.000000E+000 1.000000E+000
[15] 1.000000E+000 1.000000E+000 1.000000E+000 1.000000E+000 1.000000E+000
[20] 1.000000E+000 1.000000E+000 1.000000E+000 1.000000E+000 1.000000E+000
[25] 1.000000E+000 1.000000E+000 4.632662E-001 1.000000E+000 1.000000E+000
[30] 1.575885E-001 1.395341E-002 3.938015E-001 7.600230E-001 2.501973E-002
[35] 1.395341E-002 3.052971E-001 5.426136E-002 4.626366E-001 1.656419E-002
[40] 8.825611E-004 1.252228E-005 9.930759E-001 3.394022E-001 3.692284E-002
[45] 1.023581E-002 3.974152E-002 3.172920E-003 8.992520E-001 2.179486E-001
[50]
type 4 = 'holm' has a cumulative error of 2.809500E-007

[ 1] 9.991834E-001 9.991834E-001 9.991834E-001 9.987624E-001 9.991834E-001
[ 5] 9.991834E-001 9.991834E-001 9.991834E-001 9.991834E-001 9.991834E-001
[10] 9.991834E-001 9.991834E-001 9.991834E-001 9.991834E-001 9.991834E-001
[15] 9.991834E-001 9.991834E-001 9.991834E-001 9.991834E-001 9.595180E-001
[20] 9.991834E-001 9.991834E-001 9.991834E-001 9.991834E-001 9.991834E-001
[25] 9.991834E-001 9.991834E-001 4.351895E-001 9.991834E-001 9.766523E-001
[30] 1.414256E-001 1.304340E-002 3.530937E-001 6.887709E-001 2.385602E-002
[35] 1.322457E-002 2.722920E-001 5.426136E-002 4.218158E-001 1.581127E-002
[40] 8.825611E-004 1.252228E-005 8.743649E-001 3.016908E-001 3.516461E-002
[45] 9.582456E-003 3.877222E-002 3.172920E-003 8.122276E-001 1.950067E-001
[50]
type 5 = 'hommel' has a cumulative error of 4.353024E-007
```



## D

{{trans|Kotlin}}
''This work is based on R source code covered by the '''GPL''' license. It is thus a modified version, also covered by the GPL. See the [https://www.gnu.org/licenses/gpl-faq.html#GPLRequireSourcePostedPublic FAQ about GNU licenses]''.

```D
import std.algorithm;
import std.conv;
import std.math;
import std.stdio;
import std.string;

int[] seqLen(int start, int end) {
    int[] result;
    if (start == end) {
        result.length = end+1;
        for (int i; i<result.length; i++) {
            result[i] = i+1;
        }
    } else if (start < end) {
        result.length = end - start + 1;
        for (int i; i<result.length; i++) {
            result[i] = start+i;
        }
    } else {
        result.length = start - end + 1;
        for (int i; i<result.length; i++) {
            result[i] = start-i;
        }
    }
    return result;
}

int[] order(double[] array, bool decreasing) {
    int size = array.length;
    int[] idx;
    idx.length = size;
    double[] baseArr;
    baseArr.length = size;
    for (int i; i<size; i++) {
        baseArr[i] = array[i];
        idx[i] = i;
    }
    if (!decreasing) {
        alias comp = (a,b) => baseArr[a] < baseArr[b];
        idx.sort!comp;
    } else {
        alias comp = (a,b) => baseArr[b] < baseArr[a];
        idx.sort!comp;
    }
    return idx;
}

double[] cummin(double[] array) {
    int size = array.length;
    if (size < 1) throw new Exception("cummin requires at least one element");
    double[] output;
    output.length = size;
    auto cumulativeMin = array[0];
    foreach (i; 0..size) {
        if (array[i] < cumulativeMin) cumulativeMin = array[i];
        output[i] = cumulativeMin;
    }
    return output;
}

double[] cummax(double[] array) {
    auto size = array.length;
    if (size < 1) throw new Exception("cummax requires at least one element");
    double[] output;
    output.length = size;
    auto cumulativeMax = array[0];
    foreach (i; 0..size) {
        if (array[i] > cumulativeMax) cumulativeMax = array[i];
        output[i] = cumulativeMax;
    }
    return output;
}

double[] pminx(double[] array, double x) {
    auto size = array.length;
    if (size < 1) throw new Exception("pmin requires at least one element");
    double[] result;
    result.length = size;
    foreach (i; 0..size) {
        if (array[i] < x) {
            result[i] = array[i];
        } else {
            result[i] = x;
        }
    }
    return result;
}

void doubleSay(double[] array) {
    writef("[ 1] %e", array[0]);
    foreach (i; 1..array.length) {
        writef(" %.10f", array[i]);
        if ((i+1) % 5 == 0) writef("\n[%2d]", i+1);
    }
    writeln;
}

auto toArray(T,U)(U[] array) {
    T[] result;
    result.length = array.length;
    foreach(i; 0..array.length) {
        result[i] = to!T(array[i]);
    }
    return result;
}

double[] pAdjust(double[] pvalues, string str) {
    auto size = pvalues.length;
    if (size < 1) throw new Exception("pAdjust requires at least one element");
    int type = str.toLower.predSwitch!"a==b"(
        "bh",         0,
        "fdr",        0,
        "by",         1,
        "bonferroni", 2,
        "hochberg",   3,
        "holm",       4,
        "hommel",     5,
        { throw new Exception(text("'",str,"' doesn't match any accepted FDR types")); }()
    );
    if (type == 2) {  // Bonferroni method
        double[] result;
        result.length = size;
        foreach (i; 0..size) {
            auto b = pvalues[i] * size;
            if (b >= 1) {
                result[i] = 1;
            } else if (0 <= b && b < 1) {
                result[i] = b;
            } else {
                throw new Exception(text(b," is outside [0, 1)"));
            }
        }
        return result;
    } else if (type == 4) {  // Holm method
        auto o = order(pvalues, false);
        auto o2Double = toArray!(double,int)(o);
        double[] cummaxInput;
        cummaxInput.length = size;
        foreach (i; 0..size) {
            cummaxInput[i] = (size-i) * pvalues[o[i]];
        }
        auto ro = order(o2Double, false);
        auto cummaxOutput = cummax(cummaxInput);
        auto pmin = pminx(cummaxOutput, 1.0);
        double[] result;
        result.length = size;
        foreach (i; 0..size) {
            result[i] = pmin[ro[i]];
        }
        return result;
    } else if (type == 5) {
        auto indices = seqLen(size, size);
        auto o = order(pvalues, false);
        double[] p;
        p.length = size;
        foreach (i; 0..size) {
            p[i] = pvalues[o[i]];
        }
        auto o2Double = toArray!double(o);
        auto ro = order(o2Double, false);
        double[] q;
        q.length = size;
        double[] pa;
        pa.length = size;
        double[] npi;
        npi.length = size;
        foreach (i; 0..size) {
            npi[i] = p[i] * size / indices[i];
        }
        auto min_ = reduce!min(npi);
        q[] = min_;
        pa[] = min_;
        foreach_reverse (j; 2..size) {
            auto ij = seqLen(1, size - j + 1);
            foreach (i; 0..size-j+1) {
                ij[i]--;
            }
            auto i2Length = j-1;
            int[] i2;
            i2.length = i2Length;
            foreach(i; 0..i2Length) {
                i2[i] = size-j+2+i-1;
            }
            auto pi2Length = i2Length;
            double q1 = j*p[i2[0]] / 2.0;
            foreach (i; 1..pi2Length) {
                auto temp_q1 = p[i2[i]] * j / (2.0 + i);
                if (temp_q1 < q1) q1 = temp_q1;
            }
            foreach (i; 0..size-j+1) {
                q[ij[i]] = min(p[ij[i]] * j, q1);
            }
            foreach(i; 0..i2Length) {
                q[i2[i]] = q[size-j];
            }
            foreach(i; 0..size) if (pa[i] < q[i]) pa[i] = q[i];
        }
        foreach (index; 0..size) {
            q[index] = pa[ro[index]];
        }
        return q;
    }

    double[] ni;
    ni.length = size;
    auto o = order(pvalues, true);
    auto oDouble = toArray!double(o);
    foreach (index; 0..size) {
        if (pvalues[index] < 0 || pvalues[index] > 1) {
            throw new Exception(text("array[", index, "] = ", pvalues[index], " is outside [0, 1]"));
        }
        ni[index] = cast(double) size / (size - index);
    }
    auto ro = order(oDouble, false);
    double[] cumminInput;
    cumminInput.length = size;
    if (type == 0) {  // BH method
        foreach (index; 0..size) {
            cumminInput[index] = ni[index] * pvalues[o[index]];
        }
    } else if (type == 1) {  // BY method
        double q = 0;
        foreach (index; 1..size+1) q += 1.0 / index;
        foreach (index; 0..size) {
            cumminInput[index] = q * ni[index] * pvalues[o[index]];
        }
    } else if (type == 3) {  // Hochberg method
        foreach (index; 0..size) {
            cumminInput[index] = (index + 1) * pvalues[o[index]];
        }
    }
    auto cumminArray  =cummin(cumminInput);
    auto pmin = pminx(cumminArray, 1.0);
    double[] result;
    result.length = size;
    foreach (i; 0..size) {
        result[i] = pmin[ro[i]];
    }
    return result;
}

void main() {
    double[] pvalues = [
        4.533744e-01, 7.296024e-01, 9.936026e-02, 9.079658e-02, 1.801962e-01,
        8.752257e-01, 2.922222e-01, 9.115421e-01, 4.355806e-01, 5.324867e-01,
        4.926798e-01, 5.802978e-01, 3.485442e-01, 7.883130e-01, 2.729308e-01,
        8.502518e-01, 4.268138e-01, 6.442008e-01, 3.030266e-01, 5.001555e-02,
        3.194810e-01, 7.892933e-01, 9.991834e-01, 1.745691e-01, 9.037516e-01,
        1.198578e-01, 3.966083e-01, 1.403837e-02, 7.328671e-01, 6.793476e-02,
        4.040730e-03, 3.033349e-04, 1.125147e-02, 2.375072e-02, 5.818542e-04,
        3.075482e-04, 8.251272e-03, 1.356534e-03, 1.360696e-02, 3.764588e-04,
        1.801145e-05, 2.504456e-07, 3.310253e-02, 9.427839e-03, 8.791153e-04,
        2.177831e-04, 9.693054e-04, 6.610250e-05, 2.900813e-02, 5.735490e-03
    ];

    double[][] correctAnswers = [
        [  // Benjamini-Hochberg
            6.126681e-01, 8.521710e-01, 1.987205e-01, 1.891595e-01, 3.217789e-01,
            9.301450e-01, 4.870370e-01, 9.301450e-01, 6.049731e-01, 6.826753e-01,
            6.482629e-01, 7.253722e-01, 5.280973e-01, 8.769926e-01, 4.705703e-01,
            9.241867e-01, 6.049731e-01, 7.856107e-01, 4.887526e-01, 1.136717e-01,
            4.991891e-01, 8.769926e-01, 9.991834e-01, 3.217789e-01, 9.301450e-01,
            2.304958e-01, 5.832475e-01, 3.899547e-02, 8.521710e-01, 1.476843e-01,
            1.683638e-02, 2.562902e-03, 3.516084e-02, 6.250189e-02, 3.636589e-03,
            2.562902e-03, 2.946883e-02, 6.166064e-03, 3.899547e-02, 2.688991e-03,
            4.502862e-04, 1.252228e-05, 7.881555e-02, 3.142613e-02, 4.846527e-03,
            2.562902e-03, 4.846527e-03, 1.101708e-03, 7.252032e-02, 2.205958e-02
        ],
        [  // Benjamini & Yekutieli
            1.000000e+00, 1.000000e+00, 8.940844e-01, 8.510676e-01, 1.000000e+00,
            1.000000e+00, 1.000000e+00, 1.000000e+00, 1.000000e+00, 1.000000e+00,
            1.000000e+00, 1.000000e+00, 1.000000e+00, 1.000000e+00, 1.000000e+00,
            1.000000e+00, 1.000000e+00, 1.000000e+00, 1.000000e+00, 5.114323e-01,
            1.000000e+00, 1.000000e+00, 1.000000e+00, 1.000000e+00, 1.000000e+00,
            1.000000e+00, 1.000000e+00, 1.754486e-01, 1.000000e+00, 6.644618e-01,
            7.575031e-02, 1.153102e-02, 1.581959e-01, 2.812089e-01, 1.636176e-02,
            1.153102e-02, 1.325863e-01, 2.774239e-02, 1.754486e-01, 1.209832e-02,
            2.025930e-03, 5.634031e-05, 3.546073e-01, 1.413926e-01, 2.180552e-02,
            1.153102e-02, 2.180552e-02, 4.956812e-03, 3.262838e-01, 9.925057e-02
        ],
        [  // Bonferroni
            1.000000e+00, 1.000000e+00, 1.000000e+00, 1.000000e+00, 1.000000e+00,
            1.000000e+00, 1.000000e+00, 1.000000e+00, 1.000000e+00, 1.000000e+00,
            1.000000e+00, 1.000000e+00, 1.000000e+00, 1.000000e+00, 1.000000e+00,
            1.000000e+00, 1.000000e+00, 1.000000e+00, 1.000000e+00, 1.000000e+00,
            1.000000e+00, 1.000000e+00, 1.000000e+00, 1.000000e+00, 1.000000e+00,
            1.000000e+00, 1.000000e+00, 7.019185e-01, 1.000000e+00, 1.000000e+00,
            2.020365e-01, 1.516674e-02, 5.625735e-01, 1.000000e+00, 2.909271e-02,
            1.537741e-02, 4.125636e-01, 6.782670e-02, 6.803480e-01, 1.882294e-02,
            9.005725e-04, 1.252228e-05, 1.000000e+00, 4.713920e-01, 4.395577e-02,
            1.088915e-02, 4.846527e-02, 3.305125e-03, 1.000000e+00, 2.867745e-01
        ],
        [  // Hochberg
            9.991834e-01, 9.991834e-01, 9.991834e-01, 9.991834e-01, 9.991834e-01,
            9.991834e-01, 9.991834e-01, 9.991834e-01, 9.991834e-01, 9.991834e-01,
            9.991834e-01, 9.991834e-01, 9.991834e-01, 9.991834e-01, 9.991834e-01,
            9.991834e-01, 9.991834e-01, 9.991834e-01, 9.991834e-01, 9.991834e-01,
            9.991834e-01, 9.991834e-01, 9.991834e-01, 9.991834e-01, 9.991834e-01,
            9.991834e-01, 9.991834e-01, 4.632662e-01, 9.991834e-01, 9.991834e-01,
            1.575885e-01, 1.383967e-02, 3.938014e-01, 7.600230e-01, 2.501973e-02,
            1.383967e-02, 3.052971e-01, 5.426136e-02, 4.626366e-01, 1.656419e-02,
            8.825610e-04, 1.252228e-05, 9.930759e-01, 3.394022e-01, 3.692284e-02,
            1.023581e-02, 3.974152e-02, 3.172920e-03, 8.992520e-01, 2.179486e-01
        ],
        [  // Holm
            1.000000e+00, 1.000000e+00, 1.000000e+00, 1.000000e+00, 1.000000e+00,
            1.000000e+00, 1.000000e+00, 1.000000e+00, 1.000000e+00, 1.000000e+00,
            1.000000e+00, 1.000000e+00, 1.000000e+00, 1.000000e+00, 1.000000e+00,
            1.000000e+00, 1.000000e+00, 1.000000e+00, 1.000000e+00, 1.000000e+00,
            1.000000e+00, 1.000000e+00, 1.000000e+00, 1.000000e+00, 1.000000e+00,
            1.000000e+00, 1.000000e+00, 4.632662e-01, 1.000000e+00, 1.000000e+00,
            1.575885e-01, 1.395341e-02, 3.938014e-01, 7.600230e-01, 2.501973e-02,
            1.395341e-02, 3.052971e-01, 5.426136e-02, 4.626366e-01, 1.656419e-02,
            8.825610e-04, 1.252228e-05, 9.930759e-01, 3.394022e-01, 3.692284e-02,
            1.023581e-02, 3.974152e-02, 3.172920e-03, 8.992520e-01, 2.179486e-01
        ],
        [   // Hommel
            9.991834e-01, 9.991834e-01, 9.991834e-01, 9.987624e-01, 9.991834e-01,
            9.991834e-01, 9.991834e-01, 9.991834e-01, 9.991834e-01, 9.991834e-01,
            9.991834e-01, 9.991834e-01, 9.991834e-01, 9.991834e-01, 9.991834e-01,
            9.991834e-01, 9.991834e-01, 9.991834e-01, 9.991834e-01, 9.595180e-01,
            9.991834e-01, 9.991834e-01, 9.991834e-01, 9.991834e-01, 9.991834e-01,
            9.991834e-01, 9.991834e-01, 4.351895e-01, 9.991834e-01, 9.766522e-01,
            1.414256e-01, 1.304340e-02, 3.530937e-01, 6.887709e-01, 2.385602e-02,
            1.322457e-02, 2.722920e-01, 5.426136e-02, 4.218158e-01, 1.581127e-02,
            8.825610e-04, 1.252228e-05, 8.743649e-01, 3.016908e-01, 3.516461e-02,
            9.582456e-03, 3.877222e-02, 3.172920e-03, 8.122276e-01, 1.950067e-01
        ]
    ];
    auto types = ["bh", "by", "bonferroni", "hochberg", "holm", "hommel"];
    foreach (type; 0..types.length) {
        auto q = pAdjust(pvalues, types[type]);
        double error = 0.0;
        foreach (i; 0..pvalues.length) {
            error += abs(q[i] - correctAnswers[type][i]);
        }
        doubleSay(q);
        writefln("\ntype %d = '%s' has a cumulative error of %g", type, types[type], error);
    }
}
```

{{out}}

```txt
[ 1] 6.126681e-01 0.8521710465 0.1987205200 0.1891595417 0.3217789286
[ 5] 0.9301450000 0.4870370000 0.9301450000 0.6049730556 0.6826752564
[10] 0.6482628947 0.7253722500 0.5280972727 0.8769925556 0.4705703448
[15] 0.9241867391 0.6049730556 0.7856107317 0.4887525806 0.1136717045
[20] 0.4991890625 0.8769925556 0.9991834000 0.3217789286 0.9301450000
[25] 0.2304957692 0.5832475000 0.0389954722 0.8521710465 0.1476842609
[30] 0.0168363750 0.0025629016 0.0351608437 0.0625018947 0.0036365887
[35] 0.0025629016 0.0294688285 0.0061660636 0.0389954722 0.0026889914
[40] 0.0004502862 0.0000125222 0.0788155476 0.0314261300 0.0048465270
[45] 0.0025629016 0.0048465270 0.0011017083 0.0725203250 0.0220595769
[50]

type 0 = 'bh' has a cumulative error of 8.03053e-07
[ 1] 1.000000e+00 1.0000000000 0.8940844244 0.8510676197 1.0000000000
[ 5] 1.0000000000 1.0000000000 1.0000000000 1.0000000000 1.0000000000
[10] 1.0000000000 1.0000000000 1.0000000000 1.0000000000 1.0000000000
[15] 1.0000000000 1.0000000000 1.0000000000 1.0000000000 0.5114323399
[20] 1.0000000000 1.0000000000 1.0000000000 1.0000000000 1.0000000000
[25] 1.0000000000 1.0000000000 0.1754486368 1.0000000000 0.6644618149
[30] 0.0757503082 0.0115310208 0.1581958559 0.2812088585 0.0163617595
[35] 0.0115310208 0.1325863108 0.0277423864 0.1754486368 0.0120983245
[40] 0.0020259303 0.0000563403 0.3546073326 0.1413926119 0.0218055201
[45] 0.0115310208 0.0218055201 0.0049568120 0.3262838334 0.0992505662
[50]

type 1 = 'by' has a cumulative error of 3.64072e-07
[ 1] 1.000000e+00 1.0000000000 1.0000000000 1.0000000000 1.0000000000
[ 5] 1.0000000000 1.0000000000 1.0000000000 1.0000000000 1.0000000000
[10] 1.0000000000 1.0000000000 1.0000000000 1.0000000000 1.0000000000
[15] 1.0000000000 1.0000000000 1.0000000000 1.0000000000 1.0000000000
[20] 1.0000000000 1.0000000000 1.0000000000 1.0000000000 1.0000000000
[25] 1.0000000000 1.0000000000 0.7019185000 1.0000000000 1.0000000000
[30] 0.2020365000 0.0151667450 0.5625735000 1.0000000000 0.0290927100
[35] 0.0153774100 0.4125636000 0.0678267000 0.6803480000 0.0188229400
[40] 0.0009005725 0.0000125222 1.0000000000 0.4713919500 0.0439557650
[45] 0.0108891550 0.0484652700 0.0033051250 1.0000000000 0.2867745000
[50]

type 2 = 'bonferroni' has a cumulative error of 6.5e-08
[ 1] 9.991834e-01 0.9991834000 0.9991834000 0.9991834000 0.9991834000
[ 5] 0.9991834000 0.9991834000 0.9991834000 0.9991834000 0.9991834000
[10] 0.9991834000 0.9991834000 0.9991834000 0.9991834000 0.9991834000
[15] 0.9991834000 0.9991834000 0.9991834000 0.9991834000 0.9991834000
[20] 0.9991834000 0.9991834000 0.9991834000 0.9991834000 0.9991834000
[25] 0.9991834000 0.9991834000 0.4632662100 0.9991834000 0.9991834000
[30] 0.1575884700 0.0138396690 0.3938014500 0.7600230400 0.0250197306
[35] 0.0138396690 0.3052970640 0.0542613600 0.4626366400 0.0165641872
[40] 0.0008825610 0.0000125222 0.9930759000 0.3394022040 0.0369228426
[45] 0.0102358057 0.0397415214 0.0031729200 0.8992520300 0.2179486200
[50]

type 3 = 'hochberg' has a cumulative error of 2.7375e-07
[ 1] 1.000000e+00 1.0000000000 1.0000000000 1.0000000000 1.0000000000
[ 5] 1.0000000000 1.0000000000 1.0000000000 1.0000000000 1.0000000000
[10] 1.0000000000 1.0000000000 1.0000000000 1.0000000000 1.0000000000
[15] 1.0000000000 1.0000000000 1.0000000000 1.0000000000 1.0000000000
[20] 1.0000000000 1.0000000000 1.0000000000 1.0000000000 1.0000000000
[25] 1.0000000000 1.0000000000 0.4632662100 1.0000000000 1.0000000000
[30] 0.1575884700 0.0139534054 0.3938014500 0.7600230400 0.0250197306
[35] 0.0139534054 0.3052970640 0.0542613600 0.4626366400 0.0165641872
[40] 0.0008825610 0.0000125222 0.9930759000 0.3394022040 0.0369228426
[45] 0.0102358057 0.0397415214 0.0031729200 0.8992520300 0.2179486200
[50]

type 4 = 'holm' has a cumulative error of 2.8095e-07
[ 1] 9.991834e-01 0.9991834000 0.9991834000 0.9987623800 0.9991834000
[ 5] 0.9991834000 0.9991834000 0.9991834000 0.9991834000 0.9991834000
[10] 0.9991834000 0.9991834000 0.9991834000 0.9991834000 0.9991834000
[15] 0.9991834000 0.9991834000 0.9991834000 0.9991834000 0.9595180000
[20] 0.9991834000 0.9991834000 0.9991834000 0.9991834000 0.9991834000
[25] 0.9991834000 0.9991834000 0.4351894700 0.9991834000 0.9766522500
[30] 0.1414255500 0.0130434007 0.3530936533 0.6887708800 0.0238560222
[35] 0.0132245726 0.2722919760 0.0542613600 0.4218157600 0.0158112696
[40] 0.0008825610 0.0000125222 0.8743649143 0.3016908480 0.0351646120
[45] 0.0095824564 0.0387722160 0.0031729200 0.8122276400 0.1950066600
[50]

type 5 = 'hommel' has a cumulative error of 4.35302e-07
```



## Go

{{trans|Kotlin (Version 2)}}

```go
package main

import (
    "fmt"
    "log"
    "math"
    "os"
    "sort"
    "strconv"
    "strings"
)

type pvalues = []float64

type iv1 struct {
    index int
    value float64
}
type iv2 struct{ index, value int }

type direction int

const (
    up direction = iota
    down
)

// Test also for 'Unknown' correction type.
var ctypes = []string{
    "Benjamini-Hochberg", "Benjamini-Yekutieli", "Bonferroni", "Hochberg",
    "Holm", "Hommel", "Šidák", "Unknown",
}

func minimum(p pvalues) float64 {
    m := p[0]
    for i := 1; i < len(p); i++ {
        if p[i] < m {
            m = p[i]
        }
    }
    return m
}

func maximum(p pvalues) float64 {
    m := p[0]
    for i := 1; i < len(p); i++ {
        if p[i] > m {
            m = p[i]
        }
    }
    return m
}

func adjusted(p pvalues, ctype string) (string, error) {
    err := check(p)
    if err != nil {
        return "", err
    }
    temp := pformat(adjust(p, ctype), 5)
    return fmt.Sprintf("\n%s\n%s", ctype, temp), nil
}

func pformat(p pvalues, cols int) string {
    var lines []string
    for i := 0; i < len(p); i += cols {
        fchunk := p[i : i+cols]
        schunk := make([]string, cols)
        for j := 0; j < cols; j++ {
            schunk[j] = strconv.FormatFloat(fchunk[j], 'f', 10, 64)
        }
        lines = append(lines, fmt.Sprintf("[%2d]  %s", i, strings.Join(schunk, " ")))
    }
    return strings.Join(lines, "\n")
}

func check(p []float64) error {
    cond := len(p) > 0 && minimum(p) >= 0 && maximum(p) <= 1
    if !cond {
        return fmt.Errorf("p-values must be in range 0.0 to 1.0")
    }
    return nil
}

func ratchet(p pvalues, dir direction) {
    size := len(p)
    m := p[0]
    if dir == up {
        for i := 1; i < size; i++ {
            if p[i] > m {
                p[i] = m
            }
            m = p[i]
        }
    } else {
        for i := 1; i < size; i++ {
            if p[i] < m {
                p[i] = m
            }
            m = p[i]
        }
    }
    for i := 0; i < size; i++ {
        if p[i] > 1.0 {
            p[i] = 1.0
        }
    }
}

func schwartzian(p pvalues, mult pvalues, dir direction) pvalues {
    size := len(p)
    order := make([]int, size)
    iv1s := make([]iv1, size)
    for i := 0; i < size; i++ {
        iv1s[i] = iv1{i, p[i]}
    }
    if dir == up {
        sort.Slice(iv1s, func(i, j int) bool {
            return iv1s[i].value > iv1s[j].value
        })
    } else {
        sort.Slice(iv1s, func(i, j int) bool {
            return iv1s[i].value < iv1s[j].value
        })
    }
    for i := 0; i < size; i++ {
        order[i] = iv1s[i].index
    }
    pa := make(pvalues, size)
    for i := 0; i < size; i++ {
        pa[i] = mult[i] * p[order[i]]
    }
    ratchet(pa, dir)
    order2 := make([]int, size)
    iv2s := make([]iv2, size)
    for i := 0; i < size; i++ {
        iv2s[i] = iv2{i, order[i]}
    }
    sort.Slice(iv2s, func(i, j int) bool {
        return iv2s[i].value < iv2s[j].value
    })
    for i := 0; i < size; i++ {
        order2[i] = iv2s[i].index
    }
    pa2 := make(pvalues, size)
    for i := 0; i < size; i++ {
        pa2[i] = pa[order2[i]]
    }
    return pa2
}

func adjust(p pvalues, ctype string) pvalues {
    size := len(p)
    if size == 0 {
        return p
    }
    fsize := float64(size)
    switch ctype {
    case "Benjamini-Hochberg":
        mult := make(pvalues, size)
        for i := 0; i < size; i++ {
            mult[i] = fsize / float64(size-i)
        }
        return schwartzian(p, mult, up)
    case "Benjamini-Yekutieli":
        q := 0.0
        for i := 1; i <= size; i++ {
            q += 1.0 / float64(i)
        }
        mult := make(pvalues, size)
        for i := 0; i < size; i++ {
            mult[i] = q * fsize / (fsize - float64(i))
        }
        return schwartzian(p, mult, up)
    case "Bonferroni":
        p2 := make(pvalues, size)
        for i := 0; i < size; i++ {
            p2[i] = math.Min(p[i]*fsize, 1.0)
        }
        return p2
    case "Hochberg":
        mult := make(pvalues, size)
        for i := 0; i < size; i++ {
            mult[i] = float64(i) + 1
        }
        return schwartzian(p, mult, up)
    case "Holm":
        mult := make(pvalues, size)
        for i := 0; i < size; i++ {
            mult[i] = fsize - float64(i)
        }
        return schwartzian(p, mult, down)
    case "Hommel":
        order := make([]int, size)
        iv1s := make([]iv1, size)
        for i := 0; i < size; i++ {
            iv1s[i] = iv1{i, p[i]}
        }
        sort.Slice(iv1s, func(i, j int) bool {
            return iv1s[i].value < iv1s[j].value
        })
        for i := 0; i < size; i++ {
            order[i] = iv1s[i].index
        }
        s := make(pvalues, size)
        for i := 0; i < size; i++ {
            s[i] = p[order[i]]
        }
        m := make(pvalues, size)
        for i := 0; i < size; i++ {
            m[i] = s[i] * fsize / (float64(i) + 1)
        }
        min := minimum(m)
        q := make(pvalues, size)
        for i := 0; i < size; i++ {
            q[i] = min
        }
        pa := make(pvalues, size)
        for i := 0; i < size; i++ {
            pa[i] = min
        }
        for j := size - 1; j >= 2; j-- {
            lower := make([]int, size-j+1) // lower indices
            for i := 0; i < len(lower); i++ {
                lower[i] = i
            }
            upper := make([]int, j-1) // upper indices
            for i := 0; i < len(upper); i++ {
                upper[i] = size - j + 1 + i
            }
            qmin := float64(j) * s[upper[0]] / 2.0
            for i := 1; i < len(upper); i++ {
                temp := s[upper[i]] * float64(j) / (2.0 + float64(i))
                if temp < qmin {
                    qmin = temp
                }
            }
            for i := 0; i < len(lower); i++ {
                q[lower[i]] = math.Min(s[lower[i]]*float64(j), qmin)
            }
            for i := 0; i < len(upper); i++ {
                q[upper[i]] = q[size-j]
            }
            for i := 0; i < size; i++ {
                if pa[i] < q[i] {
                    pa[i] = q[i]
                }
            }
        }
        order2 := make([]int, size)
        iv2s := make([]iv2, size)
        for i := 0; i < size; i++ {
            iv2s[i] = iv2{i, order[i]}
        }
        sort.Slice(iv2s, func(i, j int) bool {
            return iv2s[i].value < iv2s[j].value
        })
        for i := 0; i < size; i++ {
            order2[i] = iv2s[i].index
        }
        pa2 := make(pvalues, size)
        for i := 0; i < size; i++ {
            pa2[i] = pa[order2[i]]
        }
        return pa2
    case "Šidák":
        p2 := make(pvalues, size)
        for i := 0; i < size; i++ {
            p2[i] = 1.0 - math.Pow(1.0-float64(p[i]), fsize)
        }
        return p2
    default:
        fmt.Printf("\nSorry, do not know how to do '%s' correction.\n", ctype)
        fmt.Println("Perhaps you want one of these?:")
        temp := make([]string, len(ctypes)-1)
        for i := 0; i < len(temp); i++ {
            temp[i] = fmt.Sprintf("  %s", ctypes[i])
        }
        fmt.Println(strings.Join(temp, "\n"))
        os.Exit(1)
    }
    return p
}

func main() {
    p := pvalues{
        4.533744e-01, 7.296024e-01, 9.936026e-02, 9.079658e-02, 1.801962e-01,
        8.752257e-01, 2.922222e-01, 9.115421e-01, 4.355806e-01, 5.324867e-01,
        4.926798e-01, 5.802978e-01, 3.485442e-01, 7.883130e-01, 2.729308e-01,
        8.502518e-01, 4.268138e-01, 6.442008e-01, 3.030266e-01, 5.001555e-02,
        3.194810e-01, 7.892933e-01, 9.991834e-01, 1.745691e-01, 9.037516e-01,
        1.198578e-01, 3.966083e-01, 1.403837e-02, 7.328671e-01, 6.793476e-02,
        4.040730e-03, 3.033349e-04, 1.125147e-02, 2.375072e-02, 5.818542e-04,
        3.075482e-04, 8.251272e-03, 1.356534e-03, 1.360696e-02, 3.764588e-04,
        1.801145e-05, 2.504456e-07, 3.310253e-02, 9.427839e-03, 8.791153e-04,
        2.177831e-04, 9.693054e-04, 6.610250e-05, 2.900813e-02, 5.735490e-03,
    }
    for _, ctype := range ctypes {
        s, err := adjusted(p, ctype)
        if err != nil {
            log.Fatal(err)
        }
        fmt.Println(s)
    }
}
```


{{out}}
<pre style="height:60ex;overflow:scroll;">
Benjamini-Hochberg
[ 0]  0.6126681081 0.8521710465 0.1987205200 0.1891595417 0.3217789286
[ 5]  0.9301450000 0.4870370000 0.9301450000 0.6049730556 0.6826752564
[10]  0.6482628947 0.7253722500 0.5280972727 0.8769925556 0.4705703448
[15]  0.9241867391 0.6049730556 0.7856107317 0.4887525806 0.1136717045
[20]  0.4991890625 0.8769925556 0.9991834000 0.3217789286 0.9301450000
[25]  0.2304957692 0.5832475000 0.0389954722 0.8521710465 0.1476842609
[30]  0.0168363750 0.0025629017 0.0351608437 0.0625018947 0.0036365888
[35]  0.0025629017 0.0294688286 0.0061660636 0.0389954722 0.0026889914
[40]  0.0004502862 0.0000125223 0.0788155476 0.0314261300 0.0048465270
[45]  0.0025629017 0.0048465270 0.0011017083 0.0725203250 0.0220595769

Benjamini-Yekutieli
[ 0]  1.0000000000 1.0000000000 0.8940844244 0.8510676197 1.0000000000
[ 5]  1.0000000000 1.0000000000 1.0000000000 1.0000000000 1.0000000000
[10]  1.0000000000 1.0000000000 1.0000000000 1.0000000000 1.0000000000
[15]  1.0000000000 1.0000000000 1.0000000000 1.0000000000 0.5114323399
[20]  1.0000000000 1.0000000000 1.0000000000 1.0000000000 1.0000000000
[25]  1.0000000000 1.0000000000 0.1754486368 1.0000000000 0.6644618149
[30]  0.0757503083 0.0115310209 0.1581958559 0.2812088585 0.0163617595
[35]  0.0115310209 0.1325863108 0.0277423864 0.1754486368 0.0120983246
[40]  0.0020259303 0.0000563403 0.3546073326 0.1413926119 0.0218055202
[45]  0.0115310209 0.0218055202 0.0049568120 0.3262838334 0.0992505663

Bonferroni
[ 0]  1.0000000000 1.0000000000 1.0000000000 1.0000000000 1.0000000000
[ 5]  1.0000000000 1.0000000000 1.0000000000 1.0000000000 1.0000000000
[10]  1.0000000000 1.0000000000 1.0000000000 1.0000000000 1.0000000000
[15]  1.0000000000 1.0000000000 1.0000000000 1.0000000000 1.0000000000
[20]  1.0000000000 1.0000000000 1.0000000000 1.0000000000 1.0000000000
[25]  1.0000000000 1.0000000000 0.7019185000 1.0000000000 1.0000000000
[30]  0.2020365000 0.0151667450 0.5625735000 1.0000000000 0.0290927100
[35]  0.0153774100 0.4125636000 0.0678267000 0.6803480000 0.0188229400
[40]  0.0009005725 0.0000125223 1.0000000000 0.4713919500 0.0439557650
[45]  0.0108891550 0.0484652700 0.0033051250 1.0000000000 0.2867745000

Hochberg
[ 0]  0.9991834000 0.9991834000 0.9991834000 0.9991834000 0.9991834000
[ 5]  0.9991834000 0.9991834000 0.9991834000 0.9991834000 0.9991834000
[10]  0.9991834000 0.9991834000 0.9991834000 0.9991834000 0.9991834000
[15]  0.9991834000 0.9991834000 0.9991834000 0.9991834000 0.9991834000
[20]  0.9991834000 0.9991834000 0.9991834000 0.9991834000 0.9991834000
[25]  0.9991834000 0.9991834000 0.4632662100 0.9991834000 0.9991834000
[30]  0.1575884700 0.0138396690 0.3938014500 0.7600230400 0.0250197306
[35]  0.0138396690 0.3052970640 0.0542613600 0.4626366400 0.0165641872
[40]  0.0008825610 0.0000125223 0.9930759000 0.3394022040 0.0369228426
[45]  0.0102358057 0.0397415214 0.0031729200 0.8992520300 0.2179486200

Holm
[ 0]  1.0000000000 1.0000000000 1.0000000000 1.0000000000 1.0000000000
[ 5]  1.0000000000 1.0000000000 1.0000000000 1.0000000000 1.0000000000
[10]  1.0000000000 1.0000000000 1.0000000000 1.0000000000 1.0000000000
[15]  1.0000000000 1.0000000000 1.0000000000 1.0000000000 1.0000000000
[20]  1.0000000000 1.0000000000 1.0000000000 1.0000000000 1.0000000000
[25]  1.0000000000 1.0000000000 0.4632662100 1.0000000000 1.0000000000
[30]  0.1575884700 0.0139534054 0.3938014500 0.7600230400 0.0250197306
[35]  0.0139534054 0.3052970640 0.0542613600 0.4626366400 0.0165641872
[40]  0.0008825610 0.0000125223 0.9930759000 0.3394022040 0.0369228426
[45]  0.0102358057 0.0397415214 0.0031729200 0.8992520300 0.2179486200

Hommel
[ 0]  0.9991834000 0.9991834000 0.9991834000 0.9987623800 0.9991834000
[ 5]  0.9991834000 0.9991834000 0.9991834000 0.9991834000 0.9991834000
[10]  0.9991834000 0.9991834000 0.9991834000 0.9991834000 0.9991834000
[15]  0.9991834000 0.9991834000 0.9991834000 0.9991834000 0.9595180000
[20]  0.9991834000 0.9991834000 0.9991834000 0.9991834000 0.9991834000
[25]  0.9991834000 0.9991834000 0.4351894700 0.9991834000 0.9766522500
[30]  0.1414255500 0.0130434007 0.3530936533 0.6887708800 0.0238560222
[35]  0.0132245726 0.2722919760 0.0542613600 0.4218157600 0.0158112696
[40]  0.0008825610 0.0000125223 0.8743649143 0.3016908480 0.0351646120
[45]  0.0095824564 0.0387722160 0.0031729200 0.8122276400 0.1950066600

Šidák
[ 0]  1.0000000000 1.0000000000 0.9946598274 0.9914285749 0.9999515274
[ 5]  1.0000000000 0.9999999688 1.0000000000 1.0000000000 1.0000000000
[10]  1.0000000000 1.0000000000 0.9999999995 1.0000000000 0.9999998801
[15]  1.0000000000 1.0000000000 1.0000000000 0.9999999855 0.9231179729
[20]  0.9999999956 1.0000000000 1.0000000000 0.9999317605 1.0000000000
[25]  0.9983109511 1.0000000000 0.5068253940 1.0000000000 0.9703301333
[30]  0.1832692440 0.0150545753 0.4320729669 0.6993672225 0.0286818157
[35]  0.0152621104 0.3391808707 0.0656206307 0.4959194266 0.0186503726
[40]  0.0009001752 0.0000125222 0.8142104886 0.3772612062 0.0430222116
[45]  0.0108312558 0.0473319661 0.0032997780 0.7705015898 0.2499384839

Sorry, do not know how to do 'Unknown' correction.
Perhaps you want one of these?:
  Benjamini-Hochberg
  Benjamini-Yekutieli
  Bonferroni
  Hochberg
  Holm
  Hommel
  Šidák

```



## Java

{{trans|D}}
{{works with|Java|8}}
''This work is based on R source code covered by the '''GPL''' license. It is thus a modified version, also covered by the GPL. See the [https://www.gnu.org/licenses/gpl-faq.html#GPLRequireSourcePostedPublic FAQ about GNU licenses]''.

```Java
import java.util.Arrays;
import java.util.Comparator;

public class PValueCorrection {
    private static int[] seqLen(int start, int end) {
        int[] result;
        if (start == end) {
            result = new int[end + 1];
            for (int i = 0; i < result.length; ++i) {
                result[i] = i + 1;
            }
        } else if (start < end) {
            result = new int[end - start + 1];
            for (int i = 0; i < result.length; ++i) {
                result[i] = start + i;
            }
        } else {
            result = new int[start - end + 1];
            for (int i = 0; i < result.length; ++i) {
                result[i] = start - i;
            }
        }
        return result;
    }

    private static int[] order(double[] array, boolean decreasing) {
        int size = array.length;
        int[] idx = new int[size];
        double[] baseArr = new double[size];
        for (int i = 0; i < size; ++i) {
            baseArr[i] = array[i];
            idx[i] = i;
        }

        Comparator<Integer> cmp;
        if (!decreasing) {
            cmp = Comparator.comparingDouble(a -> baseArr[a]);
        } else {
            cmp = (a, b) -> Double.compare(baseArr[b], baseArr[a]);
        }

        return Arrays.stream(idx)
            .boxed()
            .sorted(cmp)
            .mapToInt(a -> a)
            .toArray();
    }

    private static double[] cummin(double[] array) {
        if (array.length < 1) throw new IllegalArgumentException("cummin requires at least one element");
        double[] output = new double[array.length];
        double cumulativeMin = array[0];
        for (int i = 0; i < array.length; ++i) {
            if (array[i] < cumulativeMin) cumulativeMin = array[i];
            output[i] = cumulativeMin;
        }
        return output;
    }

    private static double[] cummax(double[] array) {
        if (array.length < 1) throw new IllegalArgumentException("cummax requires at least one element");
        double[] output = new double[array.length];
        double cumulativeMax = array[0];
        for (int i = 0; i < array.length; ++i) {
            if (array[i] > cumulativeMax) cumulativeMax = array[i];
            output[i] = cumulativeMax;
        }
        return output;
    }

    private static double[] pminx(double[] array, double x) {
        if (array.length < 1) throw new IllegalArgumentException("pmin requires at least one element");
        double[] result = new double[array.length];
        for (int i = 0; i < array.length; ++i) {
            if (array[i] < x) {
                result[i] = array[i];
            } else {
                result[i] = x;
            }
        }
        return result;
    }

    private static void doubleSay(double[] array) {
        System.out.printf("[ 1] %e", array[0]);
        for (int i = 1; i < array.length; ++i) {
            System.out.printf(" %.10f", array[i]);
            if ((i + 1) % 5 == 0) System.out.printf("\n[%2d]", i + 1);
        }
        System.out.println();
    }

    private static double[] intToDouble(int[] array) {
        double[] result = new double[array.length];
        for (int i = 0; i < array.length; i++) {
            result[i] = array[i];
        }
        return result;
    }

    private static double doubleArrayMin(double[] array) {
        if (array.length < 1) throw new IllegalArgumentException("pAdjust requires at least one element");
        return Arrays.stream(array).min().orElse(Double.NaN);
    }

    private static double[] pAdjust(double[] pvalues, String str) {
        int size = pvalues.length;
        if (size < 1) throw new IllegalArgumentException("pAdjust requires at least one element");
        int type;
        switch (str.toLowerCase()) {
            case "bh":
            case "fdr":
                type = 0;
                break;
            case "by":
                type = 1;
                break;
            case "bonferroni":
                type = 2;
                break;
            case "hochberg":
                type = 3;
                break;
            case "holm":
                type = 4;
                break;
            case "hommel":
                type = 5;
                break;
            default:
                throw new IllegalArgumentException(str + " doesn't match any accepted FDR types");
        }

        if (type == 2) {  // Bonferroni method
            double[] result = new double[size];
            for (int i = 0; i < size; ++i) {
                double b = pvalues[i] * size;
                if (b >= 1) {
                    result[i] = 1;
                } else if (0 <= b && b < 1) {
                    result[i] = b;
                } else {
                    throw new RuntimeException("" + b + " is outside [0, 1)");
                }
            }
            return result;
        } else if (type == 4) {  // Holm method
            int[] o = order(pvalues, false);
            double[] o2Double = intToDouble(o);
            double[] cummaxInput = new double[size];
            for (int i = 0; i < size; ++i) {
                cummaxInput[i] = (size - i) * pvalues[o[i]];
            }
            int[] ro = order(o2Double, false);
            double[] cummaxOutput = cummax(cummaxInput);
            double[] pmin = pminx(cummaxOutput, 1.0);
            double[] result = new double[size];
            for (int i = 0; i < size; ++i) {
                result[i] = pmin[ro[i]];
            }
            return result;
        } else if (type == 5) {
            int[] indices = seqLen(size, size);
            int[] o = order(pvalues, false);
            double[] p = new double[size];
            for (int i = 0; i < size; ++i) {
                p[i] = pvalues[o[i]];
            }
            double[] o2Double = intToDouble(o);
            int[] ro = order(o2Double, false);
            double[] q = new double[size];
            double[] pa = new double[size];
            double[] npi = new double[size];
            for (int i = 0; i < size; ++i) {
                npi[i] = p[i] * size / indices[i];
            }
            double min = doubleArrayMin(npi);
            Arrays.fill(q, min);
            Arrays.fill(pa, min);
            for (int j = size; j >= 2; --j) {
                int[] ij = seqLen(1, size - j + 1);
                for (int i = 0; i < size - j + 1; ++i) {
                    ij[i]--;
                }
                int i2Length = j - 1;
                int[] i2 = new int[i2Length];
                for (int i = 0; i < i2Length; ++i) {
                    i2[i] = size - j + 2 + i - 1;
                }
                double q1 = j * p[i2[0]] / 2.0;
                for (int i = 1; i < i2Length; ++i) {
                    double temp_q1 = p[i2[i]] * j / (2.0 + i);
                    if (temp_q1 < q1) q1 = temp_q1;
                }
                for (int i = 0; i < size - j + 1; ++i) {
                    q[ij[i]] = Math.min(p[ij[i]] * j, q1);
                }
                for (int i = 0; i < i2Length; ++i) {
                    q[i2[i]] = q[size - j];
                }
                for (int i = 0; i < size; ++i) {
                    if (pa[i] < q[i]) {
                        pa[i] = q[i];
                    }
                }
            }
            for (int i = 0; i < size; ++i) {
                q[i] = pa[ro[i]];
            }
            return q;
        }

        double[] ni = new double[size];
        int[] o = order(pvalues, true);
        double[] oDouble = intToDouble(o);
        for (int i = 0; i < size; ++i) {
            if (pvalues[i] < 0 || pvalues[i] > 1) {
                throw new RuntimeException("array[" + i + "] = " + pvalues[i] + " is outside [0, 1]");
            }
            ni[i] = (double) size / (size - i);
        }
        int[] ro = order(oDouble, false);
        double[] cumminInput = new double[size];
        if (type == 0) {  // BH method
            for (int i = 0; i < size; ++i) {
                cumminInput[i] = ni[i] * pvalues[o[i]];
            }
        } else if (type == 1) {  // BY method
            double q = 0;
            for (int i = 1; i < size + 1; ++i) {
                q += 1.0 / i;
            }
            for (int i = 0; i < size; ++i) {
                cumminInput[i] = q * ni[i] * pvalues[o[i]];
            }
        } else if (type == 3) {  // Hochberg method
            for (int i = 0; i < size; ++i) {
                cumminInput[i] = (i + 1) * pvalues[o[i]];
            }
        }
        double[] cumminArray = cummin(cumminInput);
        double[] pmin = pminx(cumminArray, 1.0);
        double[] result = new double[size];
        for (int i = 0; i < size; ++i) {
            result[i] = pmin[ro[i]];
        }
        return result;
    }

    public static void main(String[] args) {
        double[] pvalues = new double[]{
            4.533744e-01, 7.296024e-01, 9.936026e-02, 9.079658e-02, 1.801962e-01,
            8.752257e-01, 2.922222e-01, 9.115421e-01, 4.355806e-01, 5.324867e-01,
            4.926798e-01, 5.802978e-01, 3.485442e-01, 7.883130e-01, 2.729308e-01,
            8.502518e-01, 4.268138e-01, 6.442008e-01, 3.030266e-01, 5.001555e-02,
            3.194810e-01, 7.892933e-01, 9.991834e-01, 1.745691e-01, 9.037516e-01,
            1.198578e-01, 3.966083e-01, 1.403837e-02, 7.328671e-01, 6.793476e-02,
            4.040730e-03, 3.033349e-04, 1.125147e-02, 2.375072e-02, 5.818542e-04,
            3.075482e-04, 8.251272e-03, 1.356534e-03, 1.360696e-02, 3.764588e-04,
            1.801145e-05, 2.504456e-07, 3.310253e-02, 9.427839e-03, 8.791153e-04,
            2.177831e-04, 9.693054e-04, 6.610250e-05, 2.900813e-02, 5.735490e-03
        };

        double[][] correctAnswers = new double[][]{
            new double[]{  // Benjamini-Hochberg
                6.126681e-01, 8.521710e-01, 1.987205e-01, 1.891595e-01, 3.217789e-01,
                9.301450e-01, 4.870370e-01, 9.301450e-01, 6.049731e-01, 6.826753e-01,
                6.482629e-01, 7.253722e-01, 5.280973e-01, 8.769926e-01, 4.705703e-01,
                9.241867e-01, 6.049731e-01, 7.856107e-01, 4.887526e-01, 1.136717e-01,
                4.991891e-01, 8.769926e-01, 9.991834e-01, 3.217789e-01, 9.301450e-01,
                2.304958e-01, 5.832475e-01, 3.899547e-02, 8.521710e-01, 1.476843e-01,
                1.683638e-02, 2.562902e-03, 3.516084e-02, 6.250189e-02, 3.636589e-03,
                2.562902e-03, 2.946883e-02, 6.166064e-03, 3.899547e-02, 2.688991e-03,
                4.502862e-04, 1.252228e-05, 7.881555e-02, 3.142613e-02, 4.846527e-03,
                2.562902e-03, 4.846527e-03, 1.101708e-03, 7.252032e-02, 2.205958e-02
            },
            new double[]{  // Benjamini & Yekutieli
                1.000000e+00, 1.000000e+00, 8.940844e-01, 8.510676e-01, 1.000000e+00,
                1.000000e+00, 1.000000e+00, 1.000000e+00, 1.000000e+00, 1.000000e+00,
                1.000000e+00, 1.000000e+00, 1.000000e+00, 1.000000e+00, 1.000000e+00,
                1.000000e+00, 1.000000e+00, 1.000000e+00, 1.000000e+00, 5.114323e-01,
                1.000000e+00, 1.000000e+00, 1.000000e+00, 1.000000e+00, 1.000000e+00,
                1.000000e+00, 1.000000e+00, 1.754486e-01, 1.000000e+00, 6.644618e-01,
                7.575031e-02, 1.153102e-02, 1.581959e-01, 2.812089e-01, 1.636176e-02,
                1.153102e-02, 1.325863e-01, 2.774239e-02, 1.754486e-01, 1.209832e-02,
                2.025930e-03, 5.634031e-05, 3.546073e-01, 1.413926e-01, 2.180552e-02,
                1.153102e-02, 2.180552e-02, 4.956812e-03, 3.262838e-01, 9.925057e-02
            },
            new double[]{  // Bonferroni
                1.000000e+00, 1.000000e+00, 1.000000e+00, 1.000000e+00, 1.000000e+00,
                1.000000e+00, 1.000000e+00, 1.000000e+00, 1.000000e+00, 1.000000e+00,
                1.000000e+00, 1.000000e+00, 1.000000e+00, 1.000000e+00, 1.000000e+00,
                1.000000e+00, 1.000000e+00, 1.000000e+00, 1.000000e+00, 1.000000e+00,
                1.000000e+00, 1.000000e+00, 1.000000e+00, 1.000000e+00, 1.000000e+00,
                1.000000e+00, 1.000000e+00, 7.019185e-01, 1.000000e+00, 1.000000e+00,
                2.020365e-01, 1.516674e-02, 5.625735e-01, 1.000000e+00, 2.909271e-02,
                1.537741e-02, 4.125636e-01, 6.782670e-02, 6.803480e-01, 1.882294e-02,
                9.005725e-04, 1.252228e-05, 1.000000e+00, 4.713920e-01, 4.395577e-02,
                1.088915e-02, 4.846527e-02, 3.305125e-03, 1.000000e+00, 2.867745e-01
            },
            new double[]{  // Hochberg
                9.991834e-01, 9.991834e-01, 9.991834e-01, 9.991834e-01, 9.991834e-01,
                9.991834e-01, 9.991834e-01, 9.991834e-01, 9.991834e-01, 9.991834e-01,
                9.991834e-01, 9.991834e-01, 9.991834e-01, 9.991834e-01, 9.991834e-01,
                9.991834e-01, 9.991834e-01, 9.991834e-01, 9.991834e-01, 9.991834e-01,
                9.991834e-01, 9.991834e-01, 9.991834e-01, 9.991834e-01, 9.991834e-01,
                9.991834e-01, 9.991834e-01, 4.632662e-01, 9.991834e-01, 9.991834e-01,
                1.575885e-01, 1.383967e-02, 3.938014e-01, 7.600230e-01, 2.501973e-02,
                1.383967e-02, 3.052971e-01, 5.426136e-02, 4.626366e-01, 1.656419e-02,
                8.825610e-04, 1.252228e-05, 9.930759e-01, 3.394022e-01, 3.692284e-02,
                1.023581e-02, 3.974152e-02, 3.172920e-03, 8.992520e-01, 2.179486e-01
            },
            new double[]{  // Holm
                1.000000e+00, 1.000000e+00, 1.000000e+00, 1.000000e+00, 1.000000e+00,
                1.000000e+00, 1.000000e+00, 1.000000e+00, 1.000000e+00, 1.000000e+00,
                1.000000e+00, 1.000000e+00, 1.000000e+00, 1.000000e+00, 1.000000e+00,
                1.000000e+00, 1.000000e+00, 1.000000e+00, 1.000000e+00, 1.000000e+00,
                1.000000e+00, 1.000000e+00, 1.000000e+00, 1.000000e+00, 1.000000e+00,
                1.000000e+00, 1.000000e+00, 4.632662e-01, 1.000000e+00, 1.000000e+00,
                1.575885e-01, 1.395341e-02, 3.938014e-01, 7.600230e-01, 2.501973e-02,
                1.395341e-02, 3.052971e-01, 5.426136e-02, 4.626366e-01, 1.656419e-02,
                8.825610e-04, 1.252228e-05, 9.930759e-01, 3.394022e-01, 3.692284e-02,
                1.023581e-02, 3.974152e-02, 3.172920e-03, 8.992520e-01, 2.179486e-01
            },
            new double[]{  // Hommel
                9.991834e-01, 9.991834e-01, 9.991834e-01, 9.987624e-01, 9.991834e-01,
                9.991834e-01, 9.991834e-01, 9.991834e-01, 9.991834e-01, 9.991834e-01,
                9.991834e-01, 9.991834e-01, 9.991834e-01, 9.991834e-01, 9.991834e-01,
                9.991834e-01, 9.991834e-01, 9.991834e-01, 9.991834e-01, 9.595180e-01,
                9.991834e-01, 9.991834e-01, 9.991834e-01, 9.991834e-01, 9.991834e-01,
                9.991834e-01, 9.991834e-01, 4.351895e-01, 9.991834e-01, 9.766522e-01,
                1.414256e-01, 1.304340e-02, 3.530937e-01, 6.887709e-01, 2.385602e-02,
                1.322457e-02, 2.722920e-01, 5.426136e-02, 4.218158e-01, 1.581127e-02,
                8.825610e-04, 1.252228e-05, 8.743649e-01, 3.016908e-01, 3.516461e-02,
                9.582456e-03, 3.877222e-02, 3.172920e-03, 8.122276e-01, 1.950067e-01
            }
        };

        String[] types = new String[]{"bh", "by", "bonferroni", "hochberg", "holm", "hommel"};
        for (int type = 0; type < types.length; ++type) {
            double[] q = pAdjust(pvalues, types[type]);
            double error = 0.0;
            for (int i = 0; i < pvalues.length; ++i) {
                error += Math.abs(q[i] - correctAnswers[type][i]);
            }
            doubleSay(q);
            System.out.printf("\ntype %d = '%s' has a cumulative error of %g\n", type, types[type], error);
        }
    }
}
```

{{out}}

```txt
[ 1] 6.126681e-01 0.8521710465 0.1987205200 0.1891595417 0.3217789286
[ 5] 0.9301450000 0.4870370000 0.9301450000 0.6049730556 0.6826752564
[10] 0.6482628947 0.7253722500 0.5280972727 0.8769925556 0.4705703448
[15] 0.9241867391 0.6049730556 0.7856107317 0.4887525806 0.1136717045
[20] 0.4991890625 0.8769925556 0.9991834000 0.3217789286 0.9301450000
[25] 0.2304957692 0.5832475000 0.0389954722 0.8521710465 0.1476842609
[30] 0.0168363750 0.0025629017 0.0351608438 0.0625018947 0.0036365888
[35] 0.0025629017 0.0294688286 0.0061660636 0.0389954722 0.0026889914
[40] 0.0004502862 0.0000125223 0.0788155476 0.0314261300 0.0048465270
[45] 0.0025629017 0.0048465270 0.0011017083 0.0725203250 0.0220595769
[50]

type 0 = 'bh' has a cumulative error of 8.03053e-07
[ 1] 1.000000e+00 1.0000000000 0.8940844244 0.8510676197 1.0000000000
[ 5] 1.0000000000 1.0000000000 1.0000000000 1.0000000000 1.0000000000
[10] 1.0000000000 1.0000000000 1.0000000000 1.0000000000 1.0000000000
[15] 1.0000000000 1.0000000000 1.0000000000 1.0000000000 0.5114323399
[20] 1.0000000000 1.0000000000 1.0000000000 1.0000000000 1.0000000000
[25] 1.0000000000 1.0000000000 0.1754486368 1.0000000000 0.6644618149
[30] 0.0757503083 0.0115310209 0.1581958559 0.2812088585 0.0163617595
[35] 0.0115310209 0.1325863108 0.0277423864 0.1754486368 0.0120983246
[40] 0.0020259303 0.0000563403 0.3546073326 0.1413926119 0.0218055202
[45] 0.0115310209 0.0218055202 0.0049568120 0.3262838334 0.0992505663
[50]

type 1 = 'by' has a cumulative error of 3.64072e-07
[ 1] 1.000000e+00 1.0000000000 1.0000000000 1.0000000000 1.0000000000
[ 5] 1.0000000000 1.0000000000 1.0000000000 1.0000000000 1.0000000000
[10] 1.0000000000 1.0000000000 1.0000000000 1.0000000000 1.0000000000
[15] 1.0000000000 1.0000000000 1.0000000000 1.0000000000 1.0000000000
[20] 1.0000000000 1.0000000000 1.0000000000 1.0000000000 1.0000000000
[25] 1.0000000000 1.0000000000 0.7019185000 1.0000000000 1.0000000000
[30] 0.2020365000 0.0151667450 0.5625735000 1.0000000000 0.0290927100
[35] 0.0153774100 0.4125636000 0.0678267000 0.6803480000 0.0188229400
[40] 0.0009005725 0.0000125223 1.0000000000 0.4713919500 0.0439557650
[45] 0.0108891550 0.0484652700 0.0033051250 1.0000000000 0.2867745000
[50]

type 2 = 'bonferroni' has a cumulative error of 6.50000e-08
[ 1] 9.991834e-01 0.9991834000 0.9991834000 0.9991834000 0.9991834000
[ 5] 0.9991834000 0.9991834000 0.9991834000 0.9991834000 0.9991834000
[10] 0.9991834000 0.9991834000 0.9991834000 0.9991834000 0.9991834000
[15] 0.9991834000 0.9991834000 0.9991834000 0.9991834000 0.9991834000
[20] 0.9991834000 0.9991834000 0.9991834000 0.9991834000 0.9991834000
[25] 0.9991834000 0.9991834000 0.4632662100 0.9991834000 0.9991834000
[30] 0.1575884700 0.0138396690 0.3938014500 0.7600230400 0.0250197306
[35] 0.0138396690 0.3052970640 0.0542613600 0.4626366400 0.0165641872
[40] 0.0008825610 0.0000125223 0.9930759000 0.3394022040 0.0369228426
[45] 0.0102358057 0.0397415214 0.0031729200 0.8992520300 0.2179486200
[50]

type 3 = 'hochberg' has a cumulative error of 2.73750e-07
[ 1] 1.000000e+00 1.0000000000 1.0000000000 1.0000000000 1.0000000000
[ 5] 1.0000000000 1.0000000000 1.0000000000 1.0000000000 1.0000000000
[10] 1.0000000000 1.0000000000 1.0000000000 1.0000000000 1.0000000000
[15] 1.0000000000 1.0000000000 1.0000000000 1.0000000000 1.0000000000
[20] 1.0000000000 1.0000000000 1.0000000000 1.0000000000 1.0000000000
[25] 1.0000000000 1.0000000000 0.4632662100 1.0000000000 1.0000000000
[30] 0.1575884700 0.0139534054 0.3938014500 0.7600230400 0.0250197306
[35] 0.0139534054 0.3052970640 0.0542613600 0.4626366400 0.0165641872
[40] 0.0008825610 0.0000125223 0.9930759000 0.3394022040 0.0369228426
[45] 0.0102358057 0.0397415214 0.0031729200 0.8992520300 0.2179486200
[50]

type 4 = 'holm' has a cumulative error of 2.80950e-07
[ 1] 9.991834e-01 0.9991834000 0.9991834000 0.9987623800 0.9991834000
[ 5] 0.9991834000 0.9991834000 0.9991834000 0.9991834000 0.9991834000
[10] 0.9991834000 0.9991834000 0.9991834000 0.9991834000 0.9991834000
[15] 0.9991834000 0.9991834000 0.9991834000 0.9991834000 0.9595180000
[20] 0.9991834000 0.9991834000 0.9991834000 0.9991834000 0.9991834000
[25] 0.9991834000 0.9991834000 0.4351894700 0.9991834000 0.9766522500
[30] 0.1414255500 0.0130434007 0.3530936533 0.6887708800 0.0238560222
[35] 0.0132245726 0.2722919760 0.0542613600 0.4218157600 0.0158112696
[40] 0.0008825610 0.0000125223 0.8743649143 0.3016908480 0.0351646120
[45] 0.0095824564 0.0387722160 0.0031729200 0.8122276400 0.1950066600
[50]

type 5 = 'hommel' has a cumulative error of 4.35302e-07
```



## Julia

{{works with|Julia|0.6}}


```julia
using MultipleTesting
using IterTools

p = [4.533744e-01, 7.296024e-01, 9.936026e-02, 9.079658e-02, 1.801962e-01,
     8.752257e-01, 2.922222e-01, 9.115421e-01, 4.355806e-01, 5.324867e-01,
     4.926798e-01, 5.802978e-01, 3.485442e-01, 7.883130e-01, 2.729308e-01,
     8.502518e-01, 4.268138e-01, 6.442008e-01, 3.030266e-01, 5.001555e-02,
     3.194810e-01, 7.892933e-01, 9.991834e-01, 1.745691e-01, 9.037516e-01,
     1.198578e-01, 3.966083e-01, 1.403837e-02, 7.328671e-01, 6.793476e-02,
     4.040730e-03, 3.033349e-04, 1.125147e-02, 2.375072e-02, 5.818542e-04,
     3.075482e-04, 8.251272e-03, 1.356534e-03, 1.360696e-02, 3.764588e-04,
     1.801145e-05, 2.504456e-07, 3.310253e-02, 9.427839e-03, 8.791153e-04,
     2.177831e-04, 9.693054e-04, 6.610250e-05, 2.900813e-02, 5.735490e-03]

function printpvalues(v)
    for chunk in partition(v, 10)
        println(join((@sprintf("%4.7f", p) for p in chunk), ", "))
    end
end

println("Original p-values:")
printpvalues(p)
for corr in (Bonferroni(), BenjaminiHochberg(), BenjaminiYekutieli(), Holm(), Hochberg(), Hommel())
    println("\n", corr)
    printpvalues(adjust(p, corr))
end
```


{{out}}

```txt
Original p-values:
0.4533744, 0.7296024, 0.0993603, 0.0907966, 0.1801962, 0.8752257, 0.2922222, 0.9115421, 0.4355806, 0.5324867
0.4926798, 0.5802978, 0.3485442, 0.7883130, 0.2729308, 0.8502518, 0.4268138, 0.6442008, 0.3030266, 0.0500155
0.3194810, 0.7892933, 0.9991834, 0.1745691, 0.9037516, 0.1198578, 0.3966083, 0.0140384, 0.7328671, 0.0679348
0.0040407, 0.0003033, 0.0112515, 0.0237507, 0.0005819, 0.0003075, 0.0082513, 0.0013565, 0.0136070, 0.0003765
0.0000180, 0.0000003, 0.0331025, 0.0094278, 0.0008791, 0.0002178, 0.0009693, 0.0000661, 0.0290081, 0.0057355

MultipleTesting.Bonferroni()
1.0000000, 1.0000000, 1.0000000, 1.0000000, 1.0000000, 1.0000000, 1.0000000, 1.0000000, 1.0000000, 1.0000000
1.0000000, 1.0000000, 1.0000000, 1.0000000, 1.0000000, 1.0000000, 1.0000000, 1.0000000, 1.0000000, 1.0000000
1.0000000, 1.0000000, 1.0000000, 1.0000000, 1.0000000, 1.0000000, 1.0000000, 0.7019185, 1.0000000, 1.0000000
0.2020365, 0.0151667, 0.5625735, 1.0000000, 0.0290927, 0.0153774, 0.4125636, 0.0678267, 0.6803480, 0.0188229
0.0009006, 0.0000125, 1.0000000, 0.4713920, 0.0439558, 0.0108892, 0.0484653, 0.0033051, 1.0000000, 0.2867745

MultipleTesting.BenjaminiHochberg()
0.6126681, 0.8521710, 0.1987205, 0.1891595, 0.3217789, 0.9301450, 0.4870370, 0.9301450, 0.6049731, 0.6826753
0.6482629, 0.7253722, 0.5280973, 0.8769926, 0.4705703, 0.9241867, 0.6049731, 0.7856107, 0.4887526, 0.1136717
0.4991891, 0.8769926, 0.9991834, 0.3217789, 0.9301450, 0.2304958, 0.5832475, 0.0389955, 0.8521710, 0.1476843
0.0168364, 0.0025629, 0.0351608, 0.0625019, 0.0036366, 0.0025629, 0.0294688, 0.0061661, 0.0389955, 0.0026890
0.0004503, 0.0000125, 0.0788155, 0.0314261, 0.0048465, 0.0025629, 0.0048465, 0.0011017, 0.0725203, 0.0220596

MultipleTesting.BenjaminiYekutieli()
1.0000000, 1.0000000, 0.8940844, 0.8510676, 1.0000000, 1.0000000, 1.0000000, 1.0000000, 1.0000000, 1.0000000
1.0000000, 1.0000000, 1.0000000, 1.0000000, 1.0000000, 1.0000000, 1.0000000, 1.0000000, 1.0000000, 0.5114323
1.0000000, 1.0000000, 1.0000000, 1.0000000, 1.0000000, 1.0000000, 1.0000000, 0.1754486, 1.0000000, 0.6644618
0.0757503, 0.0115310, 0.1581959, 0.2812089, 0.0163618, 0.0115310, 0.1325863, 0.0277424, 0.1754486, 0.0120983
0.0020259, 0.0000563, 0.3546073, 0.1413926, 0.0218055, 0.0115310, 0.0218055, 0.0049568, 0.3262838, 0.0992506

MultipleTesting.Holm()
1.0000000, 1.0000000, 1.0000000, 1.0000000, 1.0000000, 1.0000000, 1.0000000, 1.0000000, 1.0000000, 1.0000000
1.0000000, 1.0000000, 1.0000000, 1.0000000, 1.0000000, 1.0000000, 1.0000000, 1.0000000, 1.0000000, 1.0000000
1.0000000, 1.0000000, 1.0000000, 1.0000000, 1.0000000, 1.0000000, 1.0000000, 0.4632662, 1.0000000, 1.0000000
0.1575885, 0.0139534, 0.3938014, 0.7600230, 0.0250197, 0.0139534, 0.3052971, 0.0542614, 0.4626366, 0.0165642
0.0008826, 0.0000125, 0.9930759, 0.3394022, 0.0369228, 0.0102358, 0.0397415, 0.0031729, 0.8992520, 0.2179486

MultipleTesting.Hochberg()
0.9991834, 0.9991834, 0.9991834, 0.9991834, 0.9991834, 0.9991834, 0.9991834, 0.9991834, 0.9991834, 0.9991834
0.9991834, 0.9991834, 0.9991834, 0.9991834, 0.9991834, 0.9991834, 0.9991834, 0.9991834, 0.9991834, 0.9991834
0.9991834, 0.9991834, 0.9991834, 0.9991834, 0.9991834, 0.9991834, 0.9991834, 0.4632662, 0.9991834, 0.9991834
0.1575885, 0.0138397, 0.3938014, 0.7600230, 0.0250197, 0.0138397, 0.3052971, 0.0542614, 0.4626366, 0.0165642
0.0008826, 0.0000125, 0.9930759, 0.3394022, 0.0369228, 0.0102358, 0.0397415, 0.0031729, 0.8992520, 0.2179486

MultipleTesting.Hommel()
0.9991834, 0.9991834, 0.9991834, 0.9987624, 0.9991834, 0.9991834, 0.9991834, 0.9991834, 0.9991834, 0.9991834
0.9991834, 0.9991834, 0.9991834, 0.9991834, 0.9991834, 0.9991834, 0.9991834, 0.9991834, 0.9991834, 0.9595180
0.9991834, 0.9991834, 0.9991834, 0.9991834, 0.9991834, 0.9991834, 0.9991834, 0.4351895, 0.9991834, 0.9766522
0.1414256, 0.0130434, 0.3530937, 0.6887709, 0.0238560, 0.0132246, 0.2722920, 0.0542614, 0.4218158, 0.0158113
0.0008826, 0.0000123, 0.8743649, 0.3016908, 0.0351646, 0.0095825, 0.0387722, 0.0031729, 0.8122276, 0.1950067
```



## Kotlin


### Version 1

{{trans|C}}
''This work is based on R source code covered by the '''GPL''' license. It is thus a modified version, also covered by the GPL. See the [https://www.gnu.org/licenses/gpl-faq.html#GPLRequireSourcePostedPublic FAQ about GNU licenses]''.


```scala
// version 1.1.51

import java.util.Arrays

typealias IAE = IllegalArgumentException

fun seqLen(start: Int, end: Int) =
    when {
        start == end -> IntArray(end + 1) { it + 1 }
        start < end  -> IntArray(end - start + 1) { start + it }
        else         -> IntArray(start - end + 1) { start - it }
    }

var baseArr: DoubleArray? = null

fun compareIncrease(a: Int, b: Int): Int = baseArr!![b].compareTo(baseArr!![a])

fun compareDecrease(a: Int, b: Int): Int = baseArr!![a].compareTo(baseArr!![b])

fun order(array: DoubleArray, decreasing: Boolean): IntArray {
    val size = array.size
    var idx = IntArray(size) { it }
    baseArr = array.copyOf()
    if (!decreasing) {
        idx = Arrays.stream(idx)
                    .boxed()
                    .sorted { a, b -> compareDecrease(a, b) }
                    .mapToInt { it }
                    .toArray()
    }
    else {
        idx = Arrays.stream(idx)
                    .boxed()
                    .sorted { a, b -> compareIncrease(a, b) }
                    .mapToInt { it }
                    .toArray()
    }
    baseArr = null
    return idx
}

fun cummin(array: DoubleArray): DoubleArray {
    val size = array.size
    if (size < 1) throw IAE("cummin requires at least one element")
    val output = DoubleArray(size)
    var cumulativeMin = array[0]
    for (i in 0 until size) {
        if (array[i] < cumulativeMin) cumulativeMin = array[i]
        output[i] = cumulativeMin
    }
    return output
}

fun cummax(array: DoubleArray): DoubleArray {
    val size = array.size
    if (size < 1) throw IAE("cummax requires at least one element")
    val output = DoubleArray(size)
    var cumulativeMax = array[0]
    for (i in 0 until size) {
        if (array[i] > cumulativeMax) cumulativeMax = array[i]
        output[i] = cumulativeMax
    }
    return output
}

fun pminx(array: DoubleArray, x: Double): DoubleArray {
    val size = array.size
    if (size < 1) throw IAE("pmin requires at least one element")
    return DoubleArray(size) { if (array[it] < x) array[it] else x }
}

fun doubleSay(array: DoubleArray) {
    print("[ 1] %e".format(array[0]))
    for (i in 1 until array.size) {
        print(" %.10f".format(array[i]))
        if ((i + 1) % 5 == 0) print("\n[%2d]".format(i + 1))
    }
    println()
}

fun intToDouble(array: IntArray) = DoubleArray(array.size) { array[it].toDouble() }

fun doubleArrayMin(array: DoubleArray) =
    if (array.size < 1) throw IAE("pAdjust requires at least one element")
    else array.min()!!

fun pAdjust(pvalues: DoubleArray, str: String): DoubleArray {
    val size = pvalues.size
    if (size < 1) throw IAE("pAdjust requires at least one element")
    val type = when(str.toLowerCase()) {
        "bh", "fdr"  -> 0
        "by"         -> 1
        "bonferroni" -> 2
        "hochberg"   -> 3
        "holm"       -> 4
        "hommel"     -> 5
        else         -> throw IAE("'$str' doesn't match any accepted FDR types")
    }
    if (type == 2) {  // Bonferroni method
        return DoubleArray(size) {
            val b = pvalues[it] * size
            when {
                b >= 1           -> 1.0
                0 <= b && b < 1  -> b
                else -> throw RuntimeException("$b is outside [0, 1)")
            }
        }
    }
    else if (type == 4) {  // Holm method
        val o = order(pvalues, false)
        val o2Double = intToDouble(o)
        val cummaxInput = DoubleArray(size) { (size - it) * pvalues[o[it]] }
        val ro = order(o2Double, false)
        val cummaxOutput = cummax(cummaxInput)
        val pmin = pminx(cummaxOutput, 1.0)
        return DoubleArray(size) { pmin[ro[it]] }
    }
    else if (type == 5) { // Hommel method
        val indices = seqLen(size, size)
        val o = order(pvalues, false)
        val p = DoubleArray(size) { pvalues[o[it]] }
        val o2Double = intToDouble(o)
        val ro = order(o2Double, false)
        val q = DoubleArray(size)
        val pa = DoubleArray(size)
        val npi = DoubleArray(size) { p[it] * size / indices[it] }
        val min = doubleArrayMin(npi)
        q.fill(min)
        pa.fill(min)
        for (j in size - 1 downTo 2) {
            val ij = seqLen(1, size - j + 1)
            for (i in 0 until size - j + 1) ij[i]--
            val i2Length = j - 1
            val i2 = IntArray(i2Length) { size - j + 2 + it - 1 }
            val pi2Length = i2Length
            var q1 = j * p[i2[0]] / 2.0
            for (i in 1 until pi2Length) {
                val temp_q1 = p[i2[i]] * j / (2.0 + i)
                if(temp_q1 < q1) q1 = temp_q1
            }
            for (i in 0 until size - j + 1) {
                q[ij[i]] = minOf(p[ij[i]] * j, q1)
            }
            for (i in 0 until i2Length) q[i2[i]] = q[size - j]
            for (i in 0 until size) if (pa[i] < q[i]) pa[i] = q[i]
        }
        for (index in 0 until size) q[index] = pa[ro[index]]
        return q
    }
    val ni = DoubleArray(size)
    val o = order(pvalues, true)
    val oDouble = intToDouble(o)
    for (index in 0 until size) {
        if (pvalues[index] !in 0.0 .. 1.0) {
            throw RuntimeException("array[$index] = ${pvalues[index]} is outside [0, 1]")
        }
        ni[index] = size.toDouble() / (size - index)
    }
    val ro = order(oDouble, false)
    val cumminInput = DoubleArray(size)
    if (type == 0) {  // BH method
        for (index in 0 until size) {
            cumminInput[index] = ni[index] * pvalues[o[index]]
        }
    }
    else if (type == 1) {  // BY method
        var q = 0.0
        for (index in 1 until size + 1) q += 1.0 / index
        for (index in 0 until size) {
            cumminInput[index] = q * ni[index] * pvalues[o[index]]
        }
    }
    else if (type == 3) {  // Hochberg method
        for (index in 0 until size) {
            cumminInput[index] = (index + 1) * pvalues[o[index]]
        }
    }
    val cumminArray = cummin(cumminInput)
    val pmin = pminx(cumminArray, 1.0)
    return DoubleArray(size) { pmin[ro[it]] }
}

fun main(args: Array<String>) {
    val pvalues = doubleArrayOf(
        4.533744e-01, 7.296024e-01, 9.936026e-02, 9.079658e-02, 1.801962e-01,
        8.752257e-01, 2.922222e-01, 9.115421e-01, 4.355806e-01, 5.324867e-01,
        4.926798e-01, 5.802978e-01, 3.485442e-01, 7.883130e-01, 2.729308e-01,
        8.502518e-01, 4.268138e-01, 6.442008e-01, 3.030266e-01, 5.001555e-02,
        3.194810e-01, 7.892933e-01, 9.991834e-01, 1.745691e-01, 9.037516e-01,
        1.198578e-01, 3.966083e-01, 1.403837e-02, 7.328671e-01, 6.793476e-02,
        4.040730e-03, 3.033349e-04, 1.125147e-02, 2.375072e-02, 5.818542e-04,
        3.075482e-04, 8.251272e-03, 1.356534e-03, 1.360696e-02, 3.764588e-04,
        1.801145e-05, 2.504456e-07, 3.310253e-02, 9.427839e-03, 8.791153e-04,
        2.177831e-04, 9.693054e-04, 6.610250e-05, 2.900813e-02, 5.735490e-03
    )

    val correctAnswers = listOf(
        doubleArrayOf(  // Benjamini-Hochberg
            6.126681e-01, 8.521710e-01, 1.987205e-01, 1.891595e-01, 3.217789e-01,
            9.301450e-01, 4.870370e-01, 9.301450e-01, 6.049731e-01, 6.826753e-01,
            6.482629e-01, 7.253722e-01, 5.280973e-01, 8.769926e-01, 4.705703e-01,
            9.241867e-01, 6.049731e-01, 7.856107e-01, 4.887526e-01, 1.136717e-01,
            4.991891e-01, 8.769926e-01, 9.991834e-01, 3.217789e-01, 9.301450e-01,
            2.304958e-01, 5.832475e-01, 3.899547e-02, 8.521710e-01, 1.476843e-01,
            1.683638e-02, 2.562902e-03, 3.516084e-02, 6.250189e-02, 3.636589e-03,
            2.562902e-03, 2.946883e-02, 6.166064e-03, 3.899547e-02, 2.688991e-03,
            4.502862e-04, 1.252228e-05, 7.881555e-02, 3.142613e-02, 4.846527e-03,
            2.562902e-03, 4.846527e-03, 1.101708e-03, 7.252032e-02, 2.205958e-02
        ),
        doubleArrayOf(  // Benjamini & Yekutieli
            1.000000e+00, 1.000000e+00, 8.940844e-01, 8.510676e-01, 1.000000e+00,
            1.000000e+00, 1.000000e+00, 1.000000e+00, 1.000000e+00, 1.000000e+00,
            1.000000e+00, 1.000000e+00, 1.000000e+00, 1.000000e+00, 1.000000e+00,
            1.000000e+00, 1.000000e+00, 1.000000e+00, 1.000000e+00, 5.114323e-01,
            1.000000e+00, 1.000000e+00, 1.000000e+00, 1.000000e+00, 1.000000e+00,
            1.000000e+00, 1.000000e+00, 1.754486e-01, 1.000000e+00, 6.644618e-01,
            7.575031e-02, 1.153102e-02, 1.581959e-01, 2.812089e-01, 1.636176e-02,
            1.153102e-02, 1.325863e-01, 2.774239e-02, 1.754486e-01, 1.209832e-02,
            2.025930e-03, 5.634031e-05, 3.546073e-01, 1.413926e-01, 2.180552e-02,
            1.153102e-02, 2.180552e-02, 4.956812e-03, 3.262838e-01, 9.925057e-02
        ),
        doubleArrayOf(  // Bonferroni
            1.000000e+00, 1.000000e+00, 1.000000e+00, 1.000000e+00, 1.000000e+00,
            1.000000e+00, 1.000000e+00, 1.000000e+00, 1.000000e+00, 1.000000e+00,
            1.000000e+00, 1.000000e+00, 1.000000e+00, 1.000000e+00, 1.000000e+00,
            1.000000e+00, 1.000000e+00, 1.000000e+00, 1.000000e+00, 1.000000e+00,
            1.000000e+00, 1.000000e+00, 1.000000e+00, 1.000000e+00, 1.000000e+00,
            1.000000e+00, 1.000000e+00, 7.019185e-01, 1.000000e+00, 1.000000e+00,
            2.020365e-01, 1.516674e-02, 5.625735e-01, 1.000000e+00, 2.909271e-02,
            1.537741e-02, 4.125636e-01, 6.782670e-02, 6.803480e-01, 1.882294e-02,
            9.005725e-04, 1.252228e-05, 1.000000e+00, 4.713920e-01, 4.395577e-02,
            1.088915e-02, 4.846527e-02, 3.305125e-03, 1.000000e+00, 2.867745e-01
        ),
        doubleArrayOf(  // Hochberg
            9.991834e-01, 9.991834e-01, 9.991834e-01, 9.991834e-01, 9.991834e-01,
            9.991834e-01, 9.991834e-01, 9.991834e-01, 9.991834e-01, 9.991834e-01,
            9.991834e-01, 9.991834e-01, 9.991834e-01, 9.991834e-01, 9.991834e-01,
            9.991834e-01, 9.991834e-01, 9.991834e-01, 9.991834e-01, 9.991834e-01,
            9.991834e-01, 9.991834e-01, 9.991834e-01, 9.991834e-01, 9.991834e-01,
            9.991834e-01, 9.991834e-01, 4.632662e-01, 9.991834e-01, 9.991834e-01,
            1.575885e-01, 1.383967e-02, 3.938014e-01, 7.600230e-01, 2.501973e-02,
            1.383967e-02, 3.052971e-01, 5.426136e-02, 4.626366e-01, 1.656419e-02,
            8.825610e-04, 1.252228e-05, 9.930759e-01, 3.394022e-01, 3.692284e-02,
            1.023581e-02, 3.974152e-02, 3.172920e-03, 8.992520e-01, 2.179486e-01
        ),
        doubleArrayOf(  // Holm
            1.000000e+00, 1.000000e+00, 1.000000e+00, 1.000000e+00, 1.000000e+00,
            1.000000e+00, 1.000000e+00, 1.000000e+00, 1.000000e+00, 1.000000e+00,
            1.000000e+00, 1.000000e+00, 1.000000e+00, 1.000000e+00, 1.000000e+00,
            1.000000e+00, 1.000000e+00, 1.000000e+00, 1.000000e+00, 1.000000e+00,
            1.000000e+00, 1.000000e+00, 1.000000e+00, 1.000000e+00, 1.000000e+00,
            1.000000e+00, 1.000000e+00, 4.632662e-01, 1.000000e+00, 1.000000e+00,
            1.575885e-01, 1.395341e-02, 3.938014e-01, 7.600230e-01, 2.501973e-02,
            1.395341e-02, 3.052971e-01, 5.426136e-02, 4.626366e-01, 1.656419e-02,
            8.825610e-04, 1.252228e-05, 9.930759e-01, 3.394022e-01, 3.692284e-02,
            1.023581e-02, 3.974152e-02, 3.172920e-03, 8.992520e-01, 2.179486e-01
        ),
        doubleArrayOf(  // Hommel
            9.991834e-01, 9.991834e-01, 9.991834e-01, 9.987624e-01, 9.991834e-01,
            9.991834e-01, 9.991834e-01, 9.991834e-01, 9.991834e-01, 9.991834e-01,
            9.991834e-01, 9.991834e-01, 9.991834e-01, 9.991834e-01, 9.991834e-01,
            9.991834e-01, 9.991834e-01, 9.991834e-01, 9.991834e-01, 9.595180e-01,
            9.991834e-01, 9.991834e-01, 9.991834e-01, 9.991834e-01, 9.991834e-01,
            9.991834e-01, 9.991834e-01, 4.351895e-01, 9.991834e-01, 9.766522e-01,
            1.414256e-01, 1.304340e-02, 3.530937e-01, 6.887709e-01, 2.385602e-02,
            1.322457e-02, 2.722920e-01, 5.426136e-02, 4.218158e-01, 1.581127e-02,
            8.825610e-04, 1.252228e-05, 8.743649e-01, 3.016908e-01, 3.516461e-02,
            9.582456e-03, 3.877222e-02, 3.172920e-03, 8.122276e-01, 1.950067e-01
        )
    )
    val types = listOf("bh", "by", "bonferroni", "hochberg", "holm", "hommel")
    val f = "\ntype %d = '%s' has cumulative error of %g"
    for (type in 0 until types.size) {
        val q = pAdjust(pvalues, types[type])
        var error = 0.0
        for (i in 0 until pvalues.size) {
            error += Math.abs(q[i] - correctAnswers[type][i])
        }
        doubleSay(q)
        println(f.format(type, types[type], error))
    }
}
```


{{out}}

```txt

[ 1] 6.126681e-01 0.8521710465 0.1987205200 0.1891595417 0.3217789286
[ 5] 0.9301450000 0.4870370000 0.9301450000 0.6049730556 0.6826752564
[10] 0.6482628947 0.7253722500 0.5280972727 0.8769925556 0.4705703448
[15] 0.9241867391 0.6049730556 0.7856107317 0.4887525806 0.1136717045
[20] 0.4991890625 0.8769925556 0.9991834000 0.3217789286 0.9301450000
[25] 0.2304957692 0.5832475000 0.0389954722 0.8521710465 0.1476842609
[30] 0.0168363750 0.0025629017 0.0351608438 0.0625018947 0.0036365888
[35] 0.0025629017 0.0294688286 0.0061660636 0.0389954722 0.0026889914
[40] 0.0004502862 0.0000125223 0.0788155476 0.0314261300 0.0048465270
[45] 0.0025629017 0.0048465270 0.0011017083 0.0725203250 0.0220595769
[50]

type 0 = 'bh' has cumulative error of 8.03053e-07
[ 1] 1.000000e+00 1.0000000000 0.8940844244 0.8510676197 1.0000000000
[ 5] 1.0000000000 1.0000000000 1.0000000000 1.0000000000 1.0000000000
[10] 1.0000000000 1.0000000000 1.0000000000 1.0000000000 1.0000000000
[15] 1.0000000000 1.0000000000 1.0000000000 1.0000000000 0.5114323399
[20] 1.0000000000 1.0000000000 1.0000000000 1.0000000000 1.0000000000
[25] 1.0000000000 1.0000000000 0.1754486368 1.0000000000 0.6644618149
[30] 0.0757503083 0.0115310209 0.1581958559 0.2812088585 0.0163617595
[35] 0.0115310209 0.1325863108 0.0277423864 0.1754486368 0.0120983246
[40] 0.0020259303 0.0000563403 0.3546073326 0.1413926119 0.0218055202
[45] 0.0115310209 0.0218055202 0.0049568120 0.3262838334 0.0992505663
[50]

type 1 = 'by' has cumulative error of 3.64072e-07
[ 1] 1.000000e+00 1.0000000000 1.0000000000 1.0000000000 1.0000000000
[ 5] 1.0000000000 1.0000000000 1.0000000000 1.0000000000 1.0000000000
[10] 1.0000000000 1.0000000000 1.0000000000 1.0000000000 1.0000000000
[15] 1.0000000000 1.0000000000 1.0000000000 1.0000000000 1.0000000000
[20] 1.0000000000 1.0000000000 1.0000000000 1.0000000000 1.0000000000
[25] 1.0000000000 1.0000000000 0.7019185000 1.0000000000 1.0000000000
[30] 0.2020365000 0.0151667450 0.5625735000 1.0000000000 0.0290927100
[35] 0.0153774100 0.4125636000 0.0678267000 0.6803480000 0.0188229400
[40] 0.0009005725 0.0000125223 1.0000000000 0.4713919500 0.0439557650
[45] 0.0108891550 0.0484652700 0.0033051250 1.0000000000 0.2867745000
[50]

type 2 = 'bonferroni' has cumulative error of 6.50000e-08
[ 1] 9.991834e-01 0.9991834000 0.9991834000 0.9991834000 0.9991834000
[ 5] 0.9991834000 0.9991834000 0.9991834000 0.9991834000 0.9991834000
[10] 0.9991834000 0.9991834000 0.9991834000 0.9991834000 0.9991834000
[15] 0.9991834000 0.9991834000 0.9991834000 0.9991834000 0.9991834000
[20] 0.9991834000 0.9991834000 0.9991834000 0.9991834000 0.9991834000
[25] 0.9991834000 0.9991834000 0.4632662100 0.9991834000 0.9991834000
[30] 0.1575884700 0.0138396690 0.3938014500 0.7600230400 0.0250197306
[35] 0.0138396690 0.3052970640 0.0542613600 0.4626366400 0.0165641872
[40] 0.0008825610 0.0000125223 0.9930759000 0.3394022040 0.0369228426
[45] 0.0102358057 0.0397415214 0.0031729200 0.8992520300 0.2179486200
[50]

type 3 = 'hochberg' has cumulative error of 2.73750e-07
[ 1] 1.000000e+00 1.0000000000 1.0000000000 1.0000000000 1.0000000000
[ 5] 1.0000000000 1.0000000000 1.0000000000 1.0000000000 1.0000000000
[10] 1.0000000000 1.0000000000 1.0000000000 1.0000000000 1.0000000000
[15] 1.0000000000 1.0000000000 1.0000000000 1.0000000000 1.0000000000
[20] 1.0000000000 1.0000000000 1.0000000000 1.0000000000 1.0000000000
[25] 1.0000000000 1.0000000000 0.4632662100 1.0000000000 1.0000000000
[30] 0.1575884700 0.0139534054 0.3938014500 0.7600230400 0.0250197306
[35] 0.0139534054 0.3052970640 0.0542613600 0.4626366400 0.0165641872
[40] 0.0008825610 0.0000125223 0.9930759000 0.3394022040 0.0369228426
[45] 0.0102358057 0.0397415214 0.0031729200 0.8992520300 0.2179486200
[50]

type 4 = 'holm' has cumulative error of 2.80950e-07
[ 1] 9.991834e-01 0.9991834000 0.9991834000 0.9987623800 0.9991834000
[ 5] 0.9991834000 0.9991834000 0.9991834000 0.9991834000 0.9991834000
[10] 0.9991834000 0.9991834000 0.9991834000 0.9991834000 0.9991834000
[15] 0.9991834000 0.9991834000 0.9991834000 0.9991834000 0.9595180000
[20] 0.9991834000 0.9991834000 0.9991834000 0.9991834000 0.9991834000
[25] 0.9991834000 0.9991834000 0.4351894700 0.9991834000 0.9766522500
[30] 0.1414255500 0.0130434007 0.3530936533 0.6887708800 0.0238560222
[35] 0.0132245726 0.2722919760 0.0542613600 0.4218157600 0.0158112696
[40] 0.0008825610 0.0000125223 0.8743649143 0.3016908480 0.0351646120
[45] 0.0095824564 0.0387722160 0.0031729200 0.8122276400 0.1950066600
[50]

type 5 = 'hommel' has cumulative error of 4.35302e-07

```



### Version 2

{{trans|Perl 6}}

To avoid licensing issues, this version follows the approach of the Perl 6 entry of which it is a partial translation. However, the correction routines themselves have been coded independently, common code factored out into separate functions (analogous to Perl 6) and (apart from the Šidák method) agree with the Perl 6 results.

```scala
// version 1.2.21

typealias DList = List<Double>

enum class Direction { UP, DOWN }

// test also for 'Unknown' correction type
val types = listOf(
    "Benjamini-Hochberg", "Benjamini-Yekutieli", "Bonferroni", "Hochberg",
    "Holm", "Hommel", "Šidák", "Unknown"
)

fun adjusted(p: DList, type: String) = "\n$type\n${pFormat(adjust(check(p), type))}"

fun pFormat(p: DList, cols: Int = 5): String {
    var i = -cols
    val fmt = "%1.10f"
    return p.chunked(cols).map { chunk ->
        i += cols
        "[%2d]  %s".format(i, chunk.map { fmt.format(it) }.joinToString(" "))
    }.joinToString("\n")
}

fun check(p: DList): DList {
    require(p.size > 0 && p.min()!! >= 0.0 && p.max()!! <= 1.0) {
        "p-values must be in range 0.0 to 1.0"
    }
    return p
}

fun ratchet(p: DList, dir: Direction): DList {
    val pp = p.toMutableList()
    var m = pp[0]
    if (dir == Direction.UP) {
        for (i in 1 until pp.size) {
            if (pp[i] > m) pp[i] = m
            m = pp[i]
        }
    }
    else {
        for (i in 1 until pp.size) {
            if (pp[i] < m) pp[i] = m
            m = pp[i]
        }
    }
    return pp.map { if (it < 1.0) it else 1.0 }
}

fun schwartzian(p: DList, mult: DList, dir: Direction): DList {
    val size = p.size
    val order = if (dir == Direction.UP)
        p.withIndex().sortedByDescending { it.value }.map { it.index }
    else
        p.withIndex().sortedBy { it.value }.map { it.index }
    var pa = List(size) { mult[it] * p[order[it]] }
    pa = ratchet(pa, dir)
    val order2 = order.withIndex().sortedBy{ it.value }.map { it.index }
    return List(size) { pa[order2[it]] }
}

fun adjust(p: DList, type: String): DList {
    val size = p.size
    require(size > 0)
    when (type) {
        "Benjamini-Hochberg" -> {
            val mult = List(size) { size.toDouble() / (size - it) }
            return schwartzian(p, mult, Direction.UP)
        }

        "Benjamini-Yekutieli" -> {
            val q = (1..size).sumByDouble { 1.0 / it }
            val mult = List(size) { q * size / (size - it) }
            return schwartzian(p, mult, Direction.UP)
        }

        "Bonferroni" -> {
            return p.map { minOf(it * size, 1.0) }
        }

        "Hochberg" -> {
            val mult = List(size) { (it + 1).toDouble() }
            return schwartzian(p, mult, Direction.UP)
        }

        "Holm" -> {
            val mult = List(size) { (size - it).toDouble() }
            return schwartzian(p, mult, Direction.DOWN)
        }

        "Hommel" -> {
            val order = p.withIndex().sortedBy { it.value }.map { it.index }
            val s = List(size) { p[order[it]] }
            val min = List(size){ s[it] * size / ( it + 1) }.min()!!
            val q = MutableList(size) { min }
            val pa = MutableList(size) { min }
            for (j in size - 1 downTo 2) {
                val lower = IntArray(size - j + 1) { it }          // lower indices
                val upper = IntArray(j - 1) { size - j + 1 + it }  // upper indices
                var qmin = j * s[upper[0]] / 2.0
                for (i in 1 until upper.size) {
                    val temp = s[upper[i]] * j / (2.0 + i)
                    if (temp < qmin) qmin = temp
                }
                for (i in 0 until lower.size) {
                    q[lower[i]] = minOf(s[lower[i]] * j, qmin)
                }
                for (i in 0 until upper.size) q[upper[i]] = q[size - j]
                for (i in 0 until size) if (pa[i] < q[i]) pa[i] = q[i]
            }
            val order2 = order.withIndex().sortedBy{ it.value }.map { it.index }
            return List(size) { pa[order2[it]] }
        }

        "Šidák" -> {
            val m = size.toDouble()
            return p.map { 1.0 - Math.pow(1.0 - it, m) }
        }

        else -> {
            println(
                "\nSorry, do not know how to do '$type' correction.\n" +
                "Perhaps you want one of these?:\n" +
                types.dropLast(1).map { "  $it" }.joinToString("\n")
            )
            System.exit(1)
        }
    }
    return p
}

fun main(args: Array<String>) {
    val pValues = listOf(
        4.533744e-01, 7.296024e-01, 9.936026e-02, 9.079658e-02, 1.801962e-01,
        8.752257e-01, 2.922222e-01, 9.115421e-01, 4.355806e-01, 5.324867e-01,
        4.926798e-01, 5.802978e-01, 3.485442e-01, 7.883130e-01, 2.729308e-01,
        8.502518e-01, 4.268138e-01, 6.442008e-01, 3.030266e-01, 5.001555e-02,
        3.194810e-01, 7.892933e-01, 9.991834e-01, 1.745691e-01, 9.037516e-01,
        1.198578e-01, 3.966083e-01, 1.403837e-02, 7.328671e-01, 6.793476e-02,
        4.040730e-03, 3.033349e-04, 1.125147e-02, 2.375072e-02, 5.818542e-04,
        3.075482e-04, 8.251272e-03, 1.356534e-03, 1.360696e-02, 3.764588e-04,
        1.801145e-05, 2.504456e-07, 3.310253e-02, 9.427839e-03, 8.791153e-04,
        2.177831e-04, 9.693054e-04, 6.610250e-05, 2.900813e-02, 5.735490e-03
    )

    types.forEach { println(adjusted(pValues, it)) }
}
```


{{out}}
Same as Perl 6 entry except:

```txt

....

Šidák
[ 0]  1.0000000000 1.0000000000 0.9946598274 0.9914285749 0.9999515274
[ 5]  1.0000000000 0.9999999688 1.0000000000 1.0000000000 1.0000000000
[10]  1.0000000000 1.0000000000 0.9999999995 1.0000000000 0.9999998801
[15]  1.0000000000 1.0000000000 1.0000000000 0.9999999855 0.9231179729
[20]  0.9999999956 1.0000000000 1.0000000000 0.9999317605 1.0000000000
[25]  0.9983109511 1.0000000000 0.5068253940 1.0000000000 0.9703301333
[30]  0.1832692440 0.0150545753 0.4320729669 0.6993672225 0.0286818157
[35]  0.0152621104 0.3391808707 0.0656206307 0.4959194266 0.0186503726
[40]  0.0009001752 0.0000125222 0.8142104886 0.3772612062 0.0430222116
[45]  0.0108312558 0.0473319661 0.0032997780 0.7705015898 0.2499384839

Sorry, do not know how to do 'Unknown' correction.
Perhaps you want one of these?:
  Benjamini-Hochberg
  Benjamini-Yekutieli
  Bonferroni
  Hochberg
  Holm
  Hommel
  Šidák

```



## Perl

{{trans|C}}
''This work is based on R source code covered by the '''GPL''' license. It is thus a modified version, also covered by the GPL. See the [https://www.gnu.org/licenses/gpl-faq.html#GPLRequireSourcePostedPublic FAQ about GNU licenses]''.

```perl
#!/usr/bin/env perl

use strict;
use warnings;

sub pmin {
	my $array_ref = shift;
	my $x = 1;
	unless ((ref $array_ref) =~ m/ARRAY/) {
		print "cummin requires an array.\n";
		die;
	}
	my @pmin_array;
	my $n = scalar @$array_ref;
	for (my $index = 0; $index < $n; $index++) {
		if (@$array_ref[$index] < $x) {
			$pmin_array[$index] = @$array_ref[$index];
		} else {
			$pmin_array[$index] = $x;
		}
	}
	return @pmin_array;
}

sub cummin {
	my $array_ref = shift;
	unless ((ref $array_ref) =~ m/ARRAY/) {
		print "cummin requires an array.\n";
		die;
	}
	my @cummin;
	my $cumulative_min = @$array_ref[0];
	foreach my $p (@$array_ref) {
		if ($p < $cumulative_min) {
			$cumulative_min = $p;
		}
		push @cummin, $cumulative_min;
	}
	return @cummin;
}

sub cummax {
	my $array_ref = shift;
	unless ((ref $array_ref) =~ m/ARRAY/) {
		print "cummin requires an array.\n";
		die;
	}
	my @cummax;
	my $cumulative_max = @$array_ref[0];
	foreach my $p (@$array_ref) {
		if ($p > $cumulative_max) {
			$cumulative_max = $p;
		}
		push @cummax, $cumulative_max;
	}
	return @cummax;
}

sub order {#made to match R's "order"
	my $array_ref = shift;
	my $decreasing = 'false';
	if (defined $_[0]) {
		my $option = shift;
		if ($option =~ m/true/i) {
			$decreasing = 'true';
		} elsif ($option =~ m/false/i) {
			#do nothing, it's already set to false
		} else {
			print "2nd option should only be case-insensitive 'true' or 'false'";
			die;
		}
	}
	unless ((ref $array_ref) =~ m/ARRAY/) {
		print "You should have entered an array.\n";
		die;
	}
	my @array;
	my $max_index = scalar @$array_ref-1;
	if ($decreasing eq 'false') {
		@array = sort { @$array_ref[$a] <=> @$array_ref[$b] } 0..$max_index;
	} elsif ($decreasing eq 'true') {
		@array = sort { @$array_ref[$b] <=> @$array_ref[$a] } 0..$max_index;
	}
}

use List::Util 'min';

sub p_adjust {
	my $pvalues_ref = shift;
	unless ((ref $pvalues_ref) =~ m/ARRAY/) {
		print "p_adjust requires an array.\n";
		die;
	}
	my $method;
	if (defined $_[0]) {
		$method = shift;
	} else {
		$method = 'Holm';
	}
	my %methods = (
						'bh'         => 1,
						'fdr'        => 1,
						'by'         => 1,
						'holm'       => 1,
						'hommel'     => 1,
						'bonferroni' => 1,
						'hochberg'   => 1
						);
	my $method_found = 'no';
	foreach my $key (keys %methods) {
		if ((uc $method) eq (uc $key)) {
			$method = $key;
			$method_found = 'yes';
			last;
		}
	}
	if ($method_found eq 'no') {
		if ($method =~ m/benjamini-?\s*hochberg/i) {
			$method = 'bh';
			$method_found = 'yes';
		} elsif ($method =~ m/benjamini-?\s*yekutieli/i) {
			$method = 'by';
			$method_found = 'yes';
		}
	}
	if ($method_found eq 'no') {
		print "No method could be determined from $method.\n";
		die;
	}
	my $lp = scalar @$pvalues_ref;
	my $n  = $lp;
	my @qvalues;
	if ($method eq 'hochberg') {
		my @o = order($pvalues_ref, 'TRUE');
		my @cummin_input;
		for (my $index = 0; $index < $n; $index++) {
			$cummin_input[$index] = ($index+1)* @$pvalues_ref[$o[$index]];#PVALUES[$o[$index]] is p[o]
		}
		my @cummin = cummin(\@cummin_input);
		undef @cummin_input;
		my @pmin   = pmin(\@cummin);
		undef @cummin;
		my @ro = order(\@o);
		undef @o;
		@qvalues = @pmin[@ro];
	} elsif ($method eq 'bh') {
		my @o = order($pvalues_ref, 'TRUE');
		my @cummin_input;
		for (my $index = 0; $index < $n; $index++) {
			$cummin_input[$index] = ($n/($n-$index))* @$pvalues_ref[$o[$index]];#PVALUES[$o[$index]] is p[o]
		}
		my @ro = order(\@o);
		undef @o;
		my @cummin = cummin(\@cummin_input);
		undef @cummin_input;
		my @pmin   = pmin(\@cummin);
		undef @cummin;
		@qvalues = @pmin[@ro];
	} elsif ($method eq 'by') {
		my $q = 0.0;
		my @o = order($pvalues_ref, 'TRUE');
		my @ro = order(\@o);
		for (my $index = 1; $index < ($n+1); $index++) {
			$q += 1.0 / $index;
		}
		my @cummin_input;
		for (my $index = 0; $index < $n; $index++) {
			$cummin_input[$index] = $q * ($n/($n-$index)) * @$pvalues_ref[$o[$index]];#PVALUES[$o[$index]] is p[o]
		}
		my @cummin = cummin(\@cummin_input);
		undef @cummin_input;
		my @pmin   = pmin(\@cummin);
		undef @cummin;
		@qvalues = @pmin[@ro];
	} elsif ($method eq 'bonferroni') {
		for (my $index = 0; $index < $n; $index++) {
			my $q = @$pvalues_ref[$index]*$n;
			if ((0 <= $q) && ($q < 1)) {
				$qvalues[$index] = $q;
			} elsif ($q >= 1) {
				$qvalues[$index] = 1.0;
			} else {
				print "Failed to get Bonferroni adjusted p.";
				die;
			}
		}
	} elsif ($method eq 'holm') {
		my @o = order($pvalues_ref);
		my @cummax_input;
		for (my $index = 0; $index < $n; $index++) {
			$cummax_input[$index] = ($n - $index) * @$pvalues_ref[$o[$index]];
		}
		my @ro = order(\@o);
		undef @o;
		my @cummax = cummax(\@cummax_input);
		undef @cummax_input;
		my @pmin = pmin(\@cummax);
		undef @cummax;
		@qvalues = @pmin[@ro];
	} elsif ($method eq 'hommel') {
		my @i = 1..$n;
		my @o = order($pvalues_ref);
		my @p = @$pvalues_ref[@o];
		my @ro = order(\@o);
		undef @o;
		my @pa;
		my @q;
		my $min = $n*$p[0];
		for (my $index = 0; $index < $n; $index++) {
			my $temp = $n*$p[$index] / ($index + 1);
			if ($temp < $min) {
				$min = $temp;
			}
		}
		for (my $index = 0; $index < $n; $index++) {
			$pa[$index] = $min;#q <- pa <- rep.int(min(n * p/i), n)
			 $q[$index] = $min;#q <- pa <- rep.int(min(n * p/i), n)
		}
		for (my $j = ($n-1); $j >= 2; $j--) {
#			printf("j = %zu\n", j);
			my @ij = 1..($n - $j + 1);#ij <- seq_len(n - j + 1)
			for (my $i = 0; $i < $n - $j + 1; $i++) {
				$ij[$i]--;#R's indices are 1-based, C's are 0-based
			}
			my $I2_LENGTH = $j - 1;
			my @i2;
			for (my $i = 0; $i < $I2_LENGTH; $i++) {
				$i2[$i] = $n-$j+2+$i-1;
#R's indices are 1-based, C's are 0-based, I added the -1
			}

			my $q1 = $j * $p[$i2[0]] / 2.0;
			for (my $i = 1; $i < $I2_LENGTH; $i++) {#loop through 2:j
				my $TEMP_Q1 = $j * $p[$i2[$i]] / (2 + $i);
				if ($TEMP_Q1 < $q1) {
					$q1 = $TEMP_Q1;
				}
			}

			for (my $i = 0; $i < ($n - $j + 1); $i++) {#q[ij] <- pmin(j * p[ij], q1)
				$q[$ij[$i]] = min( $j*$p[$ij[$i]], $q1);
			}

			for (my $i = 0; $i < $I2_LENGTH; $i++) {#q[i2] <- q[n - j + 1]
				$q[$i2[$i]] = $q[$n - $j];#subtract 1 because of starting index difference
			}

			for (my $i = 0; $i < $n; $i++) {#pa <- pmax(pa, q)
				if ($pa[$i] < $q[$i]) {
					$pa[$i] = $q[$i];
				}
			}
#			printf("j = %zu, pa = \n", j);
#				double_say(pa, N);
		}#end j loop
		@qvalues = @pa[@ro];
	} else {
		print "$method doesn't fit my types.\n";
		die;
	}
	return @qvalues;
}
my @pvalues = (4.533744e-01, 7.296024e-01, 9.936026e-02, 9.079658e-02, 1.801962e-01,
8.752257e-01, 2.922222e-01, 9.115421e-01, 4.355806e-01, 5.324867e-01,
4.926798e-01, 5.802978e-01, 3.485442e-01, 7.883130e-01, 2.729308e-01,
8.502518e-01, 4.268138e-01, 6.442008e-01, 3.030266e-01, 5.001555e-02,
3.194810e-01, 7.892933e-01, 9.991834e-01, 1.745691e-01, 9.037516e-01,
1.198578e-01, 3.966083e-01, 1.403837e-02, 7.328671e-01, 6.793476e-02,
4.040730e-03, 3.033349e-04, 1.125147e-02, 2.375072e-02, 5.818542e-04,
3.075482e-04, 8.251272e-03, 1.356534e-03, 1.360696e-02, 3.764588e-04,
1.801145e-05, 2.504456e-07, 3.310253e-02, 9.427839e-03, 8.791153e-04,
2.177831e-04, 9.693054e-04, 6.610250e-05, 2.900813e-02, 5.735490e-03);

my %correct_answers = (
	'Benjamini-Hochberg' => [6.126681e-01, 8.521710e-01, 1.987205e-01, 1.891595e-01, 3.217789e-01,
9.301450e-01, 4.870370e-01, 9.301450e-01, 6.049731e-01, 6.826753e-01,
6.482629e-01, 7.253722e-01, 5.280973e-01, 8.769926e-01, 4.705703e-01,
9.241867e-01, 6.049731e-01, 7.856107e-01, 4.887526e-01, 1.136717e-01,
4.991891e-01, 8.769926e-01, 9.991834e-01, 3.217789e-01, 9.301450e-01,
2.304958e-01, 5.832475e-01, 3.899547e-02, 8.521710e-01, 1.476843e-01,
1.683638e-02, 2.562902e-03, 3.516084e-02, 6.250189e-02, 3.636589e-03,
2.562902e-03, 2.946883e-02, 6.166064e-03, 3.899547e-02, 2.688991e-03,
4.502862e-04, 1.252228e-05, 7.881555e-02, 3.142613e-02, 4.846527e-03,
2.562902e-03, 4.846527e-03, 1.101708e-03, 7.252032e-02, 2.205958e-02],
	'Benjamini-Yekutieli' => [1.000000e+00, 1.000000e+00, 8.940844e-01, 8.510676e-01, 1.000000e+00,
1.000000e+00, 1.000000e+00, 1.000000e+00, 1.000000e+00, 1.000000e+00,
1.000000e+00, 1.000000e+00, 1.000000e+00, 1.000000e+00, 1.000000e+00,
1.000000e+00, 1.000000e+00, 1.000000e+00, 1.000000e+00, 5.114323e-01,
1.000000e+00, 1.000000e+00, 1.000000e+00, 1.000000e+00, 1.000000e+00,
1.000000e+00, 1.000000e+00, 1.754486e-01, 1.000000e+00, 6.644618e-01,
7.575031e-02, 1.153102e-02, 1.581959e-01, 2.812089e-01, 1.636176e-02,
1.153102e-02, 1.325863e-01, 2.774239e-02, 1.754486e-01, 1.209832e-02,
2.025930e-03, 5.634031e-05, 3.546073e-01, 1.413926e-01, 2.180552e-02,
1.153102e-02, 2.180552e-02, 4.956812e-03, 3.262838e-01, 9.925057e-02],
	'Bonferroni' => [1.000000e+00, 1.000000e+00, 1.000000e+00, 1.000000e+00, 1.000000e+00,
1.000000e+00, 1.000000e+00, 1.000000e+00, 1.000000e+00, 1.000000e+00,
1.000000e+00, 1.000000e+00, 1.000000e+00, 1.000000e+00, 1.000000e+00,
1.000000e+00, 1.000000e+00, 1.000000e+00, 1.000000e+00, 1.000000e+00,
1.000000e+00, 1.000000e+00, 1.000000e+00, 1.000000e+00, 1.000000e+00,
1.000000e+00, 1.000000e+00, 7.019185e-01, 1.000000e+00, 1.000000e+00,
2.020365e-01, 1.516674e-02, 5.625735e-01, 1.000000e+00, 2.909271e-02,
1.537741e-02, 4.125636e-01, 6.782670e-02, 6.803480e-01, 1.882294e-02,
9.005725e-04, 1.252228e-05, 1.000000e+00, 4.713920e-01, 4.395577e-02,
1.088915e-02, 4.846527e-02, 3.305125e-03, 1.000000e+00, 2.867745e-01],

	'Hochberg' => [9.991834e-01, 9.991834e-01, 9.991834e-01, 9.991834e-01, 9.991834e-01,
9.991834e-01, 9.991834e-01, 9.991834e-01, 9.991834e-01, 9.991834e-01,
9.991834e-01, 9.991834e-01, 9.991834e-01, 9.991834e-01, 9.991834e-01,
9.991834e-01, 9.991834e-01, 9.991834e-01, 9.991834e-01, 9.991834e-01,
9.991834e-01, 9.991834e-01, 9.991834e-01, 9.991834e-01, 9.991834e-01,
9.991834e-01, 9.991834e-01, 4.632662e-01, 9.991834e-01, 9.991834e-01,
1.575885e-01, 1.383967e-02, 3.938014e-01, 7.600230e-01, 2.501973e-02,
1.383967e-02, 3.052971e-01, 5.426136e-02, 4.626366e-01, 1.656419e-02,
8.825610e-04, 1.252228e-05, 9.930759e-01, 3.394022e-01, 3.692284e-02,
1.023581e-02, 3.974152e-02, 3.172920e-03, 8.992520e-01, 2.179486e-01],
	'Holm' => [1.000000e+00, 1.000000e+00, 1.000000e+00, 1.000000e+00, 1.000000e+00,
1.000000e+00, 1.000000e+00, 1.000000e+00, 1.000000e+00, 1.000000e+00,
1.000000e+00, 1.000000e+00, 1.000000e+00, 1.000000e+00, 1.000000e+00,
1.000000e+00, 1.000000e+00, 1.000000e+00, 1.000000e+00, 1.000000e+00,
1.000000e+00, 1.000000e+00, 1.000000e+00, 1.000000e+00, 1.000000e+00,
1.000000e+00, 1.000000e+00, 4.632662e-01, 1.000000e+00, 1.000000e+00,
1.575885e-01, 1.395341e-02, 3.938014e-01, 7.600230e-01, 2.501973e-02,
1.395341e-02, 3.052971e-01, 5.426136e-02, 4.626366e-01, 1.656419e-02,
8.825610e-04, 1.252228e-05, 9.930759e-01, 3.394022e-01, 3.692284e-02,
1.023581e-02, 3.974152e-02, 3.172920e-03, 8.992520e-01, 2.179486e-01],

	'Hommel' => [9.991834e-01, 9.991834e-01, 9.991834e-01, 9.987624e-01, 9.991834e-01,
9.991834e-01, 9.991834e-01, 9.991834e-01, 9.991834e-01, 9.991834e-01,
9.991834e-01, 9.991834e-01, 9.991834e-01, 9.991834e-01, 9.991834e-01,
9.991834e-01, 9.991834e-01, 9.991834e-01, 9.991834e-01, 9.595180e-01,
9.991834e-01, 9.991834e-01, 9.991834e-01, 9.991834e-01, 9.991834e-01,
9.991834e-01, 9.991834e-01, 4.351895e-01, 9.991834e-01, 9.766522e-01,
1.414256e-01, 1.304340e-02, 3.530937e-01, 6.887709e-01, 2.385602e-02,
1.322457e-02, 2.722920e-01, 5.426136e-02, 4.218158e-01, 1.581127e-02,
8.825610e-04, 1.252228e-05, 8.743649e-01, 3.016908e-01, 3.516461e-02,
9.582456e-03, 3.877222e-02, 3.172920e-03, 8.122276e-01, 1.950067e-01]);


foreach my $method ('Hochberg','Benjamini-Hochberg','Benjamini-Yekutieli', 'Bonferroni', 'Holm', 'Hommel') {
	print "$method\n";
	my @qvalues = p_adjust(\@pvalues, $method);
	my $error = 0.0;
	foreach my $q (0..$#qvalues) {
		$error += abs($qvalues[$q] - $correct_answers{$method}[$q]);
	}
	printf("type $method has cumulative error of %g.\n", $error);
}

```


{{out}}

```txt

Hochberg
type Hochberg has cumulative error of 2.7375e-07.
Benjamini-Hochberg
type Benjamini-Hochberg has cumulative error of 8.03053e-07.
Benjamini-Yekutieli
type Benjamini-Yekutieli has cumulative error of 3.64072e-07.
Bonferroni
type Bonferroni has cumulative error of 6.5e-08.
Holm
type Holm has cumulative error of 2.8095e-07.
Hommel
type Hommel has cumulative error of 4.35302e-07.

```



## Perl 6

{{works with|Rakudo|2019.03.1}}


```perl6
########################### Helper subs ###########################

sub adjusted (@p, $type) { "\n$type\n" ~ format adjust( check(@p), $type ) }

sub format ( @p, $cols = 5 ) {
    my $i = -$cols;
    my $fmt = "%1.10f";
    join "\n", @p.rotor($cols, :partial).map:
      { sprintf "[%2d]  { join ' ', $fmt xx $_ }", $i+=$cols, $_ };
}

sub check ( @p ) { die 'p-values must be in range 0.0 to 1.0' if @p.min < 0 or 1 < @p.max; @p }

multi ratchet ( 'up', @p ) { my $m; @p[$_] min= $m, $m = @p[$_] for ^@p; @p }

multi ratchet ( 'dn', @p ) { my $m; @p[$_] max= $m, $m = @p[$_] for ^@p .reverse; @p }

sub schwartzian ( @p, &transform, :$ratchet ) {
    my @pa = @p.map( {[$_, $++]} ).sort( -*.[0] ).map: { [transform(.[0]), .[1]] };
    @pa[*;0] = ratchet($ratchet, @pa»[0]);
    @pa.sort( *.[1] )»[0]
}

############# The various p-value correction routines #############

multi adjust( @p, 'Benjamini-Hochberg' ) {
    @p.&schwartzian: * * @p / (@p - $++) min 1, :ratchet('up')
}

multi adjust( @p, 'Benjamini-Yekutieli' ) {
    my \r = ^@p .map( { 1 / ++$ } ).sum;
    @p.&schwartzian: * * r * @p / (@p - $++) min 1, :ratchet('up')
}

multi adjust( @p, 'Hochberg' ) {
    my \m = @p.max;
    @p.&schwartzian: * * ++$ min m, :ratchet('up')
}

multi adjust( @p, 'Holm' ) {
    @p.&schwartzian: * * ++$ min 1, :ratchet('dn')
}

multi adjust( @p, 'Šidák' ) {
    @p.&schwartzian: 1 - (1 - *) ** ++$, :ratchet('dn')
}

multi adjust( @p, 'Bonferroni' ) {
    @p.map: * * @p min 1
}

# Hommel correction can't be easily reduced to a one pass transform
multi adjust( @p, 'Hommel' ) {
    my @s  = @p.map( {[$_, $++]} ).sort: *.[0] ; # sorted
    my \z  = +@p; # array si(z)e
    my @pa = @s»[0].map( * * z / ++$ ).min xx z; # p adjusted
    my @q;        # scratch array
    for (1 ..^ z).reverse -> $i {
        my @L  = 0 .. z - $i; # lower indices
        my @U  = z - $i ^..^ z; # upper indices
        my $q  = @s[@U]»[0].map( { $_ * $i / (2 + $++) } ).min;
        @q[@L] = @s[@L]»[0].map: { min $_ * $i, $q, @s[*-1][0] };
        @pa    = ^z .map: { max @pa[$_], @q[$_] }
    }
    @pa[@s[*;1].map( {[$_, $++]} ).sort( *.[0] )»[1]]
}

multi adjust ( @p, $unknown ) {
    note "\nSorry, do not know how to do $unknown correction.\n" ~
    "Perhaps you want one of these?:\n" ~
    <Benjamini-Hochberg Benjamini-Yekutieli Bonferroni Hochberg
    Holm Hommel Šidák>.join("\n");
    exit
}

########################### The task ###########################

my @p-values =
    4.533744e-01, 7.296024e-01, 9.936026e-02, 9.079658e-02, 1.801962e-01,
    8.752257e-01, 2.922222e-01, 9.115421e-01, 4.355806e-01, 5.324867e-01,
    4.926798e-01, 5.802978e-01, 3.485442e-01, 7.883130e-01, 2.729308e-01,
    8.502518e-01, 4.268138e-01, 6.442008e-01, 3.030266e-01, 5.001555e-02,
    3.194810e-01, 7.892933e-01, 9.991834e-01, 1.745691e-01, 9.037516e-01,
    1.198578e-01, 3.966083e-01, 1.403837e-02, 7.328671e-01, 6.793476e-02,
    4.040730e-03, 3.033349e-04, 1.125147e-02, 2.375072e-02, 5.818542e-04,
    3.075482e-04, 8.251272e-03, 1.356534e-03, 1.360696e-02, 3.764588e-04,
    1.801145e-05, 2.504456e-07, 3.310253e-02, 9.427839e-03, 8.791153e-04,
    2.177831e-04, 9.693054e-04, 6.610250e-05, 2.900813e-02, 5.735490e-03
;

for < Benjamini-Hochberg Benjamini-Yekutieli Bonferroni Hochberg Holm Hommel Šidák >
{
    say adjusted @p-values, $_
}
```


{{out}}
<pre style="height:60ex;overflow:scroll;">Benjamini-Hochberg
[ 0]  0.6126681081 0.8521710465 0.1987205200 0.1891595417 0.3217789286
[ 5]  0.9301450000 0.4870370000 0.9301450000 0.6049730556 0.6826752564
[10]  0.6482628947 0.7253722500 0.5280972727 0.8769925556 0.4705703448
[15]  0.9241867391 0.6049730556 0.7856107317 0.4887525806 0.1136717045
[20]  0.4991890625 0.8769925556 0.9991834000 0.3217789286 0.9301450000
[25]  0.2304957692 0.5832475000 0.0389954722 0.8521710465 0.1476842609
[30]  0.0168363750 0.0025629017 0.0351608438 0.0625018947 0.0036365888
[35]  0.0025629017 0.0294688286 0.0061660636 0.0389954722 0.0026889914
[40]  0.0004502863 0.0000125223 0.0788155476 0.0314261300 0.0048465270
[45]  0.0025629017 0.0048465270 0.0011017083 0.0725203250 0.0220595769

Benjamini-Yekutieli
[ 0]  1.0000000000 1.0000000000 0.8940844244 0.8510676197 1.0000000000
[ 5]  1.0000000000 1.0000000000 1.0000000000 1.0000000000 1.0000000000
[10]  1.0000000000 1.0000000000 1.0000000000 1.0000000000 1.0000000000
[15]  1.0000000000 1.0000000000 1.0000000000 1.0000000000 0.5114323399
[20]  1.0000000000 1.0000000000 1.0000000000 1.0000000000 1.0000000000
[25]  1.0000000000 1.0000000000 0.1754486368 1.0000000000 0.6644618149
[30]  0.0757503083 0.0115310209 0.1581958559 0.2812088585 0.0163617595
[35]  0.0115310209 0.1325863108 0.0277423864 0.1754486368 0.0120983246
[40]  0.0020259303 0.0000563403 0.3546073326 0.1413926119 0.0218055202
[45]  0.0115310209 0.0218055202 0.0049568120 0.3262838334 0.0992505663

Bonferroni
[ 0]  1.0000000000 1.0000000000 1.0000000000 1.0000000000 1.0000000000
[ 5]  1.0000000000 1.0000000000 1.0000000000 1.0000000000 1.0000000000
[10]  1.0000000000 1.0000000000 1.0000000000 1.0000000000 1.0000000000
[15]  1.0000000000 1.0000000000 1.0000000000 1.0000000000 1.0000000000
[20]  1.0000000000 1.0000000000 1.0000000000 1.0000000000 1.0000000000
[25]  1.0000000000 1.0000000000 0.7019185000 1.0000000000 1.0000000000
[30]  0.2020365000 0.0151667450 0.5625735000 1.0000000000 0.0290927100
[35]  0.0153774100 0.4125636000 0.0678267000 0.6803480000 0.0188229400
[40]  0.0009005725 0.0000125223 1.0000000000 0.4713919500 0.0439557650
[45]  0.0108891550 0.0484652700 0.0033051250 1.0000000000 0.2867745000

Hochberg
[ 0]  0.9991834000 0.9991834000 0.9991834000 0.9991834000 0.9991834000
[ 5]  0.9991834000 0.9991834000 0.9991834000 0.9991834000 0.9991834000
[10]  0.9991834000 0.9991834000 0.9991834000 0.9991834000 0.9991834000
[15]  0.9991834000 0.9991834000 0.9991834000 0.9991834000 0.9991834000
[20]  0.9991834000 0.9991834000 0.9991834000 0.9991834000 0.9991834000
[25]  0.9991834000 0.9991834000 0.4632662100 0.9991834000 0.9991834000
[30]  0.1575884700 0.0138396690 0.3938014500 0.7600230400 0.0250197306
[35]  0.0138396690 0.3052970640 0.0542613600 0.4626366400 0.0165641872
[40]  0.0008825611 0.0000125223 0.9930759000 0.3394022040 0.0369228426
[45]  0.0102358057 0.0397415214 0.0031729200 0.8992520300 0.2179486200

Holm
[ 0]  1.0000000000 1.0000000000 1.0000000000 1.0000000000 1.0000000000
[ 5]  1.0000000000 1.0000000000 1.0000000000 1.0000000000 1.0000000000
[10]  1.0000000000 1.0000000000 1.0000000000 1.0000000000 1.0000000000
[15]  1.0000000000 1.0000000000 1.0000000000 1.0000000000 1.0000000000
[20]  1.0000000000 1.0000000000 1.0000000000 1.0000000000 1.0000000000
[25]  1.0000000000 1.0000000000 0.4632662100 1.0000000000 1.0000000000
[30]  0.1575884700 0.0139534054 0.3938014500 0.7600230400 0.0250197306
[35]  0.0139534054 0.3052970640 0.0542613600 0.4626366400 0.0165641872
[40]  0.0008825611 0.0000125223 0.9930759000 0.3394022040 0.0369228426
[45]  0.0102358057 0.0397415214 0.0031729200 0.8992520300 0.2179486200

Hommel
[ 0]  0.9991834000 0.9991834000 0.9991834000 0.9987623800 0.9991834000
[ 5]  0.9991834000 0.9991834000 0.9991834000 0.9991834000 0.9991834000
[10]  0.9991834000 0.9991834000 0.9991834000 0.9991834000 0.9991834000
[15]  0.9991834000 0.9991834000 0.9991834000 0.9991834000 0.9595180000
[20]  0.9991834000 0.9991834000 0.9991834000 0.9991834000 0.9991834000
[25]  0.9991834000 0.9991834000 0.4351894700 0.9991834000 0.9766522500
[30]  0.1414255500 0.0130434007 0.3530936533 0.6887708800 0.0238560222
[35]  0.0132245726 0.2722919760 0.0542613600 0.4218157600 0.0158112696
[40]  0.0008825611 0.0000125223 0.8743649143 0.3016908480 0.0351646120
[45]  0.0095824564 0.0387722160 0.0031729200 0.8122276400 0.1950066600

Šidák
[ 0]  0.9998642526 0.9999922727 0.9341844137 0.9234670175 0.9899922294
[ 5]  0.9999922727 0.9992955735 0.9999922727 0.9998642526 0.9998909746
[10]  0.9998642526 0.9999288207 0.9995533892 0.9999922727 0.9990991210
[15]  0.9999922727 0.9998642526 0.9999674876 0.9992955735 0.7741716825
[20]  0.9993332472 0.9999922727 0.9999922727 0.9899922294 0.9999922727
[25]  0.9589019598 0.9998137104 0.3728369461 0.9999922727 0.8605248833
[30]  0.1460714182 0.0138585952 0.3270159382 0.5366136349 0.0247164330
[35]  0.0138585952 0.2640282766 0.0528503728 0.3723753774 0.0164308228
[40]  0.0008821796 0.0000125222 0.6357389664 0.2889497995 0.0362651575
[45]  0.0101847015 0.0389807074 0.0031679962 0.5985019850 0.1963376344
```



## Phix

Translation of Kotlin (version 2), except for the Hommel part, which is translated from Go.

Note that sq_min(), extract(), and custom_sort() as used below require 0.8.0+

```Phix
enum UP, DOWN

function ratchet(sequence p, integer direction)
    atom m = p[1]
    for i=1 to length(p) do
        if iff(direction=UP?p[i]>m:p[i]<m) then p[i] = m end if
        m = p[i]
    end for
    return sq_min(p,1)
end function

function schwartzian(sequence p, mult, integer direction)
    sequence order = custom_sort(p,tagset(length(p)))
    if direction=UP then order = reverse(order) end if
    sequence pa = ratchet(sq_mul(mult,extract(p,order)), direction)
    return extract(pa,order,invert:=true)
end function

function adjust(sequence p, string method)
    integer size = length(p)
    sequence mult = tagset(size)
    switch method

        case "Benjamini-Hochberg":
            mult = sq_div(size,sq_sub(size+1,mult))
            return schwartzian(p, mult, UP)

        case "Benjamini-Yekutieli":
            atom q = sum(sq_div(1,mult))
            mult = sq_div(q*size,sq_sub(size+1,mult))
            return schwartzian(p, mult, UP)

        case "Bonferroni":
            return sq_min(sq_mul(p,size),1)

        case "Hochberg":
            return schwartzian(p, mult, UP)

        case "Holm":
            mult = sq_sub(size+1,mult)
            return schwartzian(p, mult, DOWN)

        case "Hommel":
            sequence ivdx = repeat(0,size)
            for i=1 to size do ivdx[i] = {p[i],i} end for
            ivdx = sort(ivdx)
            sequence s = vslice(ivdx,1),
                     m = sq_div(sq_mul(s,size),mult),
                     {q,pa} @= repeat(min(m),size),
                     order = vslice(ivdx,2)
            for j=size-1 to 2 by -1 do
                sequence lwr = tagset(size-j+1),
                         upr = sq_add(size-j+1,tagset(j-1))
                atom qmin = j*s[upr[1]]/2
                for i=2 to length(upr) do
                    qmin = min(s[upr[i]]*j/(i+1),qmin)
                end for
                for i=1 to length(lwr) do
                    q[lwr[i]] = min(s[lwr[i]]*j, qmin)
                end for
                for i=1 to length(upr) do
                    q[upr[i]] = q[size-j+1]
                end for
                pa = sq_max(pa,q)
            end for
            return extract(pa,order,invert:=true)

        case "Sidak":
            for i=1 to length(p) do
                p[i] = 1 - power(1-p[i],size)
            end for
            return p

        else
            return {}   -- (unknown method)

    end switch
    return p
end function

constant {types,correct_answers} = columnize({
    {"Benjamini-Hochberg",
     {6.126681e-01, 8.521710e-01, 1.987205e-01, 1.891595e-01, 3.217789e-01,
      9.301450e-01, 4.870370e-01, 9.301450e-01, 6.049731e-01, 6.826753e-01,
      6.482629e-01, 7.253722e-01, 5.280973e-01, 8.769926e-01, 4.705703e-01,
      9.241867e-01, 6.049731e-01, 7.856107e-01, 4.887526e-01, 1.136717e-01,
      4.991891e-01, 8.769926e-01, 9.991834e-01, 3.217789e-01, 9.301450e-01,
      2.304958e-01, 5.832475e-01, 3.899547e-02, 8.521710e-01, 1.476843e-01,
      1.683638e-02, 2.562902e-03, 3.516084e-02, 6.250189e-02, 3.636589e-03,
      2.562902e-03, 2.946883e-02, 6.166064e-03, 3.899547e-02, 2.688991e-03,
      4.502862e-04, 1.252228e-05, 7.881555e-02, 3.142613e-02, 4.846527e-03,
      2.562902e-03, 4.846527e-03, 1.101708e-03, 7.252032e-02, 2.205958e-02}},
    {"Benjamini-Yekutieli",
     {1.000000e+00, 1.000000e+00, 8.940844e-01, 8.510676e-01, 1.000000e+00,
      1.000000e+00, 1.000000e+00, 1.000000e+00, 1.000000e+00, 1.000000e+00,
      1.000000e+00, 1.000000e+00, 1.000000e+00, 1.000000e+00, 1.000000e+00,
      1.000000e+00, 1.000000e+00, 1.000000e+00, 1.000000e+00, 5.114323e-01,
      1.000000e+00, 1.000000e+00, 1.000000e+00, 1.000000e+00, 1.000000e+00,
      1.000000e+00, 1.000000e+00, 1.754486e-01, 1.000000e+00, 6.644618e-01,
      7.575031e-02, 1.153102e-02, 1.581959e-01, 2.812089e-01, 1.636176e-02,
      1.153102e-02, 1.325863e-01, 2.774239e-02, 1.754486e-01, 1.209832e-02,
      2.025930e-03, 5.634031e-05, 3.546073e-01, 1.413926e-01, 2.180552e-02,
      1.153102e-02, 2.180552e-02, 4.956812e-03, 3.262838e-01, 9.925057e-02}},
    {"Bonferroni",
     {1.000000e+00, 1.000000e+00, 1.000000e+00, 1.000000e+00, 1.000000e+00,
      1.000000e+00, 1.000000e+00, 1.000000e+00, 1.000000e+00, 1.000000e+00,
      1.000000e+00, 1.000000e+00, 1.000000e+00, 1.000000e+00, 1.000000e+00,
      1.000000e+00, 1.000000e+00, 1.000000e+00, 1.000000e+00, 1.000000e+00,
      1.000000e+00, 1.000000e+00, 1.000000e+00, 1.000000e+00, 1.000000e+00,
      1.000000e+00, 1.000000e+00, 7.019185e-01, 1.000000e+00, 1.000000e+00,
      2.020365e-01, 1.516674e-02, 5.625735e-01, 1.000000e+00, 2.909271e-02,
      1.537741e-02, 4.125636e-01, 6.782670e-02, 6.803480e-01, 1.882294e-02,
      9.005725e-04, 1.252228e-05, 1.000000e+00, 4.713920e-01, 4.395577e-02,
      1.088915e-02, 4.846527e-02, 3.305125e-03, 1.000000e+00, 2.867745e-01}},
    {"Hochberg",
     {9.991834e-01, 9.991834e-01, 9.991834e-01, 9.991834e-01, 9.991834e-01,
      9.991834e-01, 9.991834e-01, 9.991834e-01, 9.991834e-01, 9.991834e-01,
      9.991834e-01, 9.991834e-01, 9.991834e-01, 9.991834e-01, 9.991834e-01,
      9.991834e-01, 9.991834e-01, 9.991834e-01, 9.991834e-01, 9.991834e-01,
      9.991834e-01, 9.991834e-01, 9.991834e-01, 9.991834e-01, 9.991834e-01,
      9.991834e-01, 9.991834e-01, 4.632662e-01, 9.991834e-01, 9.991834e-01,
      1.575885e-01, 1.383967e-02, 3.938014e-01, 7.600230e-01, 2.501973e-02,
      1.383967e-02, 3.052971e-01, 5.426136e-02, 4.626366e-01, 1.656419e-02,
      8.825610e-04, 1.252228e-05, 9.930759e-01, 3.394022e-01, 3.692284e-02,
      1.023581e-02, 3.974152e-02, 3.172920e-03, 8.992520e-01, 2.179486e-01}},
    {"Holm",
     {1.000000e+00, 1.000000e+00, 1.000000e+00, 1.000000e+00, 1.000000e+00,
      1.000000e+00, 1.000000e+00, 1.000000e+00, 1.000000e+00, 1.000000e+00,
      1.000000e+00, 1.000000e+00, 1.000000e+00, 1.000000e+00, 1.000000e+00,
      1.000000e+00, 1.000000e+00, 1.000000e+00, 1.000000e+00, 1.000000e+00,
      1.000000e+00, 1.000000e+00, 1.000000e+00, 1.000000e+00, 1.000000e+00,
      1.000000e+00, 1.000000e+00, 4.632662e-01, 1.000000e+00, 1.000000e+00,
      1.575885e-01, 1.395341e-02, 3.938014e-01, 7.600230e-01, 2.501973e-02,
      1.395341e-02, 3.052971e-01, 5.426136e-02, 4.626366e-01, 1.656419e-02,
      8.825610e-04, 1.252228e-05, 9.930759e-01, 3.394022e-01, 3.692284e-02,
      1.023581e-02, 3.974152e-02, 3.172920e-03, 8.992520e-01, 2.179486e-01}},
    {"Hommel",
     {9.991834e-01, 9.991834e-01, 9.991834e-01, 9.987624e-01, 9.991834e-01,
      9.991834e-01, 9.991834e-01, 9.991834e-01, 9.991834e-01, 9.991834e-01,
      9.991834e-01, 9.991834e-01, 9.991834e-01, 9.991834e-01, 9.991834e-01,
      9.991834e-01, 9.991834e-01, 9.991834e-01, 9.991834e-01, 9.595180e-01,
      9.991834e-01, 9.991834e-01, 9.991834e-01, 9.991834e-01, 9.991834e-01,
      9.991834e-01, 9.991834e-01, 4.351895e-01, 9.991834e-01, 9.766522e-01,
      1.414256e-01, 1.304340e-02, 3.530937e-01, 6.887709e-01, 2.385602e-02,
      1.322457e-02, 2.722920e-01, 5.426136e-02, 4.218158e-01, 1.581127e-02,
      8.825610e-04, 1.252228e-05, 8.743649e-01, 3.016908e-01, 3.516461e-02,
      9.582456e-03, 3.877222e-02, 3.172920e-03, 8.122276e-01, 1.950067e-01}},
    {"Sidak",
     {1.0000000000, 1.0000000000, 0.9946598274, 0.9914285749, 0.9999515274,
      1.0000000000, 0.9999999688, 1.0000000000, 1.0000000000, 1.0000000000,
      1.0000000000, 1.0000000000, 0.9999999995, 1.0000000000, 0.9999998801,
      1.0000000000, 1.0000000000, 1.0000000000, 0.9999999855, 0.9231179729,
      0.9999999956, 1.0000000000, 1.0000000000, 0.9999317605, 1.0000000000,
      0.9983109511, 1.0000000000, 0.5068253940, 1.0000000000, 0.9703301333,
      0.1832692440, 0.0150545753, 0.4320729669, 0.6993672225, 0.0286818157,
      0.0152621104, 0.3391808707, 0.0656206307, 0.4959194266, 0.0186503726,
      0.0009001752, 0.0000125222, 0.8142104886, 0.3772612062, 0.0430222116,
      0.0108312558, 0.0473319661, 0.0032997780, 0.7705015898, 0.2499384839}}})
--  {"Unknown",{1}}})

constant pValues = {4.533744e-01, 7.296024e-01, 9.936026e-02, 9.079658e-02, 1.801962e-01,
                    8.752257e-01, 2.922222e-01, 9.115421e-01, 4.355806e-01, 5.324867e-01,
                    4.926798e-01, 5.802978e-01, 3.485442e-01, 7.883130e-01, 2.729308e-01,
                    8.502518e-01, 4.268138e-01, 6.442008e-01, 3.030266e-01, 5.001555e-02,
                    3.194810e-01, 7.892933e-01, 9.991834e-01, 1.745691e-01, 9.037516e-01,
                    1.198578e-01, 3.966083e-01, 1.403837e-02, 7.328671e-01, 6.793476e-02,
                    4.040730e-03, 3.033349e-04, 1.125147e-02, 2.375072e-02, 5.818542e-04,
                    3.075482e-04, 8.251272e-03, 1.356534e-03, 1.360696e-02, 3.764588e-04,
                    1.801145e-05, 2.504456e-07, 3.310253e-02, 9.427839e-03, 8.791153e-04,
                    2.177831e-04, 9.693054e-04, 6.610250e-05, 2.900813e-02, 5.735490e-03}

if length(pValues)=0 or min(pValues)<0 or max(pValues)>1 then
    crash("p-values must be in range 0.0 to 1.0")
end if

for i=1 to length(types) do
    string ti = types[i]
    sequence res = adjust(pValues,ti)
    if res={} then
        printf(1,"\nSorry, do not know how to do %s correction.\n"&
                 "Perhaps you want one of these?:\n  %s\n",
                 {ti,join(types[1..$-1],"\n  ")})
        exit
    end if
--  printf(1,"%s\n",{ti})
--  res = correct_answers[i] -- (for easier comparison only)
--  pp(res,{pp_FltFmt,"%13.10f",pp_IntFmt,"%13.10f",pp_Maxlen,75,pp_Pause,0})
    atom error = sum(sq_abs(sq_sub(res,correct_answers[i])))
    printf(1,"%s has cumulative error of %g\n", {ti,error})
end for
```

{{out}}
Matches Kotlin (etc) when some of those lines just above are uncommented.

```txt

Benjamini-Hochberg has cumulative error of 8.03052e-7
Benjamini-Yekutieli has cumulative error of 3.64071e-7
Bonferroni has cumulative error of 6.5e-8
Hochberg has cumulative error of 2.7375e-7
Holm has cumulative error of 2.8095e-7
Hommel has cumulative error of 4.35302e-7
Sidak has cumulative error of 7.26897e-10

```



## Python

{{trans|Perl}}
''This work is based on R source code covered by the '''GPL''' license. It is thus a modified version, also covered by the GPL. See the [https://www.gnu.org/licenses/gpl-faq.html#GPLRequireSourcePostedPublic FAQ about GNU licenses]''.

```python
from __future__ import division
import sys

def pminf(array):
    x = 1
    pmin_list = []
    N = len(array)
    for index in range(N):
        if array[index] < x:
            pmin_list.insert(index, array[index])
        else:
            pmin_list.insert(index, x)
    return pmin_list
#end function

def cumminf(array):
    cummin = []
    cumulative_min = array[0]
    for p in array:
        if p < cumulative_min:
            cumulative_min = p
        cummin.append(cumulative_min)
    return cummin
#end

def cummaxf(array):
    cummax = []
    cumulative_max = array[0]
    for e in array:
        if e > cumulative_max:
            cumulative_max = e
        cummax.append(cumulative_max)
    return cummax
#end

def order(*args):
    if len(args) > 1:
        if args[1].lower() == 'false':# if ($string1 eq $string2) {
            return sorted(range(len(args[0])), key = lambda k: args[0][k])
        elif list(args[1].lower()) == list('true'):
            return sorted(range(len(args[0])), key = lambda k: args[0][k], reverse = True)
        else:
            print "%s isn't a recognized parameter" % args[1]
            sys.exit()
    elif len(args) == 1:
        return sorted(range(len(args[0])), key = lambda k: args[0][k])
#end

def p_adjust(*args):
    method = "bh"
    pvalues = args[0]
    if len(args) > 1:
        methods = {"bh", "fdr", "by", "holm", "hommel", "bonferroni", "hochberg"}
        metharg = arg[1].lower()
        if metharg in methods:
            method = metharg
    lp = len(pvalues)
    n = lp
    qvalues = []

    if method == 'hochberg':#already all lower case
        o = order(pvalues, 'TRUE')
        cummin_input = []
        for index in range(n):
            cummin_input.insert(index, (index+1)*pvalues[o[index]])
        cummin = cumminf(cummin_input)
        pmin = pminf(cummin)
        ro = order(o)
        qvalues = [pmin[i] for i in ro]
    elif method == 'bh':
        o = order(pvalues, 'TRUE')
        cummin_input = []
        for index in range(n):
            cummin_input.insert(index, (n/(n-index))* pvalues[o[index]])
        ro = order(o)
        cummin = cumminf(cummin_input)
        pmin = pminf(cummin)
        qvalues = [pmin[i] for i in ro]
    elif method == 'by':
        q = 0.0
        o = order(pvalues, 'TRUE')
        ro = order(o)
        for index in range(1, n+1):
            q += 1.0 / index;
        cummin_input = []
        for index in range(n):
            cummin_input.insert(index, q * (n/(n-index)) * pvalues[o[index]])
        cummin = cumminf(cummin_input)
        pmin = pminf(cummin)
        qvalues = [pmin[i] for i in ro]
    elif method == 'bonferroni':
        for index in range(n):
            q = pvalues[index] * n
            if (0 <= q) and (q < 1):
                qvalues.insert(index, q)
            elif q >= 1:
                qvalues.insert(index, 1)
            else:
                print '%g won\'t give a Bonferroni adjusted p' % q
                sys.exit()
    elif method == 'holm':
        o = order(pvalues)
        cummax_input = []
        for index in range(n):
            cummax_input.insert(index, (n - index) * pvalues[o[index]])
        ro = order(o)
        cummax = cummaxf(cummax_input)
        pmin = pminf(cummax)
        qvalues = [pmin[i] for i in ro]
    elif method == 'hommel':
        i = range(1,n+1)
        o = order(pvalues)
        p = [pvalues[index] for index in o]
        ro = order(o)
        pa = []
        q = []
        smin = n*p[0]
        for index in range(n):
            temp = n*p[index] / (index + 1)
            if temp < smin:
                smin = temp
        for index in range(n):
            pa.insert(index, smin)
            q.insert(index, smin)
        for j in range(n-1,1,-1):
            ij = range(1,n-j+2)
            for x in range(len(ij)):
                ij[x] -= 1
            I2_LENGTH = j - 1
            i2 = []
            for index in range(I2_LENGTH+1):
                i2.insert(index, n - j + 2 + index - 1)
            q1 = j * p[i2[0]] / 2.0
            for index in range(1,I2_LENGTH):
                TEMP_Q1 = j * p[i2[index]] / (2.0 + index)
                if TEMP_Q1 < q1:
                    q1 = TEMP_Q1
            for index in range(n - j + 1):
                q[ij[index]] = min(j * p[ij[index]], q1)
            for index in range(I2_LENGTH):
                q[i2[index]] = q[n-j]
            for index in range(n):
                if pa[index] < q[index]:
                    pa[index] = q[index]
            qvalues = [pa[index] for index in ro]
    else:
        print "method %s isn't defined." % method
        sys.exit()
    return qvalues

pvalues = [4.533744e-01, 7.296024e-01, 9.936026e-02, 9.079658e-02, 1.801962e-01,
8.752257e-01, 2.922222e-01, 9.115421e-01, 4.355806e-01, 5.324867e-01,
4.926798e-01, 5.802978e-01, 3.485442e-01, 7.883130e-01, 2.729308e-01,
8.502518e-01, 4.268138e-01, 6.442008e-01, 3.030266e-01, 5.001555e-02,
3.194810e-01, 7.892933e-01, 9.991834e-01, 1.745691e-01, 9.037516e-01,
1.198578e-01, 3.966083e-01, 1.403837e-02, 7.328671e-01, 6.793476e-02,
4.040730e-03, 3.033349e-04, 1.125147e-02, 2.375072e-02, 5.818542e-04,
3.075482e-04, 8.251272e-03, 1.356534e-03, 1.360696e-02, 3.764588e-04,
1.801145e-05, 2.504456e-07, 3.310253e-02, 9.427839e-03, 8.791153e-04,
2.177831e-04, 9.693054e-04, 6.610250e-05, 2.900813e-02, 5.735490e-03]

correct_answers = {}

correct_answers['bh'] = [6.126681e-01, 8.521710e-01, 1.987205e-01, 1.891595e-01, 3.217789e-01,
9.301450e-01, 4.870370e-01, 9.301450e-01, 6.049731e-01, 6.826753e-01,
6.482629e-01, 7.253722e-01, 5.280973e-01, 8.769926e-01, 4.705703e-01,
9.241867e-01, 6.049731e-01, 7.856107e-01, 4.887526e-01, 1.136717e-01,
4.991891e-01, 8.769926e-01, 9.991834e-01, 3.217789e-01, 9.301450e-01,
2.304958e-01, 5.832475e-01, 3.899547e-02, 8.521710e-01, 1.476843e-01,
1.683638e-02, 2.562902e-03, 3.516084e-02, 6.250189e-02, 3.636589e-03,
2.562902e-03, 2.946883e-02, 6.166064e-03, 3.899547e-02, 2.688991e-03,
4.502862e-04, 1.252228e-05, 7.881555e-02, 3.142613e-02, 4.846527e-03,
2.562902e-03, 4.846527e-03, 1.101708e-03, 7.252032e-02, 2.205958e-02]

correct_answers['by'] = [1.000000e+00, 1.000000e+00, 8.940844e-01, 8.510676e-01, 1.000000e+00,
1.000000e+00, 1.000000e+00, 1.000000e+00, 1.000000e+00, 1.000000e+00,
1.000000e+00, 1.000000e+00, 1.000000e+00, 1.000000e+00, 1.000000e+00,
1.000000e+00, 1.000000e+00, 1.000000e+00, 1.000000e+00, 5.114323e-01,
1.000000e+00, 1.000000e+00, 1.000000e+00, 1.000000e+00, 1.000000e+00,
1.000000e+00, 1.000000e+00, 1.754486e-01, 1.000000e+00, 6.644618e-01,
7.575031e-02, 1.153102e-02, 1.581959e-01, 2.812089e-01, 1.636176e-02,
1.153102e-02, 1.325863e-01, 2.774239e-02, 1.754486e-01, 1.209832e-02,
2.025930e-03, 5.634031e-05, 3.546073e-01, 1.413926e-01, 2.180552e-02,
1.153102e-02, 2.180552e-02, 4.956812e-03, 3.262838e-01, 9.925057e-02]

correct_answers['bonferroni'] = [1.000000e+00, 1.000000e+00, 1.000000e+00, 1.000000e+00, 1.000000e+00,
1.000000e+00, 1.000000e+00, 1.000000e+00, 1.000000e+00, 1.000000e+00,
1.000000e+00, 1.000000e+00, 1.000000e+00, 1.000000e+00, 1.000000e+00,
1.000000e+00, 1.000000e+00, 1.000000e+00, 1.000000e+00, 1.000000e+00,
1.000000e+00, 1.000000e+00, 1.000000e+00, 1.000000e+00, 1.000000e+00,
1.000000e+00, 1.000000e+00, 7.019185e-01, 1.000000e+00, 1.000000e+00,
2.020365e-01, 1.516674e-02, 5.625735e-01, 1.000000e+00, 2.909271e-02,
1.537741e-02, 4.125636e-01, 6.782670e-02, 6.803480e-01, 1.882294e-02,
9.005725e-04, 1.252228e-05, 1.000000e+00, 4.713920e-01, 4.395577e-02,
1.088915e-02, 4.846527e-02, 3.305125e-03, 1.000000e+00, 2.867745e-01]

correct_answers['hochberg'] = [9.991834e-01, 9.991834e-01, 9.991834e-01, 9.991834e-01, 9.991834e-01,
9.991834e-01, 9.991834e-01, 9.991834e-01, 9.991834e-01, 9.991834e-01,
9.991834e-01, 9.991834e-01, 9.991834e-01, 9.991834e-01, 9.991834e-01,
9.991834e-01, 9.991834e-01, 9.991834e-01, 9.991834e-01, 9.991834e-01,
9.991834e-01, 9.991834e-01, 9.991834e-01, 9.991834e-01, 9.991834e-01,
9.991834e-01, 9.991834e-01, 4.632662e-01, 9.991834e-01, 9.991834e-01,
1.575885e-01, 1.383967e-02, 3.938014e-01, 7.600230e-01, 2.501973e-02,
1.383967e-02, 3.052971e-01, 5.426136e-02, 4.626366e-01, 1.656419e-02,
8.825610e-04, 1.252228e-05, 9.930759e-01, 3.394022e-01, 3.692284e-02,
1.023581e-02, 3.974152e-02, 3.172920e-03, 8.992520e-01, 2.179486e-01]

correct_answers['holm'] = [1.000000e+00, 1.000000e+00, 1.000000e+00, 1.000000e+00, 1.000000e+00,
1.000000e+00, 1.000000e+00, 1.000000e+00, 1.000000e+00, 1.000000e+00,
1.000000e+00, 1.000000e+00, 1.000000e+00, 1.000000e+00, 1.000000e+00,
1.000000e+00, 1.000000e+00, 1.000000e+00, 1.000000e+00, 1.000000e+00,
1.000000e+00, 1.000000e+00, 1.000000e+00, 1.000000e+00, 1.000000e+00,
1.000000e+00, 1.000000e+00, 4.632662e-01, 1.000000e+00, 1.000000e+00,
1.575885e-01, 1.395341e-02, 3.938014e-01, 7.600230e-01, 2.501973e-02,
1.395341e-02, 3.052971e-01, 5.426136e-02, 4.626366e-01, 1.656419e-02,
8.825610e-04, 1.252228e-05, 9.930759e-01, 3.394022e-01, 3.692284e-02,
1.023581e-02, 3.974152e-02, 3.172920e-03, 8.992520e-01, 2.179486e-01]

correct_answers['hommel'] = [9.991834e-01, 9.991834e-01, 9.991834e-01, 9.987624e-01, 9.991834e-01,
9.991834e-01, 9.991834e-01, 9.991834e-01, 9.991834e-01, 9.991834e-01,
9.991834e-01, 9.991834e-01, 9.991834e-01, 9.991834e-01, 9.991834e-01,
9.991834e-01, 9.991834e-01, 9.991834e-01, 9.991834e-01, 9.595180e-01,
9.991834e-01, 9.991834e-01, 9.991834e-01, 9.991834e-01, 9.991834e-01,
9.991834e-01, 9.991834e-01, 4.351895e-01, 9.991834e-01, 9.766522e-01,
1.414256e-01, 1.304340e-02, 3.530937e-01, 6.887709e-01, 2.385602e-02,
1.322457e-02, 2.722920e-01, 5.426136e-02, 4.218158e-01, 1.581127e-02,
8.825610e-04, 1.252228e-05, 8.743649e-01, 3.016908e-01, 3.516461e-02,
9.582456e-03, 3.877222e-02, 3.172920e-03, 8.122276e-01, 1.950067e-01]

for key in correct_answers.keys():
    error = 0.0
    q = p_adjust(pvalues, key)
    for i in range(len(q)):
        error += abs(q[i] - correct_answers[key][i])
    print '%s error = %g' % (key.upper(), error)

```


{{out}}


```txt

BONFERRONI error = 6.5e-08
BH error = 8.03053e-07
HOLM error = 2.8095e-07
HOMMEL error = 4.35302e-07
HOCHBERG error = 2.7375e-07
BY error = 3.64072e-07
```



## R

The '''p.adjust''' function is built-in, see [https://stat.ethz.ch/R-manual/R-devel/library/stats/html/p.adjust.html R manual].


```R
p <- c(4.533744e-01, 7.296024e-01, 9.936026e-02, 9.079658e-02, 1.801962e-01,
       8.752257e-01, 2.922222e-01, 9.115421e-01, 4.355806e-01, 5.324867e-01,
       4.926798e-01, 5.802978e-01, 3.485442e-01, 7.883130e-01, 2.729308e-01,
       8.502518e-01, 4.268138e-01, 6.442008e-01, 3.030266e-01, 5.001555e-02,
       3.194810e-01, 7.892933e-01, 9.991834e-01, 1.745691e-01, 9.037516e-01,
       1.198578e-01, 3.966083e-01, 1.403837e-02, 7.328671e-01, 6.793476e-02,
       4.040730e-03, 3.033349e-04, 1.125147e-02, 2.375072e-02, 5.818542e-04,
       3.075482e-04, 8.251272e-03, 1.356534e-03, 1.360696e-02, 3.764588e-04,
       1.801145e-05, 2.504456e-07, 3.310253e-02, 9.427839e-03, 8.791153e-04,
       2.177831e-04, 9.693054e-04, 6.610250e-05, 2.900813e-02, 5.735490e-03)

p.adjust(p, method = 'BH')
print("Benjamini-Hochberg")
writeLines("\n")

p.adjust(p, method = 'BY')
print("Benjamini & Yekutieli")
writeLines("\n")

p.adjust(p, method = 'bonferroni')
print("Bonferroni")
writeLines("\n")

p.adjust(p, method = 'hochberg')
print("Hochberg")
writeLines("\n");

p.adjust(p, method = 'hommel')
writeLines("Hommel\n")
```


{{out}}

```txt

 [1] 6.126681e-01 8.521710e-01 1.987205e-01 1.891595e-01 3.217789e-01
 [6] 9.301450e-01 4.870370e-01 9.301450e-01 6.049731e-01 6.826753e-01
[11] 6.482629e-01 7.253722e-01 5.280973e-01 8.769926e-01 4.705703e-01
[16] 9.241867e-01 6.049731e-01 7.856107e-01 4.887526e-01 1.136717e-01
[21] 4.991891e-01 8.769926e-01 9.991834e-01 3.217789e-01 9.301450e-01
[26] 2.304958e-01 5.832475e-01 3.899547e-02 8.521710e-01 1.476843e-01
[31] 1.683638e-02 2.562902e-03 3.516084e-02 6.250189e-02 3.636589e-03
[36] 2.562902e-03 2.946883e-02 6.166064e-03 3.899547e-02 2.688991e-03
[41] 4.502862e-04 1.252228e-05 7.881555e-02 3.142613e-02 4.846527e-03
[46] 2.562902e-03 4.846527e-03 1.101708e-03 7.252032e-02 2.205958e-02
[1] "Benjamini-Hochberg"


 [1] 1.000000e+00 1.000000e+00 8.940844e-01 8.510676e-01 1.000000e+00
 [6] 1.000000e+00 1.000000e+00 1.000000e+00 1.000000e+00 1.000000e+00
[11] 1.000000e+00 1.000000e+00 1.000000e+00 1.000000e+00 1.000000e+00
[16] 1.000000e+00 1.000000e+00 1.000000e+00 1.000000e+00 5.114323e-01
[21] 1.000000e+00 1.000000e+00 1.000000e+00 1.000000e+00 1.000000e+00
[26] 1.000000e+00 1.000000e+00 1.754486e-01 1.000000e+00 6.644618e-01
[31] 7.575031e-02 1.153102e-02 1.581959e-01 2.812089e-01 1.636176e-02
[36] 1.153102e-02 1.325863e-01 2.774239e-02 1.754486e-01 1.209832e-02
[41] 2.025930e-03 5.634031e-05 3.546073e-01 1.413926e-01 2.180552e-02
[46] 1.153102e-02 2.180552e-02 4.956812e-03 3.262838e-01 9.925057e-02
[1] "Benjamini & Yekutieli"


 [1] 1.000000e+00 1.000000e+00 1.000000e+00 1.000000e+00 1.000000e+00
 [6] 1.000000e+00 1.000000e+00 1.000000e+00 1.000000e+00 1.000000e+00
[11] 1.000000e+00 1.000000e+00 1.000000e+00 1.000000e+00 1.000000e+00
[16] 1.000000e+00 1.000000e+00 1.000000e+00 1.000000e+00 1.000000e+00
[21] 1.000000e+00 1.000000e+00 1.000000e+00 1.000000e+00 1.000000e+00
[26] 1.000000e+00 1.000000e+00 7.019185e-01 1.000000e+00 1.000000e+00
[31] 2.020365e-01 1.516674e-02 5.625735e-01 1.000000e+00 2.909271e-02
[36] 1.537741e-02 4.125636e-01 6.782670e-02 6.803480e-01 1.882294e-02
[41] 9.005725e-04 1.252228e-05 1.000000e+00 4.713920e-01 4.395577e-02
[46] 1.088915e-02 4.846527e-02 3.305125e-03 1.000000e+00 2.867745e-01
[1] "Bonferroni"


 [1] 9.991834e-01 9.991834e-01 9.991834e-01 9.991834e-01 9.991834e-01
 [6] 9.991834e-01 9.991834e-01 9.991834e-01 9.991834e-01 9.991834e-01
[11] 9.991834e-01 9.991834e-01 9.991834e-01 9.991834e-01 9.991834e-01
[16] 9.991834e-01 9.991834e-01 9.991834e-01 9.991834e-01 9.991834e-01
[21] 9.991834e-01 9.991834e-01 9.991834e-01 9.991834e-01 9.991834e-01
[26] 9.991834e-01 9.991834e-01 4.632662e-01 9.991834e-01 9.991834e-01
[31] 1.575885e-01 1.383967e-02 3.938014e-01 7.600230e-01 2.501973e-02
[36] 1.383967e-02 3.052971e-01 5.426136e-02 4.626366e-01 1.656419e-02
[41] 8.825610e-04 1.252228e-05 9.930759e-01 3.394022e-01 3.692284e-02
[46] 1.023581e-02 3.974152e-02 3.172920e-03 8.992520e-01 2.179486e-01
[1] "Hochberg"


 [1] 9.991834e-01 9.991834e-01 9.991834e-01 9.987624e-01 9.991834e-01
 [6] 9.991834e-01 9.991834e-01 9.991834e-01 9.991834e-01 9.991834e-01
[11] 9.991834e-01 9.991834e-01 9.991834e-01 9.991834e-01 9.991834e-01
[16] 9.991834e-01 9.991834e-01 9.991834e-01 9.991834e-01 9.595180e-01
[21] 9.991834e-01 9.991834e-01 9.991834e-01 9.991834e-01 9.991834e-01
[26] 9.991834e-01 9.991834e-01 4.351895e-01 9.991834e-01 9.766522e-01
[31] 1.414256e-01 1.304340e-02 3.530937e-01 6.887709e-01 2.385602e-02
[36] 1.322457e-02 2.722920e-01 5.426136e-02 4.218158e-01 1.581127e-02
[41] 8.825610e-04 1.252228e-05 8.743649e-01 3.016908e-01 3.516461e-02
[46] 9.582456e-03 3.877222e-02 3.172920e-03 8.122276e-01 1.950067e-01
Hommel
```



## SAS



```sas
data pvalues;
input raw_p @@;
cards;
4.533744e-01 7.296024e-01 9.936026e-02 9.079658e-02 1.801962e-01
8.752257e-01 2.922222e-01 9.115421e-01 4.355806e-01 5.324867e-01
4.926798e-01 5.802978e-01 3.485442e-01 7.883130e-01 2.729308e-01
8.502518e-01 4.268138e-01 6.442008e-01 3.030266e-01 5.001555e-02
3.194810e-01 7.892933e-01 9.991834e-01 1.745691e-01 9.037516e-01
1.198578e-01 3.966083e-01 1.403837e-02 7.328671e-01 6.793476e-02
4.040730e-03 3.033349e-04 1.125147e-02 2.375072e-02 5.818542e-04
3.075482e-04 8.251272e-03 1.356534e-03 1.360696e-02 3.764588e-04
1.801145e-05 2.504456e-07 3.310253e-02 9.427839e-03 8.791153e-04
2.177831e-04 9.693054e-04 6.610250e-05 2.900813e-02 5.735490e-03
;
run;

proc multtest pdata=pvalues bon sid hom hoc holm;
run;
```


'''output'''


```txt
                                      The Multtest Procedure

                                  P-Value Adjustment Information

                            P-Value Adjustment     Bonferroni
                            P-Value Adjustment     Stepdown Bonferroni
                            P-Value Adjustment     Sidak
                            P-Value Adjustment     Hochberg
                            P-Value Adjustment     Hommel


                                             p-Values

                                            Stepdown
      Test           Raw    Bonferroni    Bonferroni         Sidak      Hochberg        Hommel

         1        0.4534        1.0000        1.0000        1.0000        0.9992        0.9992
         2        0.7296        1.0000        1.0000        1.0000        0.9992        0.9992
         3        0.0994        1.0000        1.0000        0.9947        0.9992        0.9992
         4        0.0908        1.0000        1.0000        0.9914        0.9992        0.9988
         5        0.1802        1.0000        1.0000        1.0000        0.9992        0.9992
         6        0.8752        1.0000        1.0000        1.0000        0.9992        0.9992
         7        0.2922        1.0000        1.0000        1.0000        0.9992        0.9992
         8        0.9115        1.0000        1.0000        1.0000        0.9992        0.9992
         9        0.4356        1.0000        1.0000        1.0000        0.9992        0.9992
        10        0.5325        1.0000        1.0000        1.0000        0.9992        0.9992
        11        0.4927        1.0000        1.0000        1.0000        0.9992        0.9992
        12        0.5803        1.0000        1.0000        1.0000        0.9992        0.9992
        13        0.3485        1.0000        1.0000        1.0000        0.9992        0.9992
        14        0.7883        1.0000        1.0000        1.0000        0.9992        0.9992
        15        0.2729        1.0000        1.0000        1.0000        0.9992        0.9992
        16        0.8503        1.0000        1.0000        1.0000        0.9992        0.9992
        17        0.4268        1.0000        1.0000        1.0000        0.9992        0.9992
        18        0.6442        1.0000        1.0000        1.0000        0.9992        0.9992
        19        0.3030        1.0000        1.0000        1.0000        0.9992        0.9992
        20        0.0500        1.0000        1.0000        0.9231        0.9992        0.9595
        21        0.3195        1.0000        1.0000        1.0000        0.9992        0.9992
        22        0.7893        1.0000        1.0000        1.0000        0.9992        0.9992
        23        0.9992        1.0000        1.0000        1.0000        0.9992        0.9992
        24        0.1746        1.0000        1.0000        0.9999        0.9992        0.9992
        25        0.9038        1.0000        1.0000        1.0000        0.9992        0.9992
        26        0.1199        1.0000        1.0000        0.9983        0.9992        0.9992
        27        0.3966        1.0000        1.0000        1.0000        0.9992        0.9992
        28        0.0140        0.7019        0.4633        0.5068        0.4633        0.4352
        29        0.7329        1.0000        1.0000        1.0000        0.9992        0.9992
        30        0.0679        1.0000        1.0000        0.9703        0.9992        0.9767
        31        0.0040        0.2020        0.1576        0.1833        0.1576        0.1414
        32        0.0003        0.0152        0.0140        0.0151        0.0138        0.0130
        33        0.0113        0.5626        0.3938        0.4321        0.3938        0.3531
        34        0.0238        1.0000        0.7600        0.6994        0.7600        0.6888
        35        0.0006        0.0291        0.0250        0.0287        0.0250        0.0239
        36        0.0003        0.0154        0.0140        0.0153        0.0138        0.0132
        37        0.0083        0.4126        0.3053        0.3392        0.3053        0.2723
        38        0.0014        0.0678        0.0543        0.0656        0.0543        0.0543
        39        0.0136        0.6803        0.4626        0.4959        0.4626        0.4218
        40        0.0004        0.0188        0.0166        0.0187        0.0166        0.0158
        41        <.0001        0.0009        0.0009        0.0009        0.0009        0.0009
        42        <.0001        <.0001        <.0001        <.0001        <.0001        <.0001
        43        0.0331        1.0000        0.9931        0.8142        0.9931        0.8744
        44        0.0094        0.4714        0.3394        0.3773        0.3394        0.3017
        45        0.0009        0.0440        0.0369        0.0430        0.0369        0.0352
        46        0.0002        0.0109        0.0102        0.0108        0.0102        0.0096
        47        0.0010        0.0485        0.0397        0.0473        0.0397        0.0388
        48        <.0001        0.0033        0.0032        0.0033        0.0032        0.0032
        49        0.0290        1.0000        0.8993        0.7705        0.8993        0.8122
        50        0.0057        0.2868        0.2179        0.2499        0.2179        0.1950
```



## Stata


The '''[https://econpapers.repec.org/software/bocbocode/s457100.htm qqvalue]''' package on SSC provides the equivalent of the R function '''p.adjust'''.

First, install the package with:


```stata>ssc install qqvalue</lang


Given a dataset containing the p-values in a variable, the qqvalue command generates another variable with the adjusted p-values. Here is an example showing the result with all implemented methods:


```stata
clear

#delimit ;
input p;
4.533744e-01;7.296024e-01;9.936026e-02;9.079658e-02;1.801962e-01;
8.752257e-01;2.922222e-01;9.115421e-01;4.355806e-01;5.324867e-01;
4.926798e-01;5.802978e-01;3.485442e-01;7.883130e-01;2.729308e-01;
8.502518e-01;4.268138e-01;6.442008e-01;3.030266e-01;5.001555e-02;
3.194810e-01;7.892933e-01;9.991834e-01;1.745691e-01;9.037516e-01;
1.198578e-01;3.966083e-01;1.403837e-02;7.328671e-01;6.793476e-02;
4.040730e-03;3.033349e-04;1.125147e-02;2.375072e-02;5.818542e-04;
3.075482e-04;8.251272e-03;1.356534e-03;1.360696e-02;3.764588e-04;
1.801145e-05;2.504456e-07;3.310253e-02;9.427839e-03;8.791153e-04;
2.177831e-04;9.693054e-04;6.610250e-05;2.900813e-02;5.735490e-03;
end;
#delimit cr

loc meth bonferroni sidak holm holland hochberg simes yekutieli
foreach m in `meth' {
	qqvalue p, method(`m') qvalue(`m')
}

list
```


'''output'''


```txt
     +-----------------------------------------------------------------------------------------------+
     |         p   bonferr~i       sidak        holm     holland    hochberg       simes   yekutieli |
     |-----------------------------------------------------------------------------------------------|
  1. |  .4533744           1           1           1   .99986425    .9991834   .61266811           1 |
  2. |  .7296024           1           1           1   .99999227    .9991834   .85217105           1 |
  3. | .09936026           1   .99465983           1   .93418441    .9991834   .19872052   .89408442 |
  4. | .09079658           1   .99142857           1   .92346702    .9991834   .18915954   .85106762 |
  5. |  .1801962           1   .99995153           1   .98999223    .9991834   .32177893           1 |
     |-----------------------------------------------------------------------------------------------|
  6. |  .8752257           1           1           1   .99999227    .9991834     .930145           1 |
  7. |  .2922222           1   .99999997           1   .99929557    .9991834     .487037           1 |
  8. |  .9115421           1           1           1   .99999227    .9991834     .930145           1 |
  9. |  .4355806           1           1           1   .99986425    .9991834   .60497306           1 |
 10. |  .5324867           1           1           1   .99989097    .9991834   .68267526           1 |
     |-----------------------------------------------------------------------------------------------|
 11. |  .4926798           1           1           1   .99986425    .9991834   .64826289           1 |
 12. |  .5802978           1           1           1   .99992882    .9991834   .72537225           1 |
 13. |  .3485442           1           1           1   .99955339    .9991834   .52809727           1 |
 14. |   .788313           1           1           1   .99999227    .9991834   .87699256           1 |
 15. |  .2729308           1   .99999988           1   .99909912    .9991834   .47057034           1 |
     |-----------------------------------------------------------------------------------------------|
 16. |  .8502518           1           1           1   .99999227    .9991834   .92418674           1 |
 17. |  .4268138           1           1           1   .99986425    .9991834   .60497306           1 |
 18. |  .6442008           1           1           1   .99996749    .9991834   .78561073           1 |
 19. |  .3030266           1   .99999999           1   .99929557    .9991834   .48875258           1 |
 20. | .05001555           1   .92311797           1   .77417168    .9991834    .1136717   .51143234 |
     |-----------------------------------------------------------------------------------------------|
 21. |   .319481           1           1           1   .99933325    .9991834   .49918906           1 |
 22. |  .7892933           1           1           1   .99999227    .9991834   .87699256           1 |
 23. |  .9991834           1           1           1   .99999227    .9991834    .9991834           1 |
 24. |  .1745691           1   .99993176           1   .98999223    .9991834   .32177893           1 |
 25. |  .9037516           1           1           1   .99999227    .9991834     .930145           1 |
     |-----------------------------------------------------------------------------------------------|
 26. |  .1198578           1   .99831095           1   .95890196    .9991834   .23049577           1 |
 27. |  .3966083           1           1           1   .99981371    .9991834    .5832475           1 |
 28. | .01403837    .7019185   .50682539   .46326621   .37283695   .46326621   .03899547   .17544864 |
 29. |  .7328671           1           1           1   .99999227    .9991834   .85217105           1 |
 30. | .06793476           1   .97033013           1   .86052488    .9991834   .14768426   .66446181 |
     |-----------------------------------------------------------------------------------------------|
 31. | .00404073    .2020365   .18326924   .15758847   .14607142   .15758847   .01683638   .07575031 |
 32. | .00030333   .01516674   .01505458   .01395341    .0138586   .01383967    .0025629   .01153102 |
 33. | .01125147    .5625735   .43207297   .39380145   .32701594   .39380145   .03516084   .15819586 |
 34. | .02375072           1   .69936722   .76002304   .53661363   .76002304   .06250189   .28120886 |
 35. | .00058185   .02909271   .02868182   .02501973   .02471643   .02501973   .00363659   .01636176 |
     |-----------------------------------------------------------------------------------------------|
 36. | .00030755   .01537741   .01526211   .01395341    .0138586   .01383967    .0025629   .01153102 |
 37. | .00825127    .4125636   .33918087   .30529706   .26402828   .30529706   .02946883   .13258631 |
 38. | .00135653    .0678267   .06562063   .05426136   .05285037   .05426136   .00616606   .02774239 |
 39. | .01360696     .680348   .49591943   .46263664   .37237538   .46263664   .03899547   .17544864 |
 40. | .00037646   .01882294   .01865037   .01656419   .01643082   .01656419   .00268899   .01209832 |
     |-----------------------------------------------------------------------------------------------|
 41. | .00001801   .00090057   .00090018   .00088256   .00088218   .00088256   .00045029   .00202593 |
 42. | 2.504e-07   .00001252   .00001252   .00001252   .00001252   .00001252   .00001252   .00005634 |
 43. | .03310253           1   .81421049    .9930759   .63573897    .9930759   .07881555   .35460733 |
 44. | .00942784   .47139195   .37726121    .3394022    .2889498    .3394022   .03142613   .14139261 |
 45. | .00087912   .04395577   .04302221   .03692284   .03626516   .03692284   .00484653   .02180552 |
     |-----------------------------------------------------------------------------------------------|
 46. | .00021778   .01088915   .01083126   .01023581    .0101847   .01023581    .0025629   .01153102 |
 47. | .00096931   .04846527   .04733197   .03974152   .03898071   .03974152   .00484653   .02180552 |
 48. |  .0000661   .00330513   .00329978   .00317292     .003168   .00317292   .00110171   .00495681 |
 49. | .02900813           1   .77050159   .89925203   .59850199   .89925203   .07252032   .32628383 |
 50. | .00573549    .2867745   .24993848   .21794862   .19633763   .21794862   .02205958   .09925057 |
     +-----------------------------------------------------------------------------------------------+
```



## zkl

{{trans|C}}
''This work is based on R source code covered by the '''GPL''' license. It is thus a modified version, also covered by the GPL. See the [https://www.gnu.org/licenses/gpl-faq.html#GPLRequireSourcePostedPublic FAQ about GNU licenses]''.


```zkl
fcn bh(pvalues){	// Benjamini-Hochberg
   psz,pszf := pvalues.len(), psz.toFloat();
   n_i      := psz.pump(List,'wrap(n){ pszf/(psz - n) }); # N/(N-0),N/(N-1),..
   o,ro     := order(pvalues,True),order(o,False); # sort pvalues, sort indices
   in	    := psz.pump(List,'wrap(n){ n_i[n]*pvalues[o[n]] });
   pmin     := cummin(in).apply((1.0).min); # (min(1,c[0]),min(1,c[1]),...)
   ro.apply(pmin.get);		# (pmin[ro[0]],pmin[ro[1]],...)
}

fcn by(pvalues){	// Benjamini & Yekutieli
   psz,pszf := pvalues.len(), psz.toFloat();
   o,ro     := order(pvalues,True),order(o,False); # sort pvalues, sort indices
   n_i      := psz.pump(List,'wrap(n){ pszf/(psz - n) }); # N/(N-0),N/(N-1),..
   q	    := [1..psz].reduce(fcn(q,n){ q+=1.0/n },0.0);
   in	    := psz.pump(List,'wrap(n){ q * n_i[n] * pvalues[o[n]] });
   cummin(in).apply((1.0).min) : ro.apply(_.get);
}

fcn hochberg(pvalues){
   psz,pszf := pvalues.len(), psz.toFloat();
   o,ro     := order(pvalues,True),order(o,False); # sort pvalues, sort indices
   n_i      := psz.pump(List,'wrap(n){ pszf/(psz - n) }); # N/(N-0),N/(N-1),..
   in	    := psz.pump(List,'wrap(n){ pvalues[o[n]]*(n + 1) });
   cummin(in).apply((1.0).min) : ro.apply(_.get);
}

fcn cummin(pvalues){  // R's cumulative minima --> list of mins
   out,m := List.createLong(pvalues.len()), pvalues[0];
   foreach pv in (pvalues){ out.append(m=m.min(pv)) }
   out
}
fcn order(list,downUp){  // True==increasing, --> List(int) sorted indices
   f:=(downUp) and fcn(a,b){ a[1]>b[1] } or fcn(a,b){ a[1]<b[1] };
   [0..].zip(list).pump(List()).sort(f).pump(List,T("get",0))
}

fcn bonferroni(pvalues){  // -->List
   sz,r := pvalues.len(),List();
   foreach pv in (pvalues){
      b:=pv*sz;
      if(b>=1.0) r.append(1.0);
      else if(0.0<=b<1.0) r.append(b);
      else throw(Exception.ValueError(
	 "%g is outside of the interval I planned.".fmt(b)));
   }
   r
}

fcn hommel(pvalues){
   psz,indices := pvalues.len(), [1..psz].walk();	// 1,2,3,4...
   o,ro        := order(pvalues,False),order(o,False); # sort pvalues, sort indices
   p           := o.apply('wrap(n){ pvalues[n] }).copy(); // pvalues[*o]
   npi         := [1..].zip(p).apply('wrap([(n,p)]){ p*psz/n });
   min	       := (0.0).min(npi);	 // min value in npi
   pa,q	       := List.createLong(psz,min), pa.copy(); #(min,min,,,)
   foreach j in ([psz - 1..2,-1]){
      ij:=[0..psz - j].walk();
      i2:=(j - 1).pump(List,'+(psz - j + 1));
      q1:=(0.0).min((j-1).pump(List,'wrap(n){ p[i2[n]]*j/(2 + n) }));
      foreach i in (psz - j + 1){ q[ij[i]] = q1.min(p[ij[i]]*j) }
      foreach i in (j - 1){ q[i2[i]] = q[psz - j] }
      foreach i in (psz){ pa[i] = pa[i].max(q[i]) }
   }
   psz.pump(List,'wrap(n){ pa[ro[n]] }); // Hommel q-values
}
```


```zkl
pvalues:=T(
   4.533744e-01, 7.296024e-01, 9.936026e-02, 9.079658e-02, 1.801962e-01,
   8.752257e-01, 2.922222e-01, 9.115421e-01, 4.355806e-01, 5.324867e-01,
   4.926798e-01, 5.802978e-01, 3.485442e-01, 7.883130e-01, 2.729308e-01,
   8.502518e-01, 4.268138e-01, 6.442008e-01, 3.030266e-01, 5.001555e-02,
   3.194810e-01, 7.892933e-01, 9.991834e-01, 1.745691e-01, 9.037516e-01,
   1.198578e-01, 3.966083e-01, 1.403837e-02, 7.328671e-01, 6.793476e-02,
   4.040730e-03, 3.033349e-04, 1.125147e-02, 2.375072e-02, 5.818542e-04,
   3.075482e-04, 8.251272e-03, 1.356534e-03, 1.360696e-02, 3.764588e-04,
   1.801145e-05, 2.504456e-07, 3.310253e-02, 9.427839e-03, 8.791153e-04,
   2.177831e-04, 9.693054e-04, 6.610250e-05, 2.900813e-02, 5.735490e-03);

bh(pvalues)	    : format(_,"\nBenjamini-Hochberg");
by(pvalues)	    : format(_,"\nBenjamini & Yekutieli");
bonferroni(pvalues) : format(_,"\nBonferroni");
hochberg(pvalues)   : format(_,"\nHochberg");
hommel(pvalues)	    : format(_,"\nHommel");

fcn format(list,title){
   print(title,":");
   foreach n in ([1..list.len(),5]){
      print("\n[%2d]:".fmt(n));
      foreach x in (list[n-1,5]){ print(" %.6e".fmt(x)) }
   }
   println();
}
```

{{out}}
<pre style="height:45ex">
Benjamini-Hochberg:
[ 1]: 6.126681e-01 8.521710e-01 1.987205e-01 1.891595e-01 3.217789e-01
[ 6]: 9.301450e-01 4.870370e-01 9.301450e-01 6.049731e-01 6.826753e-01
[11]: 6.482629e-01 7.253722e-01 5.280973e-01 8.769926e-01 4.705703e-01
[16]: 9.241867e-01 6.049731e-01 7.856107e-01 4.887526e-01 1.136717e-01
[21]: 4.991891e-01 8.769926e-01 9.991834e-01 3.217789e-01 9.301450e-01
[26]: 2.304958e-01 5.832475e-01 3.899547e-02 8.521710e-01 1.476843e-01
[31]: 1.683638e-02 2.562902e-03 3.516084e-02 6.250189e-02 3.636589e-03
[36]: 2.562902e-03 2.946883e-02 6.166064e-03 3.899547e-02 2.688991e-03
[41]: 4.502862e-04 1.252228e-05 7.881555e-02 3.142613e-02 4.846527e-03
[46]: 2.562902e-03 4.846527e-03 1.101708e-03 7.252032e-02 2.205958e-02

Benjamini & Yekutieli:
[ 1]: 1.000000e+00 1.000000e+00 8.940844e-01 8.510676e-01 1.000000e+00
[ 6]: 1.000000e+00 1.000000e+00 1.000000e+00 1.000000e+00 1.000000e+00
[11]: 1.000000e+00 1.000000e+00 1.000000e+00 1.000000e+00 1.000000e+00
[16]: 1.000000e+00 1.000000e+00 1.000000e+00 1.000000e+00 5.114323e-01
[21]: 1.000000e+00 1.000000e+00 1.000000e+00 1.000000e+00 1.000000e+00
[26]: 1.000000e+00 1.000000e+00 1.754486e-01 1.000000e+00 6.644618e-01
[31]: 7.575031e-02 1.153102e-02 1.581959e-01 2.812089e-01 1.636176e-02
[36]: 1.153102e-02 1.325863e-01 2.774239e-02 1.754486e-01 1.209832e-02
[41]: 2.025930e-03 5.634031e-05 3.546073e-01 1.413926e-01 2.180552e-02
[46]: 1.153102e-02 2.180552e-02 4.956812e-03 3.262838e-01 9.925057e-02

Bonferroni:
[ 1]: 1.000000e+00 1.000000e+00 1.000000e+00 1.000000e+00 1.000000e+00
[ 6]: 1.000000e+00 1.000000e+00 1.000000e+00 1.000000e+00 1.000000e+00
[11]: 1.000000e+00 1.000000e+00 1.000000e+00 1.000000e+00 1.000000e+00
[16]: 1.000000e+00 1.000000e+00 1.000000e+00 1.000000e+00 1.000000e+00
[21]: 1.000000e+00 1.000000e+00 1.000000e+00 1.000000e+00 1.000000e+00
[26]: 1.000000e+00 1.000000e+00 7.019185e-01 1.000000e+00 1.000000e+00
[31]: 2.020365e-01 1.516674e-02 5.625735e-01 1.000000e+00 2.909271e-02
[36]: 1.537741e-02 4.125636e-01 6.782670e-02 6.803480e-01 1.882294e-02
[41]: 9.005725e-04 1.252228e-05 1.000000e+00 4.713920e-01 4.395577e-02
[46]: 1.088915e-02 4.846527e-02 3.305125e-03 1.000000e+00 2.867745e-01

Hochberg:
[ 1]: 9.991834e-01 9.991834e-01 9.991834e-01 9.991834e-01 9.991834e-01
[ 6]: 9.991834e-01 9.991834e-01 9.991834e-01 9.991834e-01 9.991834e-01
[11]: 9.991834e-01 9.991834e-01 9.991834e-01 9.991834e-01 9.991834e-01
[16]: 9.991834e-01 9.991834e-01 9.991834e-01 9.991834e-01 9.991834e-01
[21]: 9.991834e-01 9.991834e-01 9.991834e-01 9.991834e-01 9.991834e-01
[26]: 9.991834e-01 9.991834e-01 4.632662e-01 9.991834e-01 9.991834e-01
[31]: 1.575885e-01 1.383967e-02 3.938014e-01 7.600230e-01 2.501973e-02
[36]: 1.383967e-02 3.052971e-01 5.426136e-02 4.626366e-01 1.656419e-02
[41]: 8.825610e-04 1.252228e-05 9.930759e-01 3.394022e-01 3.692284e-02
[46]: 1.023581e-02 3.974152e-02 3.172920e-03 8.992520e-01 2.179486e-01

Hommel:
[ 1]: 9.991834e-01 9.991834e-01 9.991834e-01 9.987624e-01 9.991834e-01
[ 6]: 9.991834e-01 9.991834e-01 9.991834e-01 9.991834e-01 9.991834e-01
[11]: 9.991834e-01 9.991834e-01 9.991834e-01 9.991834e-01 9.991834e-01
[16]: 9.991834e-01 9.991834e-01 9.991834e-01 9.991834e-01 9.595180e-01
[21]: 9.991834e-01 9.991834e-01 9.991834e-01 9.991834e-01 9.991834e-01
[26]: 9.991834e-01 9.991834e-01 4.351895e-01 9.991834e-01 9.766522e-01
[31]: 1.414256e-01 1.304340e-02 3.530937e-01 6.887709e-01 2.385602e-02
[36]: 1.322457e-02 2.722920e-01 5.426136e-02 4.218158e-01 1.581127e-02
[41]: 8.825610e-04 1.252228e-05 8.743649e-01 3.016908e-01 3.516461e-02
[46]: 9.582456e-03 3.877222e-02 3.172920e-03 8.122276e-01 1.950067e-01

```

