+++
title = "Determinant and permanent"
description = ""
date = 2019-10-08T10:26:45Z
aliases = []
[extra]
id = 11931
[taxonomies]
categories = ["task"]
tags = []
+++

## Task

{{task}} [[Category:Matrices]]
For a given matrix, return the [[wp:Determinant|determinant]] and the [[wp:Permanent|permanent]] of the matrix.

The determinant is given by
:: <big><math>\det(A) = \sum_\sigma\sgn(\sigma)\prod_{i=1}^n M_{i,\sigma_i}</math></big>
while the permanent is given by
:: <big><math> \operatorname{perm}(A)=\sum_\sigma\prod_{i=1}^n M_{i,\sigma_i}</math></big>
In both cases the sum is over the permutations <math>\sigma</math> of the permutations of 1, 2, ..., ''n''. (A permutation's sign is 1 if there are an even number of inversions and -1 otherwise; see [[wp:Parity of a permutation|parity of a permutation]].)

More efficient algorithms for the determinant are known: [[LU decomposition]], see for example [[wp:LU decomposition#Computing the determinant]]. Efficient methods for calculating the permanent are not known.


## Related tasks

* [[Permutations by swapping]]





## 360 Assembly

For maximum compatibility, this program uses only the basic instruction set (S/360)
and two ASSIST macros (XDECO,XPRNT) to keep it as short as possible.
It works on OS/360 family (MVS,z/OS), on DOS/360 family (z/VSE) use GETVIS,FREEVIS instead of GETMAIN,FREEMAIN.

```360asm
*        Matrix arithmetic         13/05/2016
MATARI   START
         STM    R14,R12,12(R13)    save caller's registers
         LR     R12,R15            set R12 as base register
         USING  MATARI,R12         notify assembler
         LA     R11,SAVEAREA       get the address of my savearea
         ST     R13,4(R11)         save caller's savearea pointer
         ST     R11,8(R13)         save my savearea pointer
         LR     R13,R11            set R13 to point to my savearea
         LA     R1,TT              @tt
         BAL    R14,DETER          call deter(tt)
         LR     R2,R0              R2=deter(tt)
         LR     R3,R1              R3=perm(tt)
         XDECO  R2,PG1+12          edit determinant
         XPRNT  PG1,80             print determinant
         XDECO  R3,PG2+12          edit permanent
         XPRNT  PG2,80             print permanent
EXITALL  L      R13,SAVEAREA+4     restore caller's savearea address
         LM     R14,R12,12(R13)    restore caller's registers
         XR     R15,R15            set return code to 0
         BR     R14                return to caller
SAVEAREA DS     18F                main savearea
TT       DC     F'3'               matrix size
         DC     F'2',F'9',F'4',F'7',F'5',F'3',F'6',F'1',F'8' <==input
PG1      DC     CL80'determinant='
PG2      DC     CL80'permanent='
XDEC     DS     CL12
*        recursive function        (R0,R1)=deter(t)   (python style)
DETER    CNOP   0,4                  returns determinant and permanent
         STM    R14,R12,12(R13)    save all registers
         LR     R9,R1              save R1
         L      R2,0(R1)           n
         BCTR   R2,0               n-1
         LR     R11,R2             n-1
         MR     R10,R2             (n-1)*(n-1)
         SLA    R11,2              (n-1)*(n-1)*4
         LA     R11,1(R11)         size of q array
         A      R11,=A(STACKLEN)   R11 storage amount required
         GETMAIN RU,LV=(R11)       allocate storage for stack
         USING  STACK,R10          make storage addressable
         LR     R10,R1             establish stack addressability
         LA     R1,SAVEAREB        get the address of my savearea
         ST     R13,4(R1)          save caller's savearea pointer
         ST     R1,8(R13)          save my savearea pointer
         LR     R13,R1             set R13 to point to my savearea
         LR     R1,R9              restore R1
         LR     R9,R1              @t
         L      R4,0(R9)           t(0)
         ST     R4,N               n=t(0)
IF1      CH     R4,=H'1'           if n=1
         BNE    SIF1               then
         L      R2,4(R9)             t(1)
         ST     R2,R                 r=t(1)
         ST     R2,S                 s=t(1)
         B      EIF1               else
SIF1     L      R2,N                 n
         BCTR   R2,0                 n-1
         ST     R2,Q                 q(0)=n-1
         ST     R2,NM1               nm1=n-1
         LA     R0,1                 1
         ST     R0,SGN               sgn=1
         SR     R0,R0                0
         ST     R0,R                 r=0
         ST     R0,S                 s=0
         LA     R6,1                 k=1
LOOPK    C      R6,N                 do k=1 to n
         BH     ELOOPK               leave k
         SR     R0,R0                  0
         ST     R0,JQ                  jq=0
         ST     R0,KTI                 kti=0
         LA     R7,1                   iq=1
LOOPIQ   C      R7,NM1                 do iq=1 to n-1
         BH     ELOOPIQ                leave iq
         LR     R2,R7                    iq
         LA     R2,1(R2)                 iq+1
         ST     R2,IT                    it=iq+1
         L      R2,KTI                   kti
         A      R2,N                     kti+n
         ST     R2,KTI                   kti=kti+n
         ST     R2,KT                    kt=kti
         LA     R8,1                     jt=1
LOOPJT   C      R8,N                     do jt=1 to n
         BH     ELOOPJT                  leave jt
         L      R2,KT                      kt
         LA     R2,1(R2)                   kt+1
         ST     R2,KT                      kt=kt+1
IF2      CR     R8,R6                      if jt<>k
         BE     EIF2                       then
         L      R2,JQ                        jq
         LA     R2,1(R2)                     jq+1
         ST     R2,JQ                        jq=jq+1
         L      R1,KT                        kt
         SLA    R1,2                         *4
         L      R2,0(R1,R9)                  t(kt)
         L      R1,JQ                        jq
         SLA    R1,2                         *4
         ST     R2,Q(R1)                     q(jq)=t(kt)
EIF2     EQU    *                          end if
         LA     R8,1(R8)                   jt=jt+1
         B      LOOPJT                   next jt
ELOOPJT  LA     R7,1(R7)                 iq=iq+1
         B      LOOPIQ                 next iq
ELOOPIQ  LR     R1,R6                  k
         SLA    R1,2                   *4
         L      R5,0(R1,R9)            t(k)
         LR     R2,R5                  R2,R5=t(k)
         LA     R1,Q                   @q
         BAL    R14,DETER              call deter(q)
         LR     R3,R0                  R3=deter(q)
         ST     R1,P                   p=perm(q)
         MR     R4,R3                  R5=t(k)*deter(q)
         M      R4,SGN                 R5=sgn*t(k)*deter(q)
         A      R5,R                   +r
         ST     R5,R                   r=r+sgn*t(k)*deter(q)
         LR     R5,R2                  t(k)
         M      R4,P                   R5=t(k)*perm(q)
         A      R5,S                   +s
         ST     R5,S                   s=s+t(k)*perm(q)
         L      R2,SGN                 sgn
         LCR    R2,R2                  -sgn
         ST     R2,SGN                 sgn=-sgn
         LA     R6,1(R6)               k=k+1
         B      LOOPK                next k
ELOOPK   EQU    *                    end do
EIF1     EQU    *                  end if
EXIT     L      R13,SAVEAREB+4     restore caller's savearea address
         L      R2,R               return value (determinant)
         L      R3,S               return value (permanent)
         XR     R15,R15            set return code to 0
         FREEMAIN A=(R10),LV=(R11) free allocated storage
         LR     R0,R2              first return value
         LR     R1,R3              second return value
         L      R14,12(R13)        restore caller's return address
         LM     R2,R12,28(R13)     restore registers R2 to R12
         BR     R14                return to caller
IT       DS     F                  static area (out of stack)
KT       DS     F                  "
JQ       DS     F                  "
KTI      DS     F                  "
P        DS     F                  "
         DROP   R12                base no longer needed
STACK    DSECT                     dynamic area (stack)
SAVEAREB DS     18F                function savearea
N        DS     F                  n
NM1      DS     F                  n-1
R        DS     F                  determinant accu
S        DS     F                  permanent accu
SGN      DS     F                  sign
STACKLEN EQU    *-STACK
Q        DS     F                  sub matrix q((n-1)*(n-1)+1)
         YREGS
         END    MATARI
```

```txt

determinant=        -360
permanent=           900

```



## C

C99 code. By no means efficient or reliable.  If you need it for serious work, go find a serious library.

```c
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

double det_in(double **in, int n, int perm)
{
	if (n == 1) return in[0][0];

	double sum = 0, *m[--n];
	for (int i = 0; i < n; i++)
		m[i] = in[i + 1] + 1;

	for (int i = 0, sgn = 1; i <= n; i++) {
		sum += sgn * (in[i][0] * det_in(m, n, perm));
		if (i == n) break;

		m[i] = in[i] + 1;
		if (!perm) sgn = -sgn;
	}
	return sum;
}

/* wrapper function */
double det(double *in, int n, int perm)
{
	double *m[n];
	for (int i = 0; i < n; i++)
		m[i] = in + (n * i);

	return det_in(m, n, perm);
}

int main(void)
{
	double x[] = {	0, 1, 2, 3, 4,
			5, 6, 7, 8, 9,
			10, 11, 12, 13, 14,
			15, 16, 17, 18, 19,
			20, 21, 22, 23, 24 };

	printf("det:  %14.12g\n", det(x, 5, 0));
	printf("perm: %14.12g\n", det(x, 5, 1));

	return 0;
}
```

A method to calculate determinant that might actually be usable:

```c
#include <stdio.h>
#include <stdlib.h>
#include <tgmath.h>

void showmat(const char *s, double **m, int n)
{
	printf("%s:\n", s);
	for (int i = 0; i < n; i++) {
		for (int j = 0; j < n; j++)
			printf("%12.4f", m[i][j]);
			putchar('\n');
	}
}

int trianglize(double **m, int n)
{
	int sign = 1;
	for (int i = 0; i < n; i++) {
		int max = 0;

		for (int row = i; row < n; row++)
			if (fabs(m[row][i]) > fabs(m[max][i]))
				max = row;

		if (max) {
			sign = -sign;
			double *tmp = m[i];
			m[i] = m[max], m[max] = tmp;
		}

		if (!m[i][i]) return 0;

		for (int row = i + 1; row < n; row++) {
			double r = m[row][i] / m[i][i];
			if (!r)	continue;

			for (int col = i; col < n; col ++)
				m[row][col] -= m[i][col] * r;
		}
	}
	return sign;
}

double det(double *in, int n)
{
	double *m[n];
	m[0] = in;

	for (int i = 1; i < n; i++)
		m[i] = m[i - 1] + n;

	showmat("Matrix", m, n);

	int sign = trianglize(m, n);
	if (!sign)
		return 0;

	showmat("Upper triangle", m, n);

	double p = 1;
	for (int i = 0; i < n; i++)
		p *= m[i][i];
	return p * sign;
}

#define N 18
int main(void)
{
	double x[N * N];
	srand(0);
	for (int i = 0; i < N * N; i++)
		x[i] = rand() % N;

	printf("det: %19f\n", det(x, N));
	return 0;
}
```





## Common Lisp

A recursive version, no libraries required, it doesn't use much consing, only for the list of columns to skip


```lisp

(defun determinant (rows &optional (skip-cols nil))
  (let* ((result 0) (sgn -1))
    (dotimes (col (length (car rows)) result)
      (unless (member col skip-cols)
        (if (null (cdr rows))
          (return-from determinant (elt (car rows) col))
          (incf result (* (setq sgn (- sgn)) (elt (car rows) col) (determinant (cdr rows) (cons col skip-cols)))) )))))

(defun permanent (rows &optional (skip-cols nil))
  (let* ((result 0))
    (dotimes (col (length (car rows)) result)
      (unless (member col skip-cols)
        (if (null (cdr rows))
          (return-from permanent (elt (car rows) col))
          (incf result (* (elt (car rows) col) (permanent (cdr rows) (cons col skip-cols)))) )))))


Test using the first set of definitions (from task description):

(setq m2
  '((1 2)
    (3 4)))

(setq m3
  '((-2 2 -3)
    (-1 1  3)
    ( 2 0 -1)))

(setq m4
  '(( 1  2  3  4)
    ( 4  5  6  7)
    ( 7  8  9 10)
    (10 11 12 13)))

(setq m5
  '(( 0  1  2  3  4)
    ( 5  6  7  8  9)
    (10 11 12 13 14)
    (15 16 17 18 19)
    (20 21 22 23 24)))

(dolist (m (list m2 m3 m4 m5))
  (format t "~a determinant: ~a, permanent: ~a~%" m (determinant m) (permanent m)) )

```


```txt
((1 2) (3 4)) determinant: -2, permanent: 10
((-2 2 -3) (-1 1 3) (2 0 -1)) determinant: 18, permanent: 10
((1 2 3 4) (4 5 6 7) (7 8 9 10) (10 11 12 13)) determinant: 0, permanent: 29556
((0 1 2 3 4) (5 6 7 8 9) (10 11 12 13 14) (15 16 17 18 19) (20 21 22 23 24)) determinant: 0, permanent: 6778800

```




## D

This requires the modules from the [[Permutations#D|Permutations]] and [[Permutations_by_swapping#D|Permutations by swapping]] tasks.
```d
import std.algorithm, std.range, std.traits, permutations2,
       permutations_by_swapping1;

auto prod(Range)(Range r) nothrow @safe @nogc {
    return reduce!q{a * b}(ForeachType!Range(1), r);
}

T permanent(T)(in T[][] a) nothrow @safe
in {
    assert(a.all!(row => row.length == a[0].length));
} body {
    auto r = a.length.iota;
    T tot = 0;
    foreach (const sigma; r.array.permutations)
        tot += r.map!(i => a[i][sigma[i]]).prod;
    return tot;
}

T determinant(T)(in T[][] a) nothrow
in {
    assert(a.all!(row => row.length == a[0].length));
} body {
    immutable n = a.length;
    auto r = n.iota;
    T tot = 0;
    //foreach (sigma, sign; n.spermutations) {
    foreach (const sigma_sign; n.spermutations) {
        const sigma = sigma_sign[0];
        immutable sign = sigma_sign[1];
        tot += sign * r.map!(i => a[i][sigma[i]]).prod;
    }
    return tot;
}

void main() {
    import std.stdio;

    foreach (/*immutable*/ const a; [[[1, 2],
                                      [3, 4]],

                                     [[1, 2, 3, 4],
                                      [4, 5, 6, 7],
                                      [7, 8, 9, 10],
                                      [10, 11, 12, 13]],

                                     [[ 0,  1,  2,  3,  4],
                                      [ 5,  6,  7,  8,  9],
                                      [10, 11, 12, 13, 14],
                                      [15, 16, 17, 18, 19],
                                      [20, 21, 22, 23, 24]]]) {
        writefln("[%([%(%2s, %)],\n %)]]", a);
        writefln("Permanent: %s, determinant: %s\n",
                 a.permanent, a.determinant);
    }
}
```

```txt
[[ 1,  2],
 [ 3,  4]]
Permanent: 10, determinant: -2

[[ 1,  2,  3,  4],
 [ 4,  5,  6,  7],
 [ 7,  8,  9, 10],
 [10, 11, 12, 13]]
Permanent: 29556, determinant: 0

[[ 0,  1,  2,  3,  4],
 [ 5,  6,  7,  8,  9],
 [10, 11, 12, 13, 14],
 [15, 16, 17, 18, 19],
 [20, 21, 22, 23, 24]]
Permanent: 6778800, determinant: 0
```



## EchoLisp

This requires the 'list' library for '''(in-permutations n)''' and the 'matrix' library for the built-in '''(determinant M)'''.

```lisp

(lib 'list)
(lib 'matrix)

;; adapted from Racket
(define (permanent M)
    (let (( n (matrix-row-num M)))
    (for/sum ([σ (in-permutations n)])
        (for/product ([i n] [σi σ])
            (array-ref M i σi)))))

;; output
(define A (list->array '(1 2 3 4) 2 2))
(array-print A)
  1  2
  3  4
(determinant A) → -2
(permanent A) → 10

(define M (list->array (iota 25) 5 5))
(array-print M)
   0   1   2   3   4
   5   6   7   8   9
  10  11  12  13  14
  15  16  17  18  19
  20  21  22  23  24
(determinant M) → 0
(permanent M) → 6778800


```



## Factor


```factor
USING: fry kernel math.combinatorics math.matrices sequences ;

: permanent ( matrix -- x )
    dup square-matrix? [ "Matrix must be square." throw ] unless
    [ dim first <iota> ] keep
    '[ [ _ nth nth ] map-index product ] map-permutations sum ;
```

Example output:

```txt

IN: scratchpad USE: math.matrices.laplace   ! for determinant
               { { 2 9 4 } { 7 5 3 } { 6 1 8 } }
               [ determinant ] [ permanent ] bi

--- Data stack:
-360
900

```



## Forth

Requiring a permute.fs file from the [[Permutations_by_swapping#Forth|Permutations by swapping]] task.

```forth
S" fsl-util.fs" REQUIRED
S" fsl/dynmem.seq" REQUIRED
[UNDEFINED] defines [IF] SYNONYM defines IS [THEN]
S" fsl/structs.seq" REQUIRED
S" fsl/lufact.seq" REQUIRED
S" fsl/dets.seq" REQUIRED
S" permute.fs" REQUIRED

VARIABLE the-mat
: add-perm ( p0 p1 p2 ... pn n s -- )
  DROP  \ sign
  1E
  1 DO
    the-mat @ SWAP 1- I 1- }} F@ F*
  LOOP
  DROP  \ Dummy element because we're using 1-based indexing
  F+ ;
: permanent ( len mat -- ) ( F: -- perm )
  the-mat !
  0E
  ['] add-perm perms ;

3 SET-PRECISION
2 2 float matrix m2{{
1e 2e  3e 4e  2 2 m2{{ }}fput
lumatrix lmat
3 3 float matrix m3{{
2e 9e 4e  7e 5e 3e  6e 1e 8e  3 3 m3{{ }}fput

lmat 2 lu-malloc
m2{{ lmat lufact
lmat det F. 2 m2{{ permanent F. CR
lmat lu-free

lmat 3 lu-malloc
m3{{ lmat lufact
lmat det F. 3 m3{{ permanent F. CR
lmat lu-free
```



## Fortran


Please find the compilation and example run at the start of the comments in the f90 source.  Thank you.


```FORTRAN

!-*- mode: compilation; default-directory: "/tmp/" -*-
!Compilation started at Sat May 18 23:25:42
!
!a=./F && make $a && $a < unixdict.txt
!f95 -Wall -ffree-form F.F -o F
! j example, determinant:    7.00000000
! j example, permanent:      5.00000000
! maxima, determinant:      -360.000000
! maxima, permanent:         900.000000
!
!Compilation finished at Sat May 18 23:25:43



!   NB. example computed by J
!   NB. fixed seed random matrix
!   _2+3 3?.@$5
! 2 _1  1
!_1 _2  1
!_1 _1 _1
!
!   (-/ .*)_2+3 3?.@$5  NB. determinant
!7
!   (+/ .*)_2+3 3?.@$5  NB. permanent
!5

!maxima example
!a: matrix([2, 9, 4], [7, 5, 3], [6, 1, 8])$
!determinant(a);
!-360
!
!permanent(a);
!900


! compute permanent or determinant
program f
  implicit none
  real, dimension(3,3) :: j, m
  data j/ 2,-1, 1,-1,-2, 1,-1,-1,-1/
  data m/2, 9, 4, 7, 5, 3, 6, 1, 8/
  write(6,*) 'j example, determinant: ',det(j,3,-1)
  write(6,*) 'j example, permanent:   ',det(j,3,1)
  write(6,*) 'maxima, determinant:    ',det(m,3,-1)
  write(6,*) 'maxima, permanent:      ',det(m,3,1)

contains

  recursive function det(a,n,permanent) result(accumulation)
    ! setting permanent to 1 computes the permanent.
    ! setting permanent to -1 computes the determinant.
    real, dimension(n,n), intent(in) :: a
    integer, intent(in) :: n, permanent
    real, dimension(n-1, n-1) :: b
    real :: accumulation
    integer :: i, sgn
    if (n .eq. 1) then
      accumulation = a(1,1)
    else
      accumulation = 0
      sgn = 1
      do i=1, n
        b(:, :(i-1)) = a(2:, :i-1)
        b(:, i:) = a(2:, i+1:)
        accumulation = accumulation + sgn * a(1, i) * det(b, n-1, permanent)
        sgn = sgn * permanent
      enddo
    endif
  end function det

end program f

```



## FunL

From the task description:

```funl
def sgn( p ) = product( (if s(0) < s(1) xor i(0) < i(1) then -1 else 1) | (s, i) <- p.combinations(2).zip( (0:p.length()).combinations(2) ) )

def perm( m ) = sum( product(m(i, sigma(i)) | i <- 0:m.length()) | sigma <- (0:m.length()).permutations() )

def det( m ) = sum( sgn(sigma)*product(m(i, sigma(i)) | i <- 0:m.length()) | sigma <- (0:m.length()).permutations() )
```


Laplace expansion (recursive):

```funl
def perm( m )
  | m.length() == 1 and m(0).length() == 1 = m(0, 0)
  | otherwise = sum( m(i, 0)*perm(m(0:i, 1:m.length()) + m(i+1:m.length(), 1:m.length())) | i <- 0:m.length() )

def det( m )
  | m.length() == 1 and m(0).length() == 1 = m(0, 0)
  | otherwise = sum( (-1)^i*m(i, 0)*det(m(0:i, 1:m.length()) + m(i+1:m.length(), 1:m.length())) | i <- 0:m.length() )
```


Test using the first set of definitions (from task description):

```funl
matrices = [
  ( (1, 2),
    (3, 4)),
  ( (-2, 2, -3),
    (-1, 1,  3),
    ( 2, 0, -1)),
  ( ( 1,  2,  3,  4),
    ( 4,  5,  6,  7),
    ( 7,  8,  9, 10),
    (10, 11, 12, 13)),
  ( ( 0,  1,  2,  3,  4),
    ( 5,  6,  7,  8,  9),
    (10, 11, 12, 13, 14),
    (15, 16, 17, 18, 19),
    (20, 21, 22, 23, 24)) ]

for m <- matrices
  println( m, 'perm: ' + perm(m), 'det: ' + det(m) )
```


```txt

((1, 2), (3, 4)), perm: 10, det: -2
((-2, 2, -3), (-1, 1, 3), (2, 0, -1)), perm: 10, det: 18
((1, 2, 3, 4), (4, 5, 6, 7), (7, 8, 9, 10), (10, 11, 12, 13)), perm: 29556, det: 0
((0, 1, 2, 3, 4), (5, 6, 7, 8, 9), (10, 11, 12, 13, 14), (15, 16, 17, 18, 19), (20, 21, 22, 23, 24)), perm: 6778800, det: 0

```



## GLSL


```glsl

  mat4 m1 = mat3(1, 2, 3, 4,
                5, 6, 7, 8
                9,10,11,12,
                13,14,15,16);

  float d = det(m1);

```


## Go


### Implementation

This implements a naive algorithm for each that follows from the definitions.  It imports the permute packge from the [[Permutations_by_swapping#Go|Permutations by swapping]] task.

```go
package main

import (
    "fmt"
    "permute"
)

func determinant(m [][]float64) (d float64) {
    p := make([]int, len(m))
    for i := range p {
        p[i] = i
    }
    it := permute.Iter(p)
    for s := it(); s != 0; s = it() {
        pr := 1.
        for i, σ := range p {
            pr *= m[i][σ]
        }
        d += float64(s) * pr
    }
    return
}

func permanent(m [][]float64) (d float64) {
    p := make([]int, len(m))
    for i := range p {
        p[i] = i
    }
    it := permute.Iter(p)
    for s := it(); s != 0; s = it() {
        pr := 1.
        for i, σ := range p {
            pr *= m[i][σ]
        }
        d += pr
    }
    return
}

var m2 = [][]float64{
    {1, 2},
    {3, 4}}

var m3 = [][]float64{
    {2, 9, 4},
    {7, 5, 3},
    {6, 1, 8}}

func main() {
    fmt.Println(determinant(m2), permanent(m2))
    fmt.Println(determinant(m3), permanent(m3))
}
```

```txt

-2 10
-360 900

```


### Ryser permanent


```go
package main

import "fmt"

func main() {
    fmt.Println(ryser([][]float64{
        {1, 2},
        {3, 4}}))
    fmt.Println(ryser([][]float64{
        {2, 9, 4},
        {7, 5, 3},
        {6, 1, 8}}))
}

func ryser(m [][]float64) (d float64) {
    gray := 0
    csum := make([]float64, len(m))
    sgn := float64(len(m)&1<<1 - 1)
    n2 := uint32(1) << uint(len(m))
    for i := uint32(1); i < n2; i++ {
        r := [...]byte{
            0, 1, 28, 2, 29, 14, 24, 3, 30, 22, 20, 15, 25, 17, 4, 8,
            31, 27, 13, 23, 21, 19, 16, 7, 26, 12, 18, 6, 11, 5, 10, 9,
        }[i&-i*0x077CB531>>27]
        b := 1 << r
        if gray&b == 0 {
            for c, e := range m[r] {
                csum[c] += e
            }
        } else {
            for c, e := range m[r] {
                csum[c] -= e
            }
        }
        gray ^= b
        p := sgn
        for _, e := range csum {
            p *= e
        }
        d += p
        sgn = -sgn
    }
    return
}
```

```txt

10
900

```


### Library determinant

'''go.matrix:'''

```go
package main

import (
    "fmt"

    "github.com/skelterjohn/go.matrix"
)

func main() {
    fmt.Println(matrix.MakeDenseMatrixStacked([][]float64{
        {1, 2},
        {3, 4}}).Det())
    fmt.Println(matrix.MakeDenseMatrixStacked([][]float64{
        {2, 9, 4},
        {7, 5, 3},
        {6, 1, 8}}).Det())
}
```

```txt

-2
-360

```

'''gonum/mat:'''

```go
package main

import (
    "fmt"

    "gonum.org/v1/gonum/mat"
)

func main() {
    fmt.Println(mat.Det(mat.NewDense(2, 2, []float64{
        1, 2,
        3, 4})))
    fmt.Println(mat.Det(mat.NewDense(3, 3, []float64{
        2, 9, 4,
        7, 5, 3,
        6, 1, 8})))
}
```

```txt

-2
-360.00000000000006

```



## Haskell


```Haskell
sPermutations :: [a] -> [([a], Int)]
sPermutations = flip zip (cycle [1, -1]) . foldl aux [[]]
  where
    aux items x = do
      (f, item) <- zip (cycle [reverse, id]) items
      f (insertEv x item)
    insertEv x [] = [[x]]
    insertEv x l@(y:ys) = (x : l) : ((y :) <$>) (insertEv x ys)

elemPos :: [[a]] -> Int -> Int -> a
elemPos ms i j = (ms !! i) !! j

prod
  :: Num a
  => ([[a]] -> Int -> Int -> a) -> [[a]] -> [Int] -> a
prod f ms = product . zipWith (f ms) [0 ..]

sDeterminant
  :: Num a
  => ([[a]] -> Int -> Int -> a) -> [[a]] -> [([Int], Int)] -> a
sDeterminant f ms = sum . fmap (\(is, s) -> fromIntegral s * prod f ms is)

determinant
  :: Num a
  => [[a]] -> a
determinant ms =
  sDeterminant elemPos ms . sPermutations $ [0 .. pred . length $ ms]

permanent
  :: Num a
  => [[a]] -> a
permanent ms =
  sum . fmap (prod elemPos ms . fst) . sPermutations $ [0 .. pred . length $ ms]

-- TEST -----------------------------------------------------------------------
result
  :: (Num a, Show a)
  => [[a]] -> String
result ms =
  unlines
    [ "Matrix:"
    , unlines (show <$> ms)
    , "Determinant:"
    , show (determinant ms)
    , "Permanent:"
    , show (permanent ms)
    ]

main :: IO ()
main =
  mapM_
    (putStrLn . result)
    [ [[5]]
    , [[1, 0, 0], [0, 1, 0], [0, 0, 1]]
    , [[0, 0, 1], [0, 1, 0], [1, 0, 0]]
    , [[4, 3], [2, 5]]
    , [[2, 5], [4, 3]]
    , [[4, 4], [2, 2]]
    ]
```

```txt
Matrix:
[5]

Determinant:
5
Permanent:
5

Matrix:
[1,0,0]
[0,1,0]
[0,0,1]

Determinant:
1
Permanent:
1

Matrix:
[0,0,1]
[0,1,0]
[1,0,0]

Determinant:
-1
Permanent:
1

Matrix:
[4,3]
[2,5]

Determinant:
14
Permanent:
26

Matrix:
[2,5]
[4,3]

Determinant:
-14
Permanent:
26

Matrix:
[4,4]
[2,2]

Determinant:
0
Permanent:
16
```


===Via Cramer's rule===
Here is code for computing the determinant and permanent very inefficiently, via [[wp:Cramer's rule|Cramer's rule]] (for the determinant, as well as its analog for the permanent):


```Haskell

outer :: (a->b->c) -> [a] -> [b] -> [[c]]
outer f [] _       = []
outer f _ []       = []
outer f (h1:t1) x2 = (f h1 <$> x2) : outer f t1 x2

dot [] []           = 0
dot (h1:t1) (h2:t2) = (h1*h2) + (dot t1 t2)

transpose [] = []
transpose ([] : xss) = transpose xss
transpose ((x:xs) : xss)
  = (x : [h | (h:_) <- xss]) : transpose (xs : [ t | (_:t) <- xss])

mul :: Num a => [[a]] -> [[a]] -> [[a]]
mul a b = outer dot a (transpose b)

delRow :: Int -> [a] -> [a]
delRow i v =
  (first ++ rest) where (first, _:rest) = splitAt i v

delCol :: Int -> [[a]] -> [[a]]
delCol j m = (delRow j) <$> m

-- Determinant:
adj :: Num a => [[a]] -> [[a]]
adj [] = []
adj m =
  [
    [(-1)^(i+j) * det (delRow i $ delCol j m)
    | i <- [0.. -1+length m]
    ]
  | j <- [0.. -1+length m]
  ]
det :: Num a => [[a]] -> a
det [] = 1
det m  = (mul m (adj m)) !! 0 !! 0

-- Permanent:
padj :: Num a => [[a]] -> [[a]]
padj [] = []
padj m =
  [
    [perm (delRow i $ delCol j m)
    | i <- [0.. -1+length m]
    ]
  | j <- [0.. -1+length m]
  ]
perm :: Num a => [[a]] -> a
perm [] = 1
perm m  = (mul m (padj m)) !! 0 !! 0


```



## J


J has a [[j:Vocabulary/dot|conjunction]] for defining verbs which can act as determinant (especially <code>-/ .* </code>).  This conjunction is symbolized as a space followed by a dot. And you can get the permanent by replacing <code>-</code> in that definition with <code>+</code>.

For example, given the matrix:


```J
   i. 5 5
 0  1  2  3  4
 5  6  7  8  9
10 11 12 13 14
15 16 17 18 19
20 21 22 23 24
```


Its determinant is 0.  When we use IEEE floating point, we only get an approximation of this result:


```J
   -/ .* i. 5 5
_1.30277e_44
```


If we use exact (rational) arithmetic, we get a precise result:


```J
   -/ .* i. 5 5x
0
```


Meanwhile, the permanent does not have this problem in this example (the matrix contains no negative values and permanent does not use subtraction):


```J
   +/ .* i. 5 5
6778800
```


As an aside, note also that for specific verbs (like <code>-/ .*</code>) J uses an algorithm which is more efficient than the brute force approach implied by the [http://www.jsoftware.com/help/dictionary/d300.htm definition of <code> .</code>]. (In general, where there are common, useful, concise definitions where special code can improve resource use by more than a factor of 2, the implementors of J try to make sure that that special code gets used for those definitions.)


## Java



```Java
import java.util.Scanner;

public class MatrixArithmetic {
	public static double[][] minor(double[][] a, int x, int y){
		int length = a.length-1;
		double[][] result = new double[length][length];
		for(int i=0;i<length;i++) for(int j=0;j<length;j++){
			if(i<x && j<y){
				result[i][j] = a[i][j];
			}else if(i>=x && j<y){
				result[i][j] = a[i+1][j];
			}else if(i<x && j>=y){
				result[i][j] = a[i][j+1];
			}else{ //i>x && j>y
				result[i][j] = a[i+1][j+1];
			}
		}
		return result;
	}
	public static double det(double[][] a){
		if(a.length == 1){
			return a[0][0];
		}else{
			int sign = 1;
			double sum = 0;
			for(int i=0;i<a.length;i++){
				sum += sign * a[0][i] * det(minor(a,0,i));
				sign *= -1;
			}
			return sum;
		}
	}
	public static double perm(double[][] a){
		if(a.length == 1){
			return a[0][0];
		}else{
			double sum = 0;
			for(int i=0;i<a.length;i++){
				sum += a[0][i] * perm(minor(a,0,i));
			}
			return sum;
		}
	}
	public static void main(String args[]){
		Scanner sc = new Scanner(System.in);
		int size = sc.nextInt();
		double[][] a = new double[size][size];
		for(int i=0;i<size;i++) for(int j=0;j<size;j++){
			a[i][j] = sc.nextDouble();
		}
		sc.close();
		System.out.println("Determinant: "+det(a));
		System.out.println("Permanent: "+perm(a));
	}
}
```


Note that the first input is the size of the matrix.

For example:

<lang>2
1 2
3 4
Determinant: -2.0
Permanent: 10.0


5
0 1 2 3 4
5 6 7 8 9
10 11 12 13 14
15 16 17 18 19
20 21 22 23 24
Determinant: 0.0
Permanent: 6778800.0

```



## jq

### =Recursive definitions=


```jq
# Eliminate row i and row j
def except(i;j):
  reduce del(.[i])[] as $row ([]; . + [$row | del(.[j]) ] );

def det:
  def parity(i): if i % 2 == 0 then 1 else -1 end;
  if length == 1 and (.[0] | length) == 1 then .[0][0]
  else . as $m
    | reduce range(0; length) as $i
        (0; . + parity($i) * $m[0][$i] * ( $m | except(0;$i) | det) )
  end ;

def perm:
  if length == 1 and (.[0] | length) == 1 then .[0][0]
  else . as $m
    | reduce range(0; length) as $i
        (0; . + $m[0][$i] * ( $m | except(0;$i) | perm) )
  end ;
```

'''Examples'''

```jq
def matrices:
  [ [1, 2],
    [3, 4]],

  [ [-2, 2, -3],
    [-1, 1,  3],
    [ 2, 0, -1]],

  [ [ 1,  2,  3,  4],
    [ 4,  5,  6,  7],
    [ 7,  8,  9, 10],
    [10, 11, 12, 13]],

  [ [ 0,  1,  2,  3,  4],
    [ 5,  6,  7,  8,  9],
    [10, 11, 12, 13, 14],
    [15, 16, 17, 18, 19],
    [20, 21, 22, 23, 24]]
;

"Determinants: ", (matrices | det),
"Permanents:   ",  (matrices | perm)
```

```sh
$ jq -n -r -f Matrix_arithmetic.jq
Determinants:
-2
18
0
0
Permanents:
10
10
29556
6778800
```


### =Determinant via LU Decomposition=

The following uses the jq infrastructure at [[LU decomposition]] to achieve an efficient implementation of det/0:

```jq
# Requires lup/0
def det:
  def product_diagonal:
    . as $m | reduce range(0;length) as $i (1; . * $m[$i][$i]);
  def tidy: if . == -0 then 0 else . end;
  lup
  | (.[0]|product_diagonal) as $l
  | if $l == 0 then 0 else $l * (.[1]|product_diagonal) | tidy end ;

```

'''Examples'''

Using matrices/0 as defined above:

```jq
matrices | det
```

 $ /usr/local/bin/jq -M -n -f LU.rc
 2
 -18
 0
 0


## Julia


```Julia> using LinearAlgebra</lang

The determinant of a matrix <code>A</code> can be computed by the built-in function

```julia
det(A)
```


The following function computes the permanent of a matrix A from the definition:

```julia
function perm(A)
  m, n = size(A)
  if m != n; throw(ArgumentError("permanent is for square matrices only")); end
  sum(σ -> prod(i -> A[i,σ[i]], 1:n), permutations(1:n))
end
```


Example output:

```julia>julia
 A = [2 9 4; 7 5 3; 6 1 8]
julia> det(A), perm(A)
(-360.0,900)
```



## Kotlin


```scala
// version 1.1.2

typealias Matrix = Array<DoubleArray>

fun johnsonTrotter(n: Int): Pair<List<IntArray>, List<Int>> {
    val p = IntArray(n) { it }  // permutation
    val q = IntArray(n) { it }  // inverse permutation
    val d = IntArray(n) { -1 }  // direction = 1 or -1
    var sign = 1
    val perms = mutableListOf<IntArray>()
    val signs = mutableListOf<Int>()

    fun permute(k: Int) {
        if (k >= n) {
            perms.add(p.copyOf())
            signs.add(sign)
            sign *= -1
            return
        }
        permute(k + 1)
        for (i in 0 until k) {
            val z = p[q[k] + d[k]]
            p[q[k]] = z
            p[q[k] + d[k]] = k
            q[z] = q[k]
            q[k] += d[k]
            permute(k + 1)
        }
        d[k] *= -1
    }

    permute(0)
    return perms to signs
}

fun determinant(m: Matrix): Double {
    val (sigmas, signs) = johnsonTrotter(m.size)
    var sum = 0.0
    for ((i, sigma) in sigmas.withIndex()) {
        var prod = 1.0
        for ((j, s) in sigma.withIndex()) prod *= m[j][s]
        sum += signs[i] * prod
    }
    return sum
}

fun permanent(m: Matrix) : Double {
    val (sigmas, _) = johnsonTrotter(m.size)
    var sum = 0.0
    for (sigma in sigmas) {
        var prod = 1.0
        for ((i, s) in sigma.withIndex()) prod *= m[i][s]
        sum += prod
    }
    return sum
}

fun main(args: Array<String>) {
    val m1 = arrayOf(
        doubleArrayOf(1.0)
    )

    val m2 = arrayOf(
        doubleArrayOf(1.0, 2.0),
        doubleArrayOf(3.0, 4.0)
    )

    val m3 = arrayOf(
        doubleArrayOf(2.0, 9.0, 4.0),
        doubleArrayOf(7.0, 5.0, 3.0),
        doubleArrayOf(6.0, 1.0, 8.0)
    )

    val m4 = arrayOf(
        doubleArrayOf( 1.0,  2.0,  3.0,  4.0),
        doubleArrayOf( 4.0,  5.0,  6.0,  7.0),
        doubleArrayOf( 7.0,  8.0,  9.0, 10.0),
        doubleArrayOf(10.0, 11.0, 12.0, 13.0)
    )

    val matrices = arrayOf(m1, m2, m3, m4)
    for (m in matrices) {
        println("m${m.size} -> ")
        println("  determinant = ${determinant(m)}")
        println("  permanent   = ${permanent(m)}\n")
    }
}
```


```txt

m1 ->
  determinant = 1.0
  permanent   = 1.0

m2 ->
  determinant = -2.0
  permanent   = 10.0

m3 ->
  determinant = -360.0
  permanent   = 900.0

m4 ->
  determinant = 0.0
  permanent   = 29556.0

```



## Lua


```lua
-- Johnson–Trotter permutations generator
_JT={}
function JT(dim)
  local n={ values={}, positions={}, directions={}, sign=1 }
  setmetatable(n,{__index=_JT})
  for i=1,dim do
    n.values[i]=i
    n.positions[i]=i
    n.directions[i]=-1
  end
  return n
end

function _JT:largestMobile()
  for i=#self.values,1,-1 do
    local loc=self.positions[i]+self.directions[i]
    if loc >= 1 and loc <= #self.values and self.values[loc] < i then
      return i
    end
  end
  return 0
end

function _JT:next()
  local r=self:largestMobile()
  if r==0 then return false end
  local rloc=self.positions[r]
  local lloc=rloc+self.directions[r]
  local l=self.values[lloc]
  self.values[lloc],self.values[rloc] = self.values[rloc],self.values[lloc]
  self.positions[l],self.positions[r] = self.positions[r],self.positions[l]
  self.sign=-self.sign
  for i=r+1,#self.directions do self.directions[i]=-self.directions[i] end
  return true
end

-- matrix class

_MTX={}
function MTX(matrix)
  setmetatable(matrix,{__index=_MTX})
  matrix.rows=#matrix
  matrix.cols=#matrix[1]
  return matrix
end

function _MTX:dump()
  for _,r in ipairs(self) do
    print(unpack(r))
  end
end

function _MTX:perm() return self:det(1) end
function _MTX:det(perm)
  local det=0
  local jt=JT(self.cols)
  repeat
    local pi=perm or jt.sign
    for i,v in ipairs(jt.values) do
      pi=pi*self[i][v]
    end
    det=det+pi
  until not jt:next()
  return det
end

-- test

matrix=MTX
{
  { 7,  2, -2,  4},
  { 4,  4,  1,  7},
  {11, -8,  9, 10},
  {10,  5, 12, 13}
}
matrix:dump();
print("det:",matrix:det(), "permanent:",matrix:perm(),"\n")

matrix2=MTX
{
  {-2, 2,-3},
  {-1, 1, 3},
  { 2, 0,-1}
}
matrix2:dump();
print("det:",matrix2:det(), "permanent:",matrix2:perm())

```

```txt
7       2       -2      4
4       4       1       7
11      -8      9       10
10      5       12      13
det:    -4319   permanent:      10723

-2      2       -3
-1      1       3
2       0       -1
det:    18      permanent:      10
```


=={{header|MK-61/52}}==

```txt

П4	ИПE	П2	КИП0	ИП0	П1	С/П	ИП4	/	КП2
L1	06	ИПE	П3	ИП0	П1	Сx	КП2	L1	17
ИП0	ИП2	+	П1	П2	ИП3	-	x#0	34	С/П
ПП	80	БП	21	КИП0	ИП4	С/П	КИП2	-	*
П4	ИП0	П3	x#0	35	Вx	С/П	КИП2	-	<->
/	КП1	L3	45	ИП1	ИП0	+	П3	ИПE	П1
П2	КИП1	/-/	ПП	80	ИП3	+	П3	ИП1	-
x=0	61	ИП0	П1	КИП3	КП2	L1	74	БП	12
ИП0	<->	^	КИП3	*	КИП1	+	КП2	->	L0
82	->	П0	В/О

```


This program calculates the determinant of the matrix of order <= 5. Prior to startup, ''РE'' entered ''13'', entered the order of the matrix ''Р0'', and the elements are introduced with the launch of the program after one of them, the last on the screen will be determinant. Permanent is calculated in this way.


## Maple


```Maple
M:=<<2|9|4>,<7|5|3>,<6|1|8>>:

with(LinearAlgebra):

Determinant(M);
Permanent(M);
```

Output:

```txt
                                    -360
                                     900
```



## Mathematica

Determinant is a built in function Det

```Mathematica
Permanent[m_List] :=
    With[{v = Array[x, Length[m]]},
      Coefficient[Times @@ (m.v), Times @@ v]
  ]
```



## Maxima


```maxima
a: matrix([2, 9, 4], [7, 5, 3], [6, 1, 8])$

determinant(a);
-360

permanent(a);
900
```



## Nim

Using the permutationsswap module from [[Permutations by swapping#Nim|Permutations by swapping]]:

```nim
import sequtils, permutationsswap

type Matrix[M,N: static[int]] = array[M, array[N, float]]

proc det[M,N](a: Matrix[M,N]): float =
  let n = toSeq 0..a.high
  for sigma, sign in n.permutations:
    var x = sign.float
    for i in n: x *= a[i][sigma[i]]
    result += x

proc perm[M,N](a: Matrix[M,N]): float =
  let n = toSeq 0..a.high
  for sigma, sign in n.permutations:
    var x = 1.0
    for i in n: x *= a[i][sigma[i]]
    result += x

const
  a = [ [1.0, 2.0]
      , [3.0, 4.0]
      ]
  b = [ [ 1.0,  2,  3,  4]
      , [ 4.0,  5,  6,  7]
      , [ 7.0,  8,  9, 10]
      , [10.0, 11, 12, 13]
      ]
  c = [ [ 0.0,  1,  2,  3,  4]
      , [ 5.0,  6,  7,  8,  9]
      , [10.0, 11, 12, 13, 14]
      , [15.0, 16, 17, 18, 19]
      , [20.0, 21, 22, 23, 24]
      ]

echo "perm: ", a.perm, " det: ", a.det
echo "perm: ", b.perm, " det: ", b.det
echo "perm: ", c.perm, " det: ", c.det
```

Output:

```txt
perm: 10.0 det: -2.0
perm: 29556.0 det: 0.0
perm: 6778800.0 det: 0.0
```



## Ol


```scheme

; helper function that returns rest of matrix by col/row
(define (rest matrix i j)
   (define (exclude1 l x) (append (take l (- x 1)) (drop l x)))
   (exclude1
      (map exclude1
         matrix (repeat i (length matrix)))
      j))

; superfunction for determinant and permanent
(define (super matrix math)
   (let loop ((n (length matrix)) (matrix matrix))
      (if (eq? n 1)
         (caar matrix)
         (fold (lambda (x a j)
                  (+ x (* a (lref math (mod j 2)) (super (rest matrix j 1) math))))
            0
            (car matrix)
            (iota n 1)))))


; det/per calculators
(define (det matrix) (super matrix '(-1 1)))
(define (per matrix) (super matrix '( 1 1)))

; ---=( testing )=---------------------
(print (det '(
   (1 2)
   (3 4))))
; ==> -2

(print (per '(
   (1 2)
   (3 4))))
; ==> 10


(print (det '(
   ( 1  2  3  1)
   (-1 -1 -1  2)
   ( 1  3  1  1)
   (-2 -2  0 -1))))
; ==> 26

(print (per '(
   ( 1  2  3  1)
   (-1 -1 -1  2)
   ( 1  3  1  1)
   (-2 -2  0 -1))))
; ==> -10


(print (det '(
   ( 0  1  2  3  4)
   ( 5  6  7  8  9)
   (10 11 12 13 14)
   (15 16 17 18 19)
   (20 21 22 23 24))))
; ==> 0

(print (per '(
   ( 0  1  2  3  4)
   ( 5  6  7  8  9)
   (10 11 12 13 14)
   (15 16 17 18 19)
   (20 21 22 23 24))))
; ==> 6778800

```



## PARI/GP

The determinant is built in:

```parigp
matdet(M)
```

and the permanent can be defined as

```parigp
matperm(M)=my(n=#M,t);sum(i=1,n!,t=numtoperm(n,i);prod(j=1,n,M[j,t[j]]))
```

For better performance, here's a version using Ryser's formula:

```parigp
matperm(M)=
{
	my(n=matsize(M)[1],innerSums=vectorv(n));
	if(n==0, return(1));
	sum(x=1,2^n-1,
		my(k=valuation(x,2),s=M[,k+1],gray=bitxor(x, x>>1));
		if(bittest(gray,k),
			innerSums += s;
		,
			innerSums -= s;
		);
		(-1)^hammingweight(gray)*factorback(innerSums)
	)*(-1)^n;
}
```


As of version 2.10, the matrix permanent is built in:

```parigp
matpermanent(M)
```



## Perl

```perl
#!/usr/bin/perl
use strict;
use warnings;
use PDL;
use PDL::NiceSlice;

sub permanent{
	my $mat = shift;
	my $n = shift // $mat->dim(0);
	return undef if $mat->dim(0) != $mat->dim(1);
	return $mat(0,0) if $n == 1;
	my $sum = 0;
	--$n;
	my $m = $mat(1:,1:)->copy;
	for(my $i = 0; $i <= $n; ++$i){
		$sum += $mat($i,0) * permanent($m, $n);
		last if $i == $n;
		$m($i,:) .= $mat($i,1:);
	}
	return sclr($sum);
}

my $M = pdl([[2,9,4], [7,5,3], [6,1,8]]);
print "M = $M\n";
print "det(M) = " . $M->determinant . ".\n";
print "det(M) = " . $M->det . ".\n";
print "perm(M) = " . permanent($M) . ".\n";
```


<code>determinant</code> and <code>det</code> are already defined in PDL, see[http://pdl.perl.org/?docs=MatrixOps&title=the%20PDL::MatrixOps%20manpage#det]. <code>permanent</code> has to be defined manually.

```txt

M =
[
 [2 9 4]
 [7 5 3]
 [6 1 8]
]

det(M) = -360.
det(M) = -360.
perm(M) = 900.

```



## Perl 6

Uses the permutations generator from the [[Permutations by swapping#Perl_6|Permutations by swapping]] task. This implementation is naive and brute-force (slow) but exact.


```perl6
sub insert ($x, @xs) { ([flat @xs[0 ..^ $_], $x, @xs[$_ .. *]] for 0 .. @xs) }
sub order ($sg, @xs) { $sg > 0 ?? @xs !! @xs.reverse }

multi σ_permutations ([]) { [] => 1 }

multi σ_permutations ([$x, *@xs]) {
    σ_permutations(@xs).map({ |order($_.value, insert($x, $_.key)) }) Z=> |(1,-1) xx *
}

sub m_arith ( @a, $op ) {
    note "Not a square matrix" and return
      if [||] map { @a.elems cmp @a[$_].elems }, ^@a;
    [+] map {
        my $permutation = .key;
        my $term = $op eq 'perm' ?? 1 !! .value;
        for $permutation.kv -> $i, $j { $term *= @a[$i][$j] };
        $term
    }, σ_permutations [^@a];
}

########### Testing ###########

my @tests = (
    [
        [ 1, 2 ],
        [ 3, 4 ]
    ],
    [
        [  1,  2,  3,  4 ],
        [  4,  5,  6,  7 ],
        [  7,  8,  9, 10 ],
        [ 10, 11, 12, 13 ]
    ],
    [
        [  0,  1,  2,  3,  4 ],
        [  5,  6,  7,  8,  9 ],
        [ 10, 11, 12, 13, 14 ],
        [ 15, 16, 17, 18, 19 ],
        [ 20, 21, 22, 23, 24 ]
    ]
);

sub dump (@matrix) {
    say $_».fmt: "%3s" for @matrix;
    say '';
}

for @tests -> @matrix {
    say 'Matrix:';
    @matrix.&dump;
    say "Determinant:\t", @matrix.&m_arith: <det>;
    say "Permanent:  \t", @matrix.&m_arith: <perm>;
    say '-' x 25;
}
```


'''Output'''

```txt
Matrix:
[  1   2]
[  3   4]

Determinant:	-2
Permanent:  	10
-------------------------
Matrix:
[  1   2   3   4]
[  4   5   6   7]
[  7   8   9  10]
[ 10  11  12  13]

Determinant:	0
Permanent:  	29556
-------------------------
Matrix:
[  0   1   2   3   4]
[  5   6   7   8   9]
[ 10  11  12  13  14]
[ 15  16  17  18  19]
[ 20  21  22  23  24]

Determinant:	0
Permanent:  	6778800
-------------------------
```



## Phix

```Phix
function minor(sequence a, integer x, integer y)
integer l = length(a)-1
sequence result = repeat(repeat(0,l),l)
    for i=1 to l do
        for j=1 to l do
            result[i][j] = a[i+(i>=x)][j+(j>=y)]
        end for
    end for
    return result
end function

function det(sequence a)
    if length(a)=1 then
        return a[1][1]
    end if
    integer sgn = 1
    integer res = 0
    for i=1 to length(a) do
        res += sgn*a[1][i]*det(minor(a,1,i))
        sgn *= -1
    end for
    return res
end function

function perm(sequence a)
    if length(a)=1 then
        return a[1][1]
    end if
    integer res = 0
    for i=1 to length(a) do
        res += a[1][i]*perm(minor(a,1,i))
    end for
    return res
end function

constant tests = {
{{1,  2},
 {3,  4}},
--Determinant: -2, permanent: 10
{{2, 9, 4},
 {7, 5, 3},
 {6, 1, 8}},
--Determinant: -360, permanent: 900
{{ 1,  2,  3,  4},
 { 4,  5,  6,  7},
 { 7,  8,  9, 10},
 {10, 11, 12, 13}},
--Determinant: 0, permanent: 29556
{{ 0,  1,  2,  3,  4},
 { 5,  6,  7,  8,  9},
 {10, 11, 12, 13, 14},
 {15, 16, 17, 18, 19},
 {20, 21, 22, 23, 24}},
--Determinant: 0, permanent: 6778800
{{5}},
--Determinant: 5, permanent: 5
{{1,0,0},
 {0,1,0},
 {0,0,1}},
--Determinant: 1, permanent: 1
{{0,0,1},
 {0,1,0},
 {1,0,0}},
--Determinant: -1, Permanent: 1
{{4,3},
 {2,5}},
--Determinant: 14, Permanent: 26
{{2,5},
 {4,3}},
--Determinant: -14, Permanent: 26
{{4,4},
 {2,2}},
--Determinant: 0, Permanent: 16
{{7,    2,      -2,     4},
 {4,    4,      1,      7},
 {11,   -8,     9,      10},
 {10,   5,      12,     13}},
--det:  -4319   permanent:      10723

{{-2,   2,      -3},
 {-1,   1,      3},
 {2 ,   0,      -1}}
--det:  18      permanent:      10
}
for i=1 to length(tests) do
    sequence ti = tests[i]
    ?{det(ti),perm(ti)}
end for
```

```txt

{-2,10}
{-360,900}
{0,29556}
{0,6778800}
{5,5}
{1,1}
{-1,1}
{14,26}
{-14,26}
{0,16}
{-4319,10723}
{18,10}

```



## PowerShell


```PowerShell

function det-perm ($array) {
    if($array) {
        $size = $array.Count
        function prod($A) {
            $prod = 1
            if($A) { $A | foreach{$prod *= $_} }
            $prod
        }
        function generate($sign, $n, $A) {
            if($n -eq 1) {
                $i = 0
                $prod = prod @($A | foreach{$array[$i++][$_]})
                [pscustomobject]@{det = $sign*$prod; perm = $prod}
            }
            else{
                for($i = 0; $i -lt ($n - 1); $i += 1) {
                    generate $sign ($n - 1) $A
                    if($n % 2 -eq 0){
                        $i1, $i2 = $i, ($n-1)
                        $A[$i1], $A[$i2] = $A[$i2], $A[$i1]
                    }
                    else{
                        $i1, $i2 = 0, ($n-1)
                        $A[$i1], $A[$i2] = $A[$i2], $A[$i1]
                    }
                    $sign *= -1
                }
                generate $sign ($n - 1) $A
            }
        }
        $det = $perm = 0
        generate 1 $size @(0..($size-1)) | foreach{
            $det += $_.det
            $perm += $_.perm
        }
        [pscustomobject]@{det =  "$det"; perm = "$perm"}
    } else {Write-Error "empty array"}
}
det-perm 5
det-perm @(@(1,0,0),@(0,1,0),@(0,0,1))
det-perm @(@(0,0,1),@(0,1,0),@(1,0,0))
det-perm @(@(4,3),@(2,5))
det-perm @(@(2,5),@(4,3))
det-perm @(@(4,4),@(2,2))

```

<b>Output:</b>

```txt

det                                                    perm
---                                                    ----
5                                                      5
1                                                      1
-1                                                     1
14                                                     26
-14                                                    26
0                                                      16

```



## Python

Using the module file spermutations.py from [[Permutations by swapping#Python|Permutations by swapping]]. The algorithm for the determinant is a more literal translation of the expression in the task description and the Wikipedia reference.


```python
from itertools import permutations
from operator import mul
from math import fsum
from spermutations import spermutations

def prod(lst):
    return reduce(mul, lst, 1)

def perm(a):
    n = len(a)
    r = range(n)
    s = permutations(r)
    return fsum(prod(a[i][sigma[i]] for i in r) for sigma in s)

def det(a):
    n = len(a)
    r = range(n)
    s = spermutations(n)
    return fsum(sign * prod(a[i][sigma[i]] for i in r)
                for sigma, sign in s)

if __name__ == '__main__':
    from pprint import pprint as pp

    for a in (
            [
             [1, 2],
             [3, 4]],

            [
             [1, 2, 3, 4],
             [4, 5, 6, 7],
             [7, 8, 9, 10],
             [10, 11, 12, 13]],

            [
             [ 0,  1,  2,  3,  4],
             [ 5,  6,  7,  8,  9],
             [10, 11, 12, 13, 14],
             [15, 16, 17, 18, 19],
             [20, 21, 22, 23, 24]],
        ):
        print('')
        pp(a)
        print('Perm: %s Det: %s' % (perm(a), det(a)))
```


;Sample output:

```txt
[[1, 2], [3, 4]]
Perm: 10 Det: -2

[[1, 2, 3, 4], [4, 5, 6, 7], [7, 8, 9, 10], [10, 11, 12, 13]]
Perm: 29556 Det: 0

[[0, 1, 2, 3, 4],
 [5, 6, 7, 8, 9],
 [10, 11, 12, 13, 14],
 [15, 16, 17, 18, 19],
 [20, 21, 22, 23, 24]]
Perm: 6778800 Det: 0
```


The second matrix above is that used in the Tcl example. The third matrix is from the J language example. Note that the determinant seems to be 'exact' using this method of calculation without needing to resort to other than Pythons default numbers.


## Racket


```racket

#lang racket
(require math)
(define determinant matrix-determinant)

(define (permanent M)
  (define n (matrix-num-rows M))
  (for/sum ([σ (in-permutations (range n))])
    (for/product ([i n] [σi σ])
      (matrix-ref M i σi))))

```



## REXX


```rexx
/* REXX ***************************************************************
* Test the two functions determinant and permanent
* using the matrix specifications shown for other languages
* 21.05.2013 Walter Pachl
**********************************************************************/
Call test ' 1  2',
          ' 3  4',2

Call test ' 1  2  3  4',
          ' 4  5  6  7',
          ' 7  8  9 10',
          '10 11 12 13',4

Call test ' 0  1  2  3  4',
          ' 5  6  7  8  9',
          '10 11 12 13 14',
          '15 16 17 18 19',
          '20 21 22 23 24',5

Exit

test:
/**********************************************************************
* Show the given matrix and compute and show determinant and permanent
**********************************************************************/
Parse Arg as,n
asc=as
Do i=1 To n
  ol=''
  Do j=1 To n
    Parse Var asc a.i.j asc
    ol=ol right(a.i.j,3)
    End
   Say ol
  End
Say 'determinant='right(determinant(as),7)
Say '  permanent='right(permanent(as),7)
Say copies('-',50)
Return
```



```rexx
/* REXX ***************************************************************
* determinant.rex
* compute the determinant of the given square matrix
* Input: as: the representation of the matrix as vector (n**2 elements)
* 21.05.2013 Walter Pachl
**********************************************************************/
  Parse Arg as
  n=sqrt(words(as))
  Do i=1 To n
    Do j=1 To n
      Parse Var as a.i.j as
      End
    End
  Select
    When n=2 Then det=a.1.1*a.2.2-a.1.2*a.2.1
    When n=3 Then det= a.1.1*a.2.2*a.3.3,
                      +a.1.2*a.2.3*a.3.1,
                      +a.1.3*a.2.1*a.3.2,
                      -a.1.3*a.2.2*a.3.1,
                      -a.1.2*a.2.1*a.3.3,
                      -a.1.1*a.2.3*a.3.2
    Otherwise Do
      det=0
      Do k=1 To n
        det=det+((-1)**(k+1))*a.1.k*determinant(subm(k))
        End
      End
    End
  Return det

subm: Procedure Expose a. n
/**********************************************************************
* compute the submatrix resulting when row 1 and column k are removed
* Input: a.*.*, k
* Output: bs the representation of the submatrix as vector
**********************************************************************/
  Parse Arg k
  bs=''
  do i=2 To n
    Do j=1 To n
      If j=k Then Iterate
      bs=bs a.i.j
      End
    End
  Return bs

sqrt: Procedure
/**********************************************************************
* compute and return the (integer) square root of the given argument
* terminate the program if the argument is not a square
**********************************************************************/
  Parse Arg nn
  Do n=1 By 1 while n*n<nn
    End
  If n*n=nn Then
    Return n
  Else Do
    Say 'invalid number of elements:' nn 'is not a square.'
    Exit
    End
```



```rexx
/* REXX ***************************************************************
* permanent.rex
* compute the permanent of a matrix
* I found an algorithm here:
* http://www.codeproject.com/Articles/21282/Compute-Permanent-of-a-Matrix-with-Ryser-s-Algorit
* see there for the original author.
* translated it to REXX (hopefully correctly) to REXX
* and believe that I can "publish" it here, on rosettacode
* when I look at the copyright rules shown there:
* http://www.codeproject.com/info/cpol10.aspx
* 20.05.2013 Walter Pachl
**********************************************************************/
Call init arg(1)                 /* initialize the matrix (n and a.* */
sum=0
rowsumprod=0
rowsum=0
chi.=0
c=2**n
Do k=1 To c-1                       /* loop all 2^n submatrices of A */
  rowsumprod = 1
  chis=dec2binarr(k,n)              /* characteristic vector         */
  Do ci=0 By 1 While chis<>''
    Parse Var chis chi.ci chis
    End
  Do m=0 To n-1                     /* loop columns of submatrix #k  */
    rowsum = 0
    Do p=0 To n-1                   /* loop rows and compute rowsum  */
      mnp=m*n+p
      rowsum=rowsum+chi.p*A.mnp
      End
    rowsumprod=rowsumprod*rowsum  /* update product of rowsums     */
                            /* (optional -- use for sparse matrices) */
                            /* if (rowsumprod == 0) break;           */
    End
  sum=sum+((-1)**(n-chi.n))*rowsumprod
  End
Return sum
/**********************************************************************
* Notes
* 1.The submatrices are chosen by use of a characteristic vector chi
* (only the columns are considered, where chi[p] == 1).
* To retrieve the t from Ryser's formula, we need to save the number
* n-t, as is done in chi[n]. Then we get t = n - chi[n].
* 2.The matrix parameter A is expected to be a one-dimensional integer
* array -- should the matrix be encoded row-wise or column-wise?
* -- It doesn't matter. The permanent is invariant under
* row-switching and column-switching, and it is Screenshot
* - per_inv.gif .
* 3.Further enhancements: If any rowsum equals zero,
* the entire rowsumprod becomes zero, and thus the m-loop can be broken.
* Since if-statements are relatively expensive compared to integer
* operations, this might save time only for sparse matrices
* (where most entries are zeros).
* 4.If anyone finds a polynomial algorithm for permanents,
* he will get rich and famous (at least in the computer science world).
**********************************************************************/
/**********************************************************************
* At first, we need to transform a decimal to a binary array
* with an additional element
* (the last one) saving the number of ones in the array:
**********************************************************************/
dec2binarr: Procedure
  Parse Arg n,dim
  ol='n='n 'dim='dim
  res.=0
  pos=dim-1
  Do While n>0
    res.pos=n//2
    res.dim=res.dim+res.pos
    n=n%2
    pos=pos-1
    End
  res_s=''
  Do i=0 To dim
    res_s=res_s res.i
    End
  Return res_s

init: Procedure Expose a. n
/**********************************************************************
* a.* (starting with index 0) contains all array elements
* n is the dimension of the square matrix
**********************************************************************/
Parse Arg as
n=sqrt(words(as))
a.=0
Do ai=0 By 1 While as<>''
  Parse Var as a.ai as
  End
Return

sqrt: Procedure
/**********************************************************************
* compute and return the (integer) square root of the given argument
* terminate the program if the argument is not a square
**********************************************************************/
  Parse Arg nn
  Do n=1 By 1 while n*n<nn
    End
  If n*n=nn Then
    Return n
  Else Do
    Say 'invalid number of elements:' nn 'is not a square.'
    Exit
    End
```


Output:

```txt
   1   2
   3   4
determinant=     -2
  permanent=     10
--------------------------------------------------
   1   2   3   4
   4   5   6   7
   7   8   9  10
  10  11  12  13
determinant=      0
  permanent=  29556
--------------------------------------------------
   0   1   2   3   4
   5   6   7   8   9
  10  11  12  13  14
  15  16  17  18  19
  20  21  22  23  24
determinant=      0
  permanent=6778800
--------------------------------------------------
```



## Ruby

Matrix in the standard library provides a method for the determinant, but not for the permanent.

```ruby
require 'matrix'

class Matrix
  # Add "permanent" method to Matrix class
  def permanent
    r = (0...row_count).to_a # [0,1] (first example), [0,1,2,3] (second example)
    r.permutation.inject(0) do |sum, sigma|
       sum += sigma.zip(r).inject(1){|prod, (row, col)| prod *= self[row, col] }
    end
  end
end

m1 = Matrix[[1,2],[3,4]] # testcases from Python version

m2 = Matrix[[1, 2, 3, 4], [4, 5, 6, 7], [7, 8, 9, 10], [10, 11, 12, 13]]

m3 = Matrix[[0, 1, 2, 3, 4],
            [5, 6, 7, 8, 9],
            [10, 11, 12, 13, 14],
            [15, 16, 17, 18, 19],
            [20, 21, 22, 23, 24]]

[m1, m2, m3].each do |m|
  puts "determinant:\t #{m.determinant}", "permanent:\t #{m.permanent}"
  puts
end
```

```txt

determinant:	 -2
permanent:	 10

determinant:	 0
permanent:	 29556

determinant:	 0
permanent:	 6778800

```


## Scala


```scala

def permutationsSgn[T]: List[T] => List[(Int,List[T])] = {
  case Nil => List((1,Nil))
  case xs => {
    for {
      (x, i) <- xs.zipWithIndex
      (sgn,ys) <- permutationsSgn(xs.take(i) ++ xs.drop(1 + i))
    } yield {
      val sgni = sgn * (2 * (i%2) - 1)
      (sgni, (x :: ys))
    }
  }
}

def det(m:List[List[Int]]) = {
  val summands =
    for {
      (sgn,sigma) <- permutationsSgn((0 to m.length - 1).toList).toList
    }
    yield {
      val factors =
        for (i <- 0 to (m.length - 1))
        yield m(i)(sigma(i))
      factors.toList.foldLeft(sgn)({case (x,y) => x * y})
    }
  summands.toList.foldLeft(0)({case (x,y) => x + y})

```



## Sidef

The `determinant` method is provided by the Array class.
```ruby
class Array {
    method permanent {
        var r = @^self.len

        var sum = 0
        r.permutations { |*a|
            var prod = 1
            [a,r].zip {|row,col| prod *= self[row][col] }
            sum += prod
        }

        return sum
    }
}

var m1 = [[1,2],[3,4]]

var m2 = [[1, 2, 3, 4],
          [4, 5, 6, 7],
          [7, 8, 9, 10],
          [10, 11, 12, 13]]

var m3 = [[0, 1, 2, 3, 4],
          [5, 6, 7, 8, 9],
          [10, 11, 12, 13, 14],
          [15, 16, 17, 18, 19],
          [20, 21, 22, 23, 24]]

[m1, m2, m3].each { |m|
  say "determinant:\t #{m.determinant}\npermanent:\t #{m.permanent}\n"
}
```

```txt
determinant:	 -2
permanent:	 10

determinant:	 0
permanent:	 29556

determinant:	 0
permanent:	 6778800

```



## Simula


```simula
! MATRIX ARITHMETIC ;
BEGIN

    INTEGER PROCEDURE LENGTH(A); ARRAY A;
        LENGTH := UPPERBOUND(A, 1) - LOWERBOUND(A, 1) + 1;

    ! Set MAT to the first minor of A dropping row X and column Y ;
    PROCEDURE MINOR(A, X, Y, MAT); ARRAY A, MAT; INTEGER X, Y;
    BEGIN
        INTEGER I, J, rowA, M; M := LENGTH(A) - 1; ! not a constant;
        FOR I := 1 STEP 1 UNTIL M DO BEGIN
            rowA := IF I < X THEN I ELSE I + 1;
            FOR J := 1 STEP 1 UNTIL M DO
                MAT(I, J) := A(rowA, IF J < Y THEN J else J + 1);
        END
    END MINOR;

    REAL PROCEDURE DET(A); REAL ARRAY A;
    BEGIN
        INTEGER N; N := LENGTH(A);
        IF N = 1 THEN
            DET := A(1, 1)
        ELSE
        BEGIN
            INTEGER I, SIGN;
            REAL SUM;
            SIGN := 1;
            FOR I := 1 STEP 1 UNTIL N DO
            BEGIN
                REAL ARRAY MAT(1:N-1, 1:N-1);
                MINOR(A, 1, I, MAT);
                SUM := SUM + SIGN * A(1, I) * DET(MAT);
                SIGN := SIGN * -1
            END;
            DET := SUM
        END
    END DET;

    REAL PROCEDURE PERM(A); REAL ARRAY A;
    BEGIN
        INTEGER N; N := LENGTH(A);
        IF N = 1 THEN
            PERM := A(1, 1)
        ELSE
        BEGIN
            REAL SUM;
            INTEGER I;

            FOR I := 1 STEP 1 UNTIL N DO
            BEGIN
                REAL ARRAY MAT(1:N-1, 1:N-1);
                MINOR(A, 1, I, MAT);
                SUM := SUM  + A(1, I) * PERM(MAT)
            END;
            PERM := SUM
        END
    END PERM;

    INTEGER SIZE;
    SIZE := ININT;
    BEGIN
        REAL ARRAY A(1:SIZE, 1:SIZE);
        INTEGER I, J;

        FOR I := 1 STEP 1 UNTIL SIZE DO BEGIN
            ! may be need here: INIMAGE;
            FOR J := 1 STEP 1 UNTIL SIZE DO
                A(I, J) := INREAL
        END;
        OUTTEXT("DETERMINANT ... : "); OUTREAL(DET (A), 10, 20); OUTIMAGE;
        OUTTEXT("PERMANENT ..... : "); OUTREAL(PERM(A), 10, 20); OUTIMAGE;
    END

    COMMENT  THE FIRST INPUT IS THE SIZE OF THE MATRIX, FOR EXAMPLE:

    ! 2
    ! 1 2
    ! 3 4
    ! DETERMINANT: -2.0
    ! PERMANENT: 10.0 ;

    COMMENT
    ! 5
    ! 0 1 2 3 4
    ! 5 6 7 8 9
    ! 10 11 12 13 14
    ! 15 16 17 18 19
    ! 20 21 22 23 24
    ! DETERMINANT: 0.0
    ! PERMANENT: 6778800.0 ;

END
```

Input:

```txt

2
1 2
3 4

```

```txt

DETERMINANT ... :    -2.000000000&+000
PERMANENT ..... :     1.000000000&+001

```

Input:

```txt

5
0 1 2 3 4
5 6 7 8 9
10 11 12 13 14
15 16 17 18 19
20 21 22 23 24

```

```txt

DETERMINANT ... :     0.000000000&+000
PERMANENT ..... :     6.778800000&+006

```



## SPAD

```SPAD
(1) -> M:=matrix [[2, 9, 4], [7, 5, 3], [6, 1, 8]]

        +2  9  4+
        |       |
   (1)  |7  5  3|
        |       |
        +6  1  8+
                                                        Type: Matrix(Integer)
(2) -> determinant M

   (2)  - 360
                                                                Type: Integer
(3) -> permanent M

   (3)  900
                                                        Type: PositiveInteger
```


[http://fricas.github.io/api/Matrix.html?highlight=matrix Domain:Matrix(R)]


## Stata


Two auxiliary functions: '''range1(n,i)''' returns the column vector with numbers 1 to n except i is removed.
And '''submat(a,i,j)''' returns matrix a with row i and column j removed.
For x=-1, the main function '''sumrec(a,x)''' computes the determinant of a by developing the determinant along the first column. For x=1, one gets the permanent.

<lang>real vector range1(real scalar n, real scalar i) {
	if (i < 1 | i > n) {
		return(1::n)
	} else if (i == 1) {
		return(2::n)
	} else if (i == n) {
		return(1::n-1)
	} else {
		return(1::i-1\i+1::n)
	}
}

real matrix submat(real matrix a, real scalar i, real scalar j) {
	return(a[range1(rows(a), i), range1(cols(a), j)])
}

real scalar sumrec(real matrix a, real scalar x) {
	real scalar n, s, p
	n = rows(a)
	if (n==1) return(a[1,1])
	s = 0
	p = 1
	for (i=1; i<=n; i++) {
		s = s+p*a[i,1]*sumrec(submat(a, i, 1), x)
		p = p*x
	}
	return(s)
}
```


Example:


```stata
: a=1,1,1,0\1,1,0,1\1,0,1,1\0,1,1,1
: a
[symmetric]
       1   2   3   4
    +-----------------+
  1 |  1              |
  2 |  1   1          |
  3 |  1   0   1      |
  4 |  0   1   1   1  |
    +-----------------+

: det(a)
  -3

: sumrec(a,-1)
  -3

: sumrec(a,1)
  9
```



## Tcl

The determinant is provided by the linear algebra package in Tcllib.
The permanent (being somewhat less common) requires definition, but is easily described:
```tcl
package require math::linearalgebra
package require struct::list

proc permanent {matrix} {
    for {set plist {};set i 0} {$i<[llength $matrix]} {incr i} {
	lappend plist $i
    }
    foreach p [::struct::list permutations $plist] {
	foreach i $plist j $p {
	    lappend prod [lindex $matrix $i $j]
	}
	lappend sum [::tcl::mathop::* {*}$prod[set prod {}]]
    }
    return [::tcl::mathop::+ {*}$sum]
}
```

Demonstrating with a sample matrix:

```tcl
set mat {
    {1 2 3 4}
    {4 5 6 7}
    {7 8 9 10}
    {10 11 12 13}
}
puts [::math::linearalgebra::det $mat]
puts [permanent $mat]
```

```txt

1.1315223609263888e-29
29556

```



## VBA

As an extra, the results of the built in WorksheetFuction.MDeterm are shown. The latter does not work for scalars.

```vb
Option Base 1
Private Function minor(a As Variant, x As Integer, y As Integer) As Variant
    Dim l As Integer: l = UBound(a) - 1
    Dim result() As Double
    If l > 0 Then ReDim result(l, l)
    For i = 1 To l
        For j = 1 To l
            result(i, j) = a(i - (i >= x), j - (j >= y))
        Next j
    Next i
    minor = result
End Function

Private Function det(a As Variant)
    If IsArray(a) Then
        If UBound(a) = 1 Then
            On Error GoTo err
            det = a(1, 1)
            Exit Function
        End If
    Else
        det = a
        Exit Function
    End If
    Dim sgn_ As Integer: sgn_ = 1
    Dim res As Integer: res = 0
    Dim i As Integer
    For i = 1 To UBound(a)
        res = res + sgn_ * a(1, i) * det(minor(a, 1, i))
        sgn_ = sgn_ * -1
    Next i
    det = res
    Exit Function
err:
    det = a(1)
End Function

Private Function perm(a As Variant) As Double
    If IsArray(a) Then
        If UBound(a) = 1 Then
            On Error GoTo err
            perm = a(1, 1)
            Exit Function
        End If
    Else
        perm = a
        Exit Function
    End If
    Dim res As Double
    Dim i As Integer
    For i = 1 To UBound(a)
        res = res + a(1, i) * perm(minor(a, 1, i))
    Next i
    perm = res
    Exit Function
err:
    perm = a(1)
End Function

Public Sub main()
    Dim tests(13) As Variant
    tests(1) = [{1,  2; 3,  4}]
    '--Determinant: -2, permanent: 10
    tests(2) = [{2, 9, 4; 7, 5, 3; 6, 1, 8}]
    '--Determinant: -360, permanent: 900
    tests(3) = [{ 1,  2,  3,  4; 4,  5,  6,  7; 7,  8,  9, 10; 10, 11, 12, 13}]
    '--Determinant: 0, permanent: 29556
    tests(4) = [{ 0,  1,  2,  3,  4; 5,  6,  7,  8,  9; 10, 11, 12, 13, 14; 15, 16, 17, 18, 19; 20, 21, 22, 23, 24}]
    '--Determinant: 0, permanent: 6778800
    tests(5) = [{5}]
    '--Determinant: 5, permanent: 5
    tests(6) = [{1,0,0; 0,1,0; 0,0,1}]
    '--Determinant: 1, permanent: 1
    tests(7) = [{0,0,1; 0,1,0; 1,0,0}]
    '--Determinant: -1, Permanent: 1
    tests(8) = [{4,3; 2,5}]
    '--Determinant: 14, Permanent: 26
    tests(9) = [{2,5; 4,3}]
    '--Determinant: -14, Permanent: 26
    tests(10) = [{4,4; 2,2}]
    '--Determinant: 0, Permanent: 16
    tests(11) = [{7,    2,      -2,     4; 4,    4,      1,      7; 11,   -8,     9,      10; 10,   5,      12,     13}]
    '--det:  -4319   permanent:      10723
    tests(12) = [{-2,   2,      -3; -1,   1,      3; 2 ,   0,      -1}]
    '--det:  18      permanent:      10
    tests(13) = 13
    Debug.Print "Determinant", "Builtin det", "Permanent"
    For i = 1 To 12
        Debug.Print det(tests(i)), WorksheetFunction.MDeterm(tests(i)), perm(tests(i))
    Next i
    Debug.Print det(tests(13)), "error", perm(tests(13))
End Sub
```
```txt
Determinant   Builtin det   Permanent
-2            -2             10
-360          -360           900
 0             0             29556
 0             0             6778800
 5             5             5
 1             1             1
-1            -1             1
 14            14            26
-14           -14            26
 0             0             16
-4319         -4319          10723
 18            18            10
 13           error          13
```


## zkl


```zkl
var [const] GSL=Import("zklGSL");	// libGSL (GNU Scientific Library)
fcn perm(A){  // should verify A is square
   numRows:=A.rows;
   Utils.Helpers.permute(numRows.toList()).reduce(  // permute(0,1,..numRows)
      'wrap(s,pm){ s + numRows.reduce('wrap(x,i){ x*A[i,pm[i]] },1.0) },
      0.0)
}
test:=fcn(A){
   println(A.format());
   println("Permanent: %.2f, determinant: %.2f".fmt(perm(A),A.det()));
};
```


```zkl
A:=GSL.Matrix(2,2).set(1,2, 3,4);
B:=GSL.Matrix(4,4).set(1,2,3,4, 4,5,6,7, 7,8,9,10, 10,11,12,13);
C:=GSL.Matrix(5,5).set( 0, 1, 2, 3, 4,  5, 6, 7, 8, 9, 10,11,12,13,14,
		       15,16,17,18,19, 20,21,22,23,24);
T(A,B,C).apply2(test);
```

```txt

      1.00,      2.00
      3.00,      4.00
Permanent: 10.00, determinant: -2.00
      1.00,      2.00,      3.00,      4.00
      4.00,      5.00,      6.00,      7.00
      7.00,      8.00,      9.00,     10.00
     10.00,     11.00,     12.00,     13.00
Permanent: 29556.00, determinant: 0.00
      0.00,      1.00,      2.00,      3.00,      4.00
      5.00,      6.00,      7.00,      8.00,      9.00
     10.00,     11.00,     12.00,     13.00,     14.00
     15.00,     16.00,     17.00,     18.00,     19.00
     20.00,     21.00,     22.00,     23.00,     24.00
Permanent: 6778800.00, determinant: 0.00

```


