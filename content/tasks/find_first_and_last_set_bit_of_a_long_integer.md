+++
title = "Find first and last set bit of a long integer"
description = ""
date = 2019-04-30T00:12:45Z
aliases = []
[extra]
id = 11018
[taxonomies]
categories = ["task"]
tags = []
+++

''Clarification: This task is asking for the position of two bits in the binary representation of a positive integer.  Some parts of this task assume that this is the native representation in the language you are working in.  Any part of this task which makes assumptions about native representation should be treated as a recommendation which is only relevant in some contexts.  A bit is defined as the exponent in a binary polynomial -- an exponent corresponding to a power of 2 which has a non-zero multiplier in the summation sequence of powers of two which yields the desired positive integer, where the only allowed coefficients are 0 and 1.''

Define routines (or operators) '''lwb''' and '''upb''' that find the ''first'' and ''last'' set bit in a binary value.  Implement using a binary search to find the location of the particular upper/lower bit.

Also: Define the reverse routines (or operators) '''rlwb''' and '''rupb''' that find host's ''positive integers'' least- and most-significant set bit in a binary value expressed in [[wp:Bit_numbering#LSB_0_bit_numbering|LSB 0 bit numbering]], i.e. indexed from the extreme right bit.

Use primarily '''bit''' operations, such as '''and''', '''or''', and bit shifting.  Avoid additions, multiplications and especially avoid divisions.


;Two implementations:
# For the host word size on the host platform, implement the routine "efficiently" in without looping or recursion.
# For the extended precision/long word implement the algorithm more generally - maybe as a template, and maybe with looping - so that any ''bits width'' for a binary type can be accommodated.


;Test cases:
# For the host machine word size: Use the powers of 42 up to host's the "natural" word size to calculate the index of the first and last set bit.
# For the extended precision: Use the powers of 1302 up to the host's next "natural" '''long''' host ''word'' size to calculate the index of the first and last set bit.
# Output bit indexes in [[wp:Bit_numbering#LSB_0_bit_numbering|LSB 0 bit numbering]].


;Additionally:
In a particular language, there maybe (at least) two alternative approaches of calculating the required values:
* Using an external library.
* Using a built-in library.


If any of these approaches are available, then ''also'' note the library or built-in name.


## See also

* [http://graphics.stanford.edu/~seander/bithacks.html#IntegerLog Find the log base 2 of an N-bit integer in O(lg(N)) operations]
* [http://pdos.csail.mit.edu/6.858/2011/readings/i386/BSF.htm 80386 Instruction Set - BSF -- Bit Scan Forward]





## ALGOL 68

'''File: Template.Find_first_and_last_set_bit.a68'''
```algol68
INT lbits width = UPB []BOOL(LBITS(2r0));

OP LWB = (BITS in x)INT: bits width - RUPB in x;

OP RUPB = (BITS in x)INT:
  ### 32 bit LWB Find Lower Set Bit using an unrolled loop ###
# Note: BITS ELEM 1 is actually numerically the Most Significant Bit!! #
  IF in x = 2r0 THEN
    -1 # EXIT #
  ELSE
    BITS x := in x, out := 2r0;
    IF(x AND NOT 2r1111111111111111)/=2r0 THEN x := x SHR 16; out := out OR 2r10000 FI;
    IF(x AND NOT 2r11111111)        /=2r0 THEN x := x SHR  8; out := out OR 2r1000 FI;
    IF(x AND NOT 2r1111)            /=2r0 THEN x := x SHR  4; out := out OR 2r100 FI;
    IF(x AND NOT 2r11)              /=2r0 THEN x := x SHR  2; out := out OR 2r10 FI;
    IF(x AND NOT 2r1)               /=2r0 THEN                out := out OR 2r1 FI;
    ABS out # EXIT #
  FI;

OP LWB = (LBITS in x)INT: lbits width - RUPB in x;

OP RUPB = (LBITS in x)INT:
  ### Generalised Find Lower Set Bit using a loop ###
# Note: BITS ELEM 32 is actually numerically the Least Significant Bit!! #
  IF in x = 2r0 THEN
    -1 # EXIT #
  ELSE
    LBITS x := in x;
    BITS
      out bit := BIN 1 SHL (bits width - LWB BIN lbits width),
      out := BIN 0;
    WHILE
      LBITS mask := NOT BIN (ABS (LONG 2r1 SHL ABS out bit) - 1);
      IF(x AND mask) /= 2r0 THEN
        x := x SHR ABS out bit;
        out := out OR out bit FI;
      out bit := out bit SHR 1;
  # WHILE # out bit /= 2r0 DO SKIP OD;
    ABS out # EXIT #
  FI;

OP UPB = (BITS in x)INT: bits width - RLWB in x;

OP RLWB = (BITS in x)INT:
  ### 32 bit Find Upper Set Bit using an unrolled loop ###
# Note: BITS ELEM 1 is actually numerically the Most Significant Bit!! #
  IF in x = 2r0 THEN
    0 # EXIT #
  ELSE
    BITS x := in x, out := 2r0;
    IF(x AND 2r1111111111111111)=2r0 THEN x := x SHR 16; out := out OR 2r10000 FI;
    IF(x AND 2r11111111)        =2r0 THEN x := x SHR  8; out := out OR 2r1000 FI;
    IF(x AND 2r1111)            =2r0 THEN x := x SHR  4; out := out OR 2r100 FI;
    IF(x AND 2r11)              =2r0 THEN x := x SHR  2; out := out OR 2r10 FI;
    IF(x AND 2r1)               =2r0 THEN                out := out OR 2r1 FI;
    ABS out # EXIT #
  FI;

OP UPB = (LBITS in x)INT: lbits width - RLWB in x;

OP RLWB = (LBITS in x)INT:
  ### Generalised Find Upper Set Bit using a loop ###
# Note: BITS ELEM 1 is actually numerically the Most Significant Bit!! #
  IF in x = 2r0 THEN
    0 # EXIT #
  ELSE
    LBITS x := in x;
    BITS
      out bit := BIN 1 SHL (bits width - LWB BIN lbits width),
      out := BIN 0;
    WHILE
      LBITS mask := BIN (ABS (LONG 2r1 SHL ABS out bit) - 1);
      IF(x AND mask) = 2r0 THEN
        x := x SHR ABS out bit;
        out := out OR out bit FI;
      out bit := out bit SHR 1;
  # WHILE # out bit /= 2r0 DO SKIP OD;
    ABS out # EXIT #
  FI;
```
'''File: test.Find_first_and_last_set_bit.a68'''
```algol68
#!/usr/local/bin/a68g --script #

MODE LBITS = LONG BITS;
PR READ "Template.Find_first_and_last_set_bit.a68" PR

INT bits of prod;
FORMAT header fmt = $g 36k"|RLWB|RUPB|Bits"l$;
FORMAT row fmt0 = $g(-35)"|"2(g(-3)" |"),2rd l$;
FORMAT row fmt  = $g(-35)"|"2(g(-3)" |"),2rn(bits of prod+1)d l$;

test int:(
  printf((header fmt, "INT: find first & last set bit"));
  INT prod := 0;

  # test case 0 #
  prod := 0; bits of prod := RUPB BIN prod;
  printf((row fmt0, prod, RLWB BIN prod, RUPB BIN prod, BIN prod));

  prod := 1; # test case 1 etc ... #
  INT zoom := 2 * 3 * 7;
  WHILE
    bits of prod := RUPB BIN prod;
    printf((row fmt, prod, RLWB BIN prod, RUPB BIN prod, BIN prod));
# WHILE # prod <=      max int / zoom DO
    prod *:= zoom
  OD
);

test long int:(
  printf(($l$,header fmt, "LONG INT:"));
  LONG INT prod := 0;

  # test case 0 #
  prod := 0; bits of prod := RUPB BIN prod;
  printf((row fmt0, prod, RLWB BIN prod, RUPB BIN prod, BIN prod));

  prod := 1; # test case 1 etc ... #
  INT zoom := 2 * 3 * 7 * 31;
  WHILE
    bits of prod := RUPB BIN prod;
    printf((row fmt, prod, RLWB BIN prod, RUPB BIN prod, BIN prod));
# WHILE # prod <= long max int / zoom DO
    prod *:= zoom
  OD
)
```
'''Output:'''

```txt

INT: find first & last set bit     |RLWB|RUPB|Bits
                                  0|  0 | -1 |0
                                  1|  0 |  0 |1
                                 42|  1 |  5 |101010
                               1764|  2 | 10 |11011100100
                              74088|  3 | 16 |10010000101101000
                            3111696|  4 | 21 |1011110111101100010000
                          130691232|  5 | 26 |111110010100011000010100000

LONG INT:                          |RLWB|RUPB|Bits
                                  0|  0 | -1 |0
                                  1|  0 |  0 |1
                               1302|  1 | 10 |10100010110
                            1695204|  2 | 20 |110011101110111100100
                         2207155608|  3 | 31 |10000011100011101000010110011000
                      2873716601616|  4 | 41 |101001110100010110110110110111001100010000
                   3741579015304032|  5 | 51 |1101010010101111001001000000000110110011001101100000
                4871535877925849664|  6 | 62 |100001110011011001011000001001000001010010101110100101001000000
             6342739713059456262528|  7 | 72 |1010101111101011100110010001000111100000010010111111100111010000110000000
          8258247106403412053811456|  8 | 82 |11011010100110000000111100100000001110101011000010011010001000101110110000100000000
      10752237732537242494062515712|  9 | 93 |1000101011111000001010111001110110111101010011111100010111111101101100111001110101011000000000
   13999413527763489727269395457024| 10 |103 |10110000101100101000101101110101000100000011010011101110001111100001001111100000100011110110010000000000
18227236413148063624904752885045248| 11 |113 |111000001010101100000100010100010101100000011011010011001110101111101110010001100000011001010001101001100000000000

```



## AutoHotkey


```AutoHotkey
loop, 12{
	First := Last := ""
	n:=42**(A_Index-1)
	while (n>v)
		if (n&v := 2**(A_Index-1))
			First := First ? First : A_Index-1 , Last := A_Index-1
	Res .= 42 "^" A_Index-1 " --> First : " First " , Last : " Last "`n"
}
MsgBox % Res
```

Outputs:
```txt
42^0 --> First : 0 , Last : 0
42^1 --> First : 1 , Last : 5
42^2 --> First : 2 , Last : 10
42^3 --> First : 3 , Last : 16
42^4 --> First : 4 , Last : 21
42^5 --> First : 5 , Last : 26
42^6 --> First : 6 , Last : 32
42^7 --> First : 7 , Last : 37
42^8 --> First : 8 , Last : 43
42^9 --> First : 9 , Last : 48
42^10 --> First : 10 , Last : 53
42^11 --> First : 11 , Last : 59
```



## C




```c
#include <stdio.h>
#include <stdint.h>

uint32_t msb32(uint32_t n)
{
	uint32_t b = 1;
	if (!n) return 0;

#define step(x) if (n >= ((uint32_t)1) << x) b <<= x, n >>= x
	step(16); step(8); step(4); step(2); step(1);
#undef step
	return b;
}

int msb32_idx(uint32_t n)
{
	int b = 0;
	if (!n) return -1;

#define step(x) if (n >= ((uint32_t)1) << x) b += x, n >>= x
	step(16); step(8); step(4); step(2); step(1);
#undef step
	return b;
}

#define lsb32(n) ( (uint32_t)(n) & -(int32_t)(n) )

/* finding the *position* of the least significant bit
   rarely makes sense, so we don't put much effort in it*/
inline int lsb32_idx(uint32_t n) { return msb32_idx(lsb32(n)); }

int main()
{
	int32_t n;
	int i;

	for (i = 0, n = 1; ; i++, n *= 42) {
		printf("42**%d = %10d(x%08x): M x%08x(%2d) L x%03x(%2d)\n",
			i, n, n,
			msb32(n), msb32_idx(n),
			lsb32(n), lsb32_idx(n));

		if (n >= INT32_MAX / 42) break;
	}

	return 0;
}
```
output ("x###" are in base 16)

```txt

42**0 =          1(x00000001): M x00000001( 0) L x001( 0)
42**1 =         42(x0000002a): M x00000020( 5) L x002( 1)
42**2 =       1764(x000006e4): M x00000400(10) L x004( 2)
42**3 =      74088(x00012168): M x00010000(16) L x008( 3)
42**4 =    3111696(x002f7b10): M x00200000(21) L x010( 4)
42**5 =  130691232(x07ca30a0): M x04000000(26) L x020( 5)

```



### GCC extension

```c
#include <stdio.h>
#include <limits.h>

int msb_int(unsigned int x) {
	int ret = sizeof(unsigned int) * CHAR_BIT - 1;
	return x ? ret - __builtin_clz(x) : ret;
}

int msb_long(unsigned long x) {
	int ret = sizeof(unsigned long) * CHAR_BIT - 1;
	return x ? ret - __builtin_clzl(x) : ret;
}

int msb_longlong(unsigned long long x) {
	int ret = sizeof(unsigned long long) * CHAR_BIT - 1;
	return x ? ret - __builtin_clzll(x) : ret;
}

#define lsb_int(x)	(__builtin_ffs(x) - 1)
#define lsb_long(x)	(__builtin_ffsl(x) - 1)
#define lsb_longlong(x) (__builtin_ffsll(x) - 1)

int main()
{
	int i;

        printf("int:\n");
	unsigned int n1;
	for (i = 0, n1 = 1; ; i++, n1 *= 42) {
		printf("42**%d = %10u(x%08x): M %2d L %2d\n",
			i, n1, n1,
			msb_int(n1),
			lsb_int(n1));

		if (n1 >= UINT_MAX / 42) break;
	}

        printf("long:\n");
	unsigned long n2;
	for (i = 0, n2 = 1; ; i++, n2 *= 42) {
		printf("42**%02d = %20lu(x%016lx): M %2d L %2d\n",
			i, n2, n2,
			msb_long(n2),
			lsb_long(n2));

		if (n2 >= ULONG_MAX / 42) break;
	}

	return 0;
}
```
output ("x###" are in base 16)

```txt

int:
42**0 =          1(x00000001): M  0 L  0
42**1 =         42(x0000002a): M  5 L  1
42**2 =       1764(x000006e4): M 10 L  2
42**3 =      74088(x00012168): M 16 L  3
42**4 =    3111696(x002f7b10): M 21 L  4
42**5 =  130691232(x07ca30a0): M 26 L  5
long:
42**00 =                    1(x0000000000000001): M  0 L  0
42**01 =                   42(x000000000000002a): M  5 L  1
42**02 =                 1764(x00000000000006e4): M 10 L  2
42**03 =                74088(x0000000000012168): M 16 L  3
42**04 =              3111696(x00000000002f7b10): M 21 L  4
42**05 =            130691232(x0000000007ca30a0): M 26 L  5
42**06 =           5489031744(x00000001472bfa40): M 32 L  6
42**07 =         230539333248(x00000035ad370e80): M 37 L  7
42**08 =        9682651996416(x000008ce6b086100): M 43 L  8
42**09 =      406671383849472(x000171dd8f5fea00): M 48 L  9
42**10 =    17080198121677824(x003cae5985bc6400): M 53 L 10
42**11 =   717368321110468608(x09f49aaff0e86800): M 59 L 11

```



## D

(This task is not complete, the second part will be added later.)

```d
import std.stdio, core.bitop, std.bigint;

void main() {
    enum size_t test = 42;
    for (size_t i = 0; true; i++) {
        immutable size_t x = test ^^ i;
        if (x != BigInt(test) ^^ i)
            break;
        writefln("%18d %0*b MSB: %2d LSB: %2d",
                 x, size_t.sizeof * 8, x, bsr(x), bsf(x));
    }
}
```

On a 32 bit system:

```txt
                 1 00000000000000000000000000000001 MSB:  0 LSB:  0
                42 00000000000000000000000000101010 MSB:  5 LSB:  1
              1764 00000000000000000000011011100100 MSB: 10 LSB:  2
             74088 00000000000000010010000101101000 MSB: 16 LSB:  3
           3111696 00000000001011110111101100010000 MSB: 21 LSB:  4
         130691232 00000111110010100011000010100000 MSB: 26 LSB:  5
```


On a 64 bit system:

```txt
                 1 0000000000000000000000000000000000000000000000000000000000000001 MSB:  0 LSB:  0
                42 0000000000000000000000000000000000000000000000000000000000101010 MSB:  5 LSB:  1
              1764 0000000000000000000000000000000000000000000000000000011011100100 MSB: 10 LSB:  2
             74088 0000000000000000000000000000000000000000000000010010000101101000 MSB: 16 LSB:  3
           3111696 0000000000000000000000000000000000000000001011110111101100010000 MSB: 21 LSB:  4
         130691232 0000000000000000000000000000000000000111110010100011000010100000 MSB: 26 LSB:  5
        5489031744 0000000000000000000000000000000101000111001010111111101001000000 MSB: 32 LSB:  6
      230539333248 0000000000000000000000000011010110101101001101110000111010000000 MSB: 37 LSB:  7
     9682651996416 0000000000000000000010001100111001101011000010000110000100000000 MSB: 43 LSB:  8
   406671383849472 0000000000000001011100011101110110001111010111111110101000000000 MSB: 48 LSB:  9
 17080198121677824 0000000000111100101011100101100110000101101111000110010000000000 MSB: 53 LSB: 10
717368321110468608 0000100111110100100110101010111111110000111010000110100000000000 MSB: 59 LSB: 11
```



## Fortran

Since the Fortran 2008 standard, the language has LEADZ and TRAILZ intrinsic functions that yield respectively the number of leading (i.e. HSB) and trailing (LSB) zero bits. This gives an immediate solution to the task.


```fortran
program bits
    implicit none
    integer :: n = 1, i

    do i = 1, 6
        print "(B32,2(' ',I2))", n, trailz(n), 31 - leadz(n)
        n = 42 * n
    end do
end program
```



```txt

                               1  0  0
                          101010  1  5
                     11011100100  2 10
               10010000101101000  3 16
          1011110111101100010000  4 21
     111110010100011000010100000  5 26

```



## Go

```go
package main

import (
    "fmt"
    "math/big"
)

const (
    mask0, bit0 = (1 << (1 << iota)) - 1, 1 << iota
    mask1, bit1
    mask2, bit2
    mask3, bit3
    mask4, bit4
    mask5, bit5
)

func rupb(x uint64) (out int) {
    if x == 0 {
        return -1
    }
    if x&^mask5 != 0 {
        x >>= bit5
        out |= bit5
    }
    if x&^mask4 != 0 {
        x >>= bit4
        out |= bit4
    }
    if x&^mask3 != 0 {
        x >>= bit3
        out |= bit3
    }
    if x&^mask2 != 0 {
        x >>= bit2
        out |= bit2
    }
    if x&^mask1 != 0 {
        x >>= bit1
        out |= bit1
    }
    if x&^mask0 != 0 {
        out |= bit0
    }
    return
}

func rlwb(x uint64) (out int) {
    if x == 0 {
        return 0
    }
    if x&mask5 == 0 {
        x >>= bit5
        out |= bit5
    }
    if x&mask4 == 0 {
        x >>= bit4
        out |= bit4
    }
    if x&mask3 == 0 {
        x >>= bit3
        out |= bit3
    }
    if x&mask2 == 0 {
        x >>= bit2
        out |= bit2
    }
    if x&mask1 == 0 {
        x >>= bit1
        out |= bit1
    }
    if x&mask0 == 0 {
        out |= bit0
    }
    return
}

// Big number versions of functions do not use the techniques of the ALGOL 68
// solution.  The big number version of rupb is trivial given one of the
// standard library functions, And for rlwb, I couldn't recommend shifting
// the whole input number when working with smaller numbers would do.
func rupbBig(x *big.Int) int {
    return x.BitLen() - 1
}

// Binary search, for the spirit of the task, but without shifting the input
// number x.  (Actually though, I don't recommend this either.  Linear search
// would be much faster.)
func rlwbBig(x *big.Int) int {
    if x.BitLen() < 2 {
        return 0
    }
    bit := uint(1)
    mask := big.NewInt(1)
    var ms []*big.Int
    var y, z big.Int
    for y.And(x, z.Lsh(mask, bit)).BitLen() == 0 {
        ms = append(ms, mask)
        mask = new(big.Int).Or(mask, &z)
        bit <<= 1
    }
    out := bit
    for i := len(ms) - 1; i >= 0; i-- {
        bit >>= 1
        if y.And(x, z.Lsh(ms[i], out)).BitLen() == 0 {
            out |= bit
        }
    }
    return int(out)
}

func main() {
    show()
    showBig()
}

func show() {
    fmt.Println("uint64:")
    fmt.Println("power              number  rupb  rlwb")
    const base = 42
    n := uint64(1)
    for i := 0; i < 12; i++ {
        fmt.Printf("%d^%02d %19d %5d %5d\n", base, i, n, rupb(n), rlwb(n))
        n *= base
    }
}

func showBig() {
    fmt.Println("\nbig numbers:")
    fmt.Println("  power                               number  rupb  rlwb")
    base := big.NewInt(1302)
    n := big.NewInt(1)
    for i := 0; i < 12; i++ {
        fmt.Printf("%d^%02d %36d %5d %5d\n", base, i, n, rupbBig(n), rlwbBig(n))
        n.Mul(n, base)
    }
}
```

```txt

uint64:
power              number  rupb  rlwb
42^00                   1     0     0
42^01                  42     5     1
42^02                1764    10     2
42^03               74088    16     3
42^04             3111696    21     4
42^05           130691232    26     5
42^06          5489031744    32     6
42^07        230539333248    37     7
42^08       9682651996416    43     8
42^09     406671383849472    48     9
42^10   17080198121677824    53    10
42^11  717368321110468608    59    11

big numbers:
  power                               number  rupb  rlwb
1302^00                                    1     0     0
1302^01                                 1302    10     1
1302^02                              1695204    20     2
1302^03                           2207155608    31     3
1302^04                        2873716601616    41     4
1302^05                     3741579015304032    51     5
1302^06                  4871535877925849664    62     6
1302^07               6342739713059456262528    72     7
1302^08            8258247106403412053811456    82     8
1302^09        10752237732537242494062515712    93     9
1302^10     13999413527763489727269395457024   103    10
1302^11  18227236413148063624904752885045248   113    11

```


=={{header|Icon}} and {{header|Unicon}}==
The task definition makes some assumptions that don't work in Icon/Unicon and are going to require some reinterpretation. In Icon/Unicon all integers appear to be implemented as a single common type.  A specific implementation may or may not have large integers, but if it does they are essentially indistinguishable from regular integers. Given all of this, implementing "efficient" procedures for the platform word size without loops or recursion makes little sense.

Instead of this, to meet the spirit of the task, these lsb and msb routines are generalized to reduce the integer in blocks of bits and then zoom in on the desired bit by binary search (i.e. successively looking a blocks that are half the size again).  The exponent for the initial power used to create the masks does not need to be itself a power of two.  The xsb_initial procedure uses introspection to determine the word size of a basic integer type.  This is used to build a mask that fits within the basic word size of the implementation. In this way we won't create unnecessary large integers through implicit type conversions.


```Icon
link printf,hexcvt

procedure main()
   every B := [42,2^32-1] | [1302,2^64-1] do {
      base := B[1]
      lim  := B[2]
      fmt := sprintf("%%i^%%i = %%%is (x%%0%is) : MSB=%%s LSB=%%s\n",*lim,*hexstring(lim))
      every e := seq(0) do {
         if (i := base^e) > lim then break
         printf(fmt,base,e,i,hexstring(i),msb(i)|"-",lsb(i)|"-")
      }
   }
end

procedure msb(i)  #: return the most significant set bit index or fail
static mask
initial mask := xsb_initial()

   if i > 0 then {
      b := 0
      every m := mask[j := 1 to *mask by 2] & r := mask[j+1] do {
         repeat {
            l := iand(i,m)
            i := ishift(i,r)
            if i = 0 then break
            b -:= r
            }
         i := l
         }
      return b
   }
end

procedure lsb(i)   #: return the least significant set bit index or fail
static mask
initial mask := xsb_initial()

   if i > 0 then {
      b := 0
      every m := mask[j := 1 to *mask by 2] & r := mask[j+1] do
         until iand(i,m) > 0 do {
            i := ishift(i,r)
            b -:= r
            }
      return b
   }
end

procedure xsb_initial() #: setup tables for lsb/msb
static mask
initial {                                          # build
      a := &allocated                              # bigint affects allocation
      p := if 2^63 & a=&allocated then 63 else 31  # find wordsize-1
      p *:= 2                                      # adjust pre-loop
      mask := []
      until (p := p / 2) = 0 do put(mask,2^p-1,-p) # list of masks and shifts
   }
   return mask                                     # return pre-built data
end
```


[http://www.cs.arizona.edu/icon/library/src/procs/printf.icn printf.icn provides formatting]
[http://www.cs.arizona.edu/icon/library/src/procs/hexcvt.icn hexcvt.icn provides hexstring]

Output:
```txt
42^0 =          1 (x00000001) : MSB=0 LSB=0
42^1 =         42 (x0000002A) : MSB=5 LSB=1
42^2 =       1764 (x000006E4) : MSB=10 LSB=2
42^3 =      74088 (x00012168) : MSB=16 LSB=3
42^4 =    3111696 (x002F7B10) : MSB=21 LSB=4
42^5 =  130691232 (x07CA30A0) : MSB=26 LSB=5
1302^0 =                    1 (x0000000000000001) : MSB=0 LSB=0
1302^1 =                 1302 (x0000000000000516) : MSB=10 LSB=1
1302^2 =              1695204 (x000000000019DDE4) : MSB=20 LSB=2
1302^3 =           2207155608 (x00000000838E8598) : MSB=31 LSB=3
1302^4 =        2873716601616 (x0000029D16DB7310) : MSB=41 LSB=4
1302^5 =     3741579015304032 (x000D4AF2401B3360) : MSB=51 LSB=5
1302^6 =  4871535877925849664 (x439B2C120A574A40) : MSB=62 LSB=6
```



## J


Implementation:


```j
lwb=: 0:
upb=: (#: i: 1:)"0
rlwb=: #@#:"0 - 1:
rupb=: rlwb - upb
```


Notes:

This implementation is agnostic to numeric storage format.

J's <code>#:</code> converts integers to bit lists.

lwb is the required name for the index of "first set bit in a binary value".  This is always zero here.  Here's why:


```J
   #: 7
1 1 1
   #: 8
1 0 0 0
   #: 20
1 0 1 0 0
   #: 789
1 1 0 0 0 1 0 1 0 1
   #:123456789123456789123456789x
1 1 0 0 1 1 0 0 0 0 1 1 1 1 0 1 1 1 1 1 1 0 1 1 1 1 1 0 0 1 0 1 1 1 0 0 0 1 1 1 0 1 1 0 0 0 1 1 0 0 1 1 1 1 1 0 1 1 1 1 1 0 0 0 0 0 0 0 1 0 0 0 1 0 1 1 1 1 1 0 0 0 1 0 1 0 1
```


The the first '''set''' bit in J's binary representation for a positive integer is always the first bit of that integer (there's an exception for zero, because it has no first set bit, but that's outside the domain of this task). That said, note that this would not hold for an arbitrary integer in a list of integers. But bit representations of lists of integers is outside the scope of this task.

And the index of the first bit will always be 0.

Example use:


```j
   (,.lwb,.upb,.rlwb,.rupb) <.i.@>.&.(42&^.) 2^64
                 1 0  0  0  0
                42 0  4  5  1
              1764 0  8 10  2
             74088 0 13 16  3
           3111696 0 17 21  4
         130691232 0 21 26  5
        5489031744 0 26 32  6
      230539333248 0 30 37  7
     9682651996416 0 35 43  8
   406671383849472 0 39 48  9
 17080198121677824 0 43 53 10
717368321110468608 0 48 59 11

   (,.lwb,.upb,.rlwb,.rupb) i.@x:@>.&.(1302&^.) 2^128
                                     1 0   0   0  0
                                  1302 0   9  10  1
                               1695204 0  18  20  2
                            2207155608 0  28  31  3
                         2873716601616 0  37  41  4
                      3741579015304032 0  46  51  5
                   4871535877925849664 0  56  62  6
                6342739713059456262528 0  65  72  7
             8258247106403412053811456 0  74  82  8
         10752237732537242494062515712 0  84  93  9
      13999413527763489727269395457024 0  93 103 10
   18227236413148063624904752885045248 0 102 113 11
23731861809918778839625988256328912896 0 112 124 12
```


Note, in the above sentences, the rightmost part of each sentence is about generating an arbitrary sequence of values.  The phrase <.i.@>.&.(42&^.) 2^64 generates the sequence 1 42 1764 74088 3111696 130691232 ... and the phrase i.@x:@>.&.(1302&^.) 2^128 generates the sequence 1 1302 1695204 2207155608 ...

The left part of each sentence uses the words we defined here, organizing their results as columns in a table.


## Java



###  Library

Notes:
* least significant bit is bit 0 (such that bit ''i'' always has value 2<sup>i</sup>, and the result is independent of integer type width)
* when the integer 0 is given, mssb() and lssb() both return no bits set; mssb_idx() and lssb_idx() will return -1 and the integer type width, respectively

```java
public class FirstLastBits {

    // most significant set bit
    public static int mssb(int x) {
        return Integer.highestOneBit(x);
    }

    public static long mssb(long x) {
        return Long.highestOneBit(x);
    }

    public static int mssb_idx(int x) {
        return Integer.SIZE - 1 - Integer.numberOfLeadingZeros(x);
    }

    public static int mssb_idx(long x) {
        return Long.SIZE - 1 - Long.numberOfLeadingZeros(x);
    }

    public static int mssb_idx(BigInteger x) {
	return x.bitLength() - 1;
    }

    // least significant set bit
    public static int lssb(int x) {
        return Integer.lowestOneBit(x);
    }

    public static long lssb(long x) {
        return Long.lowestOneBit(x);
    }

    public static int lssb_idx(int x) {
        return Integer.numberOfTrailingZeros(x);
    }

    public static int lssb_idx(long x) {
        return Long.numberOfTrailingZeros(x);
    }

    public static int lssb_idx(BigInteger x) {
	return x.getLowestSetBit();
    }

    public static void main(String[] args) {
        System.out.println("int:");
        int n1 = 1;
        for (int i = 0; ; i++, n1 *= 42) {
            System.out.printf("42**%d = %10d(x%08x): M x%08x(%2d) L x%03x(%2d)\n",
                              i, n1, n1,
                              mssb(n1), mssb_idx(n1),
                              lssb(n1), lssb_idx(n1));
            if (n1 >= Integer.MAX_VALUE / 42)
                break;
        }
        System.out.println();
        System.out.println("long:");
        long n2 = 1;
        for (int i = 0; ; i++, n2 *= 42) {
            System.out.printf("42**%02d = %20d(x%016x): M x%016x(%2d) L x%06x(%2d)\n",
                              i, n2, n2,
                              mssb(n2), mssb_idx(n2),
                              lssb(n2), lssb_idx(n2));
            if (n2 >= Long.MAX_VALUE / 42)
                break;
        }
	System.out.println();
	System.out.println("BigInteger:");
	BigInteger n3 = BigInteger.ONE;
	BigInteger k = BigInteger.valueOf(1302);
	for (int i = 0; i < 10; i++, n3 = n3.multiply(k)) {
	    System.out.printf("1302**%02d = %30d(x%28x): M %2d L %2d\n",
			      i, n3, n3,
			      mssb_idx(n3),
			      lssb_idx(n3));
	}
    }
}
```

output:

```txt

int:
42**0 =          1(x00000001): M x00000001( 0) L x001( 0)
42**1 =         42(x0000002a): M x00000020( 5) L x002( 1)
42**2 =       1764(x000006e4): M x00000400(10) L x004( 2)
42**3 =      74088(x00012168): M x00010000(16) L x008( 3)
42**4 =    3111696(x002f7b10): M x00200000(21) L x010( 4)
42**5 =  130691232(x07ca30a0): M x04000000(26) L x020( 5)

long:
42**00 =                    1(x0000000000000001): M x0000000000000001( 0) L x000001( 0)
42**01 =                   42(x000000000000002a): M x0000000000000020( 5) L x000002( 1)
42**02 =                 1764(x00000000000006e4): M x0000000000000400(10) L x000004( 2)
42**03 =                74088(x0000000000012168): M x0000000000010000(16) L x000008( 3)
42**04 =              3111696(x00000000002f7b10): M x0000000000200000(21) L x000010( 4)
42**05 =            130691232(x0000000007ca30a0): M x0000000004000000(26) L x000020( 5)
42**06 =           5489031744(x00000001472bfa40): M x0000000100000000(32) L x000040( 6)
42**07 =         230539333248(x00000035ad370e80): M x0000002000000000(37) L x000080( 7)
42**08 =        9682651996416(x000008ce6b086100): M x0000080000000000(43) L x000100( 8)
42**09 =      406671383849472(x000171dd8f5fea00): M x0001000000000000(48) L x000200( 9)
42**10 =    17080198121677824(x003cae5985bc6400): M x0020000000000000(53) L x000400(10)
42**11 =   717368321110468608(x09f49aaff0e86800): M x0800000000000000(59) L x000800(11)

BigInteger:
1302**00 =                              1(x                           1): M  0 L  0
1302**01 =                           1302(x                         516): M 10 L  1
1302**02 =                        1695204(x                      19dde4): M 20 L  2
1302**03 =                     2207155608(x                    838e8598): M 31 L  3
1302**04 =                  2873716601616(x                 29d16db7310): M 41 L  4
1302**05 =               3741579015304032(x               d4af2401b3360): M 51 L  5
1302**06 =            4871535877925849664(x            439b2c120a574a40): M 62 L  6
1302**07 =         6342739713059456262528(x         157d73223c097f3a180): M 72 L  7
1302**08 =      8258247106403412053811456(x       6d4c07901d584d1176100): M 82 L  8
1302**09 =  10752237732537242494062515712(x    22be0ae76f53f17f6ce75600): M 93 L  9

```



## Julia

'''Module''':

```julia
module Bits

export lwb, upb

lwb(n) = trailing_zeros(n)
upb(n) = 8 * sizeof(n) - leading_zeros(n) - 1

end  # module Bits
```


'''Main''':

```julia
using Main.Bits

# Using the built-in functions `leading_zeros` and `trailing_zeros`
println("# 64 bits integers:")
@printf(" %-18s | %-64s | %-2s | %-2s\n", "number", "bit representation", "lwb", "upb")
for n in 42 .^ (0:11)
    @printf(" %-18i | %-64s | %-3i | %-3i\n", n, bits(n), lwb(n), upb(n))
end

println("\n# 128 bits integers:")
@printf(" %-40s | %-2s | %-2s\n", "number", "lwb", "upb")
for n in int128"1302" .^ (0:11)
    @printf(" %-40i | %-3i | %-3i\n", n, lwb(n), upb(n))
end
```


```txt
# 64 bits integers:
 number             | bit representation                                               | lwb | upb
 1                  | 0000000000000000000000000000000000000000000000000000000000000001 | 0   | 0
 42                 | 0000000000000000000000000000000000000000000000000000000000101010 | 1   | 5
 1764               | 0000000000000000000000000000000000000000000000000000011011100100 | 2   | 10
 74088              | 0000000000000000000000000000000000000000000000010010000101101000 | 3   | 16
 3111696            | 0000000000000000000000000000000000000000001011110111101100010000 | 4   | 21
 130691232          | 0000000000000000000000000000000000000111110010100011000010100000 | 5   | 26
 5489031744         | 0000000000000000000000000000000101000111001010111111101001000000 | 6   | 32
 230539333248       | 0000000000000000000000000011010110101101001101110000111010000000 | 7   | 37
 9682651996416      | 0000000000000000000010001100111001101011000010000110000100000000 | 8   | 43
 406671383849472    | 0000000000000001011100011101110110001111010111111110101000000000 | 9   | 48
 17080198121677824  | 0000000000111100101011100101100110000101101111000110010000000000 | 10  | 53
 717368321110468608 | 0000100111110100100110101010111111110000111010000110100000000000 | 11  | 59

# 128 bits integers:
 number                                   | lwb | upb
 1                                        | 0   | 0
 1302                                     | 1   | 10
 1695204                                  | 2   | 20
 2207155608                               | 3   | 31
 2873716601616                            | 4   | 41
 3741579015304032                         | 5   | 51
 4871535877925849664                      | 6   | 62
 6342739713059456262528                   | 7   | 72
 8258247106403412053811456                | 8   | 82
 10752237732537242494062515712            | 9   | 93
 13999413527763489727269395457024         | 10  | 103
 18227236413148063624904752885045248      | 11  | 113
```



## Kotlin

As I have no idea what the difference is supposed to be between lwb/uwb and rlwb/ruwb (unless the former numbers bits from left to right), I have only provided implementations of the latter - using Java/Kotlin library functions - which seem to be all that is needed in any case to perform the task in hand:

```scala
// version 1.1.0

import java.math.BigInteger

fun Long.rlwb() = when {
        this <= 0L -> throw IllegalArgumentException("Receiver must be positive")
        else       -> java.lang.Long.numberOfTrailingZeros(this)
    }

fun Long.ruwb() = when {
        this <= 0L -> throw IllegalArgumentException("Receiver must be positive")
        else       -> 63 - java.lang.Long.numberOfLeadingZeros(this)
    }

fun BigInteger.rlwb() = when {
        this <= BigInteger.ZERO -> throw IllegalArgumentException("Receiver must be positive")
        else                    -> this.lowestSetBit
    }

fun BigInteger.ruwb() = when {
        this <= BigInteger.ZERO -> throw IllegalArgumentException("Receiver must be positive")
        else                    -> this.bitLength() - 1
    }

fun main(args: Array<String>) {
    var pow42 = 1L
    for (i in 0..11) {
        print("42 ^ ${i.toString().padEnd(2)}  = ${pow42.toString(2).padStart(64, '0').padEnd(64)} -> ")
        println(String.format("MSB: %2d, LSB: %2d", pow42.ruwb(), pow42.rlwb()))
        pow42 *= 42L
    }
    println()
    val big1302 = BigInteger.valueOf(1302)
    var pow1302 = BigInteger.ONE
    for (i in 0..6) {
        print("1302 ^ $i = ${pow1302.toString(2).padStart(64, '0').padEnd(64)} -> ")
        println(String.format("MSB: %2d, LSB: %2d", pow1302.ruwb(), pow1302.rlwb()))
        pow1302 *= big1302
    }
}
```


```txt

42 ^ 0   = 0000000000000000000000000000000000000000000000000000000000000001 -> MSB:  0, LSB:  0
42 ^ 1   = 0000000000000000000000000000000000000000000000000000000000101010 -> MSB:  5, LSB:  1
42 ^ 2   = 0000000000000000000000000000000000000000000000000000011011100100 -> MSB: 10, LSB:  2
42 ^ 3   = 0000000000000000000000000000000000000000000000010010000101101000 -> MSB: 16, LSB:  3
42 ^ 4   = 0000000000000000000000000000000000000000001011110111101100010000 -> MSB: 21, LSB:  4
42 ^ 5   = 0000000000000000000000000000000000000111110010100011000010100000 -> MSB: 26, LSB:  5
42 ^ 6   = 0000000000000000000000000000000101000111001010111111101001000000 -> MSB: 32, LSB:  6
42 ^ 7   = 0000000000000000000000000011010110101101001101110000111010000000 -> MSB: 37, LSB:  7
42 ^ 8   = 0000000000000000000010001100111001101011000010000110000100000000 -> MSB: 43, LSB:  8
42 ^ 9   = 0000000000000001011100011101110110001111010111111110101000000000 -> MSB: 48, LSB:  9
42 ^ 10  = 0000000000111100101011100101100110000101101111000110010000000000 -> MSB: 53, LSB: 10
42 ^ 11  = 0000100111110100100110101010111111110000111010000110100000000000 -> MSB: 59, LSB: 11

1302 ^ 0 = 0000000000000000000000000000000000000000000000000000000000000001 -> MSB:  0, LSB:  0
1302 ^ 1 = 0000000000000000000000000000000000000000000000000000010100010110 -> MSB: 10, LSB:  1
1302 ^ 2 = 0000000000000000000000000000000000000000000110011101110111100100 -> MSB: 20, LSB:  2
1302 ^ 3 = 0000000000000000000000000000000010000011100011101000010110011000 -> MSB: 31, LSB:  3
1302 ^ 4 = 0000000000000000000000101001110100010110110110110111001100010000 -> MSB: 41, LSB:  4
1302 ^ 5 = 0000000000001101010010101111001001000000000110110011001101100000 -> MSB: 51, LSB:  5
1302 ^ 6 = 0100001110011011001011000001001000001010010101110100101001000000 -> MSB: 62, LSB:  6

```


=={{header|Mathematica}} / {{header|Wolfram Language}}==

```Mathematica
MSB[n_]:=BitLength[n]-1
LSB[n_]:=IntegerExponent[n,2]
```



```txt
Map[{#,"MSB:",MSB[#],"LSB:",LSB[#]}&,
Join[NestList[(42*#)&,42,5],NestList[(1302*#)&,1302,5]]]//TableForm

42			MSB:	5	LSB:	1
1764			MSB:	10	LSB:	2
74088			MSB:	16	LSB:	3
3111696			MSB:	21	LSB:	4
130691232		MSB:	26	LSB:	5
5489031744		MSB:	32	LSB:	6
1302			MSB:	10	LSB:	1
1695204			MSB:	20	LSB:	2
2207155608		MSB:	31	LSB:	3
2873716601616		MSB:	41	LSB:	4
3741579015304032	MSB:	51	LSB:	5
4871535877925849664	MSB:	62	LSB:	6
```



## PARI/GP

This version uses PARI. These work on arbitrary-length integers; the implementation for wordsize integers would be identical to [[#C|C's]].

```c
long
msb(GEN n)
{
	return expi(n);
}

long
lsb(GEN n)
{
	return vali(n);
}
```


This version uses GP. It works on arbitrary-length integers; GP cannot directly work on wordsize integers except in a <code>vecsmall</code>.

```parigp
lsb(n)=valuation(n,2);
msb(n)=#binary(n)-1;
```



## Perl

This is simple and works with both native and bigint numbers.

```perl
sub msb {
  my ($n, $base) = (shift, 0);
  $base++ while $n >>= 1;
  $base;
}
sub lsb {
  my $n = shift;
  msb($n & -$n);
}
```

With large bigints, this is much faster (while as_bin seems expensive, every Math::BigInt transaction has large overhead, so Perl ops on the binary string ends up being a huge win vs. anything doing shifts, ands, compares, etc.).  If we want one function to work on both types, we could easily modify this to make a Math::BigInt object if the input isn't already one.

```perl
sub bi_msb {         # Input should be a Math::BigInt object
  length(shift->as_bin)-3;
}
```

With native ints, this meets the task description assuming a 64-bit Perl:

```perl
sub msb64 {
  my($n, $pos) = (shift, 0);
  die "n must be a 64-bit integer)" if $n > ~0;
  no warnings 'portable';  # Remove this and adjust lines for 32-bit
  if (($n & 0xFFFFFFFF00000000) == 0) { $pos += 32; $n <<= 32; }
  if (($n & 0xFFFF000000000000) == 0) { $pos += 16; $n <<= 16; }
  if (($n & 0xFF00000000000000) == 0) { $pos +=  8; $n <<=  8; }
  if (($n & 0xF000000000000000) == 0) { $pos +=  4; $n <<=  4; }
  if (($n & 0xC000000000000000) == 0) { $pos +=  2; $n <<=  2; }
  if (($n & 0x8000000000000000) == 0) { $pos +=  1; $n <<=  1; }
  63-$pos;
}
```




## Perl 6

Perl 6 integers are arbitrary sized, and the lsb and msb methods are built-in.

```perl6
sub table ($base,$power) {
    my $digits = ($base ** $power).chars;
    printf "%{$digits}s  lsb msb\n", 'number';
    for 0..$power {
	my $x = $base ** $_;
	printf "%{$digits}d  %2d  %2d\n", $x, $x.lsb, $x.msb;
    }
}

table 42, 20;
table 1302, 20;
```

```txt
                           number  lsb msb
                                1   0   0
                               42   1   5
                             1764   2  10
                            74088   3  16
                          3111696   4  21
                        130691232   5  26
                       5489031744   6  32
                     230539333248   7  37
                    9682651996416   8  43
                  406671383849472   9  48
                17080198121677824  10  53
               717368321110468608  11  59
             30129469486639681536  12  64
           1265437718438866624512  13  70
          53148384174432398229504  14  75
        2232232135326160725639168  15  80
       93753749683698750476845056  16  86
     3937657486715347520027492352  17  91
   165381614442044595841154678784  18  97
  6946027806565873025328496508928  19  102
291733167875766667063796853374976  20  107
                                                         number  lsb msb
                                                              1   0   0
                                                           1302   1  10
                                                        1695204   2  20
                                                     2207155608   3  31
                                                  2873716601616   4  41
                                               3741579015304032   5  51
                                            4871535877925849664   6  62
                                         6342739713059456262528   7  72
                                      8258247106403412053811456   8  82
                                  10752237732537242494062515712   9  93
                               13999413527763489727269395457024  10  103
                            18227236413148063624904752885045248  11  113
                         23731861809918778839625988256328912896  12  124
                      30898884076514250049193036709740244590592  13  134
                   40230347067621553564049333796081798456950784  14  144
                52379911882043262740392232602498501590949920768  15  155
             68198645270420328087990686848453049071416796839936  16  165
          88794636142087267170563874276685869890984669485596672  17  175
      115610616256997621856074164308245002598062039670246866944  18  186
   150525022366610903656608561929334993382676775650661420761088  19  196
195983579121327396560904347631994161384245161897161169830936576  20  206
```



## Phix

=== machine-sized integers ===
There is nothing like this already built in, so we will roll our own, in low-level assembly. Of course you would normally hide this sort of stuff out of sight, far away from the usual day-to-day code.

```Phix
function msb(integer i)
    #ilASM{
            [32]
                mov eax,[i]
                bsr ecx,eax
                mov [i],ecx
            [64]
                mov rax,[i]
                bsr rcx,rax
                mov [i],rcx
          }
    return i
end function

function lsb(integer i)
    #ilASM{
            [32]
                mov eax,[i]
                bsf ecx,eax     -- (requires 0.8.0+)
                mov [i],ecx
            [64]
                mov rax,[i]
                bsf rcx,rax
                mov [i],rcx
          }
    return i
end function

atom p = 1
for i=0 to 11 do
    printf(1,"%18d %064b MSB:%2d LSB: %2d\n",{p,p,msb(p),lsb(p)})
    p *= 42
    if not integer(p) then exit end if
end for
```

```txt

                 1 0000000000000000000000000000000000000000000000000000000000000001 MSB: 0 LSB:  0
                42 0000000000000000000000000000000000000000000000000000000000101010 MSB: 5 LSB:  1
              1764 0000000000000000000000000000000000000000000000000000011011100100 MSB:10 LSB:  2
             74088 0000000000000000000000000000000000000000000000010010000101101000 MSB:16 LSB:  3
           3111696 0000000000000000000000000000000000000000001011110111101100010000 MSB:21 LSB:  4
         130691232 0000000000000000000000000000000000000111110010100011000010100000 MSB:26 LSB:  5
        5489031744 0000000000000000000000000000000101000111001010111111101001000000 MSB:32 LSB:  6
      230539333248 0000000000000000000000000011010110101101001101110000111010000000 MSB:37 LSB:  7
     9682651996416 0000000000000000000010001100111001101011000010000110000100000000 MSB:43 LSB:  8
   406671383849472 0000000000000001011100011101110110001111010111111110101000000000 MSB:48 LSB:  9
 17080198121677824 0000000000111100101011100101100110000101101111000110010000000000 MSB:53 LSB: 10
717368321110468608 0000100111110100100110101010111111110000111010000110100000000000 MSB:59 LSB: 11

```

On 32-bit the table stops at msb of 26.

Aside: power(42,5) [and above] are implemented on the FPU using fyl2x, f2xm1, and fscale;
on 64-bit that results in 130691232 + ~7.3e-12 rather than the integer 130691232 exactly,
whereas repeated multiplication by 42 as shown keeps it integer for longer.


###  mpfr/gmp

```Phix
include mpfr.e   -- 0.8.0+

function rupbz(mpz n)
    integer res = mpz_sizeinbase(n,2)
    while res!=0 and mpz_tstbit(n,res)=0 do
        res -= 1
    end while
    return res
end function

function rlwbz(mpz n)
    return mpz_scan1(n,0)
end function

mpz n = mpz_init(1)
for i = 0 to 12 do
    printf(1,"1302^%02d %38s %5d %5d\n", {i,mpz_get_str(n), rupbz(n), rlwbz(n)})
    mpz_mul_si(n,n,1302)
end for
```

```txt

1302^00                                      1     0     0
1302^01                                   1302    10     1
1302^02                                1695204    20     2
1302^03                             2207155608    31     3
1302^04                          2873716601616    41     4
1302^05                       3741579015304032    51     5
1302^06                    4871535877925849664    62     6
1302^07                 6342739713059456262528    72     7
1302^08              8258247106403412053811456    82     8
1302^09          10752237732537242494062515712    93     9
1302^10       13999413527763489727269395457024   103    10
1302^11    18227236413148063624904752885045248   113    11
1302^12 23731861809918778839625988256328912896   124    12

```

In my tests the while loop in rupbz() always iterated precisely once, suggesting it merely converts a 1-based bit count to a 0-based bit number and could be replaced by -1


## PicoLisp


```PicoLisp
(de msb (N)
   (dec (length (bin (abs N)))) )

(de lsb (N)
   (length (stem (chop (bin N)) "1")) )
```

Test:

```PicoLisp
(for N (1 42 717368321110468608 291733167875766667063796853374976)
   (tab (33 6 6) N (lsb N) (msb N)) )
```

Output:

```txt
                                1     0     0
                               42     1     5
               717368321110468608    11    59
291733167875766667063796853374976    20   107
```



## Python

```python
def msb(x):
    return x.bit_length() - 1

def lsb(x):
    return msb(x & -x)

for i in range(6):
    x = 42 ** i
    print("%10d MSB: %2d LSB: %2d" % (x, msb(x), lsb(x)))

for i in range(6):
    x = 1302 ** i
    print("%20d MSB: %2d LSB: %2d" % (x, msb(x), lsb(x)))
```

```txt

         1 MSB:  0 LSB:  0
        42 MSB:  5 LSB:  1
      1764 MSB: 10 LSB:  2
     74088 MSB: 16 LSB:  3
   3111696 MSB: 21 LSB:  4
 130691232 MSB: 26 LSB:  5
                   1 MSB:  0 LSB:  0
                1302 MSB: 10 LSB:  1
             1695204 MSB: 20 LSB:  2
          2207155608 MSB: 31 LSB:  3
       2873716601616 MSB: 41 LSB:  4
    3741579015304032 MSB: 51 LSB:  5

```



## Racket


```racket

#lang racket
(require rnrs/arithmetic/bitwise-6)
(for/list ([n 20])
  (define x (expt 42 n))
  (list n (bitwise-first-bit-set x) (- (integer-length x) 1)))

```

Output:

```racket

'((0 0 0)
  (1 1 5)
  (2 2 10)
  (3 3 16)
  (4 4 21)
  (5 5 26)
  (6 6 32)
  (7 7 37)
  (8 8 43)
  (9 9 48)
  (10 10 53)
  (11 11 59)
  (12 12 64)
  (13 13 70)
  (14 14 75)
  (15 15 80)
  (16 16 86)
  (17 17 91)
  (18 18 97)
  (19 19 102))

```



## REXX

Programming note:   The task's requirements state to compute powers of   1302   up the host's next "natural" '''long''' host word

size ···, but for REXX, the "natural" size is a character string (indeed, the only thing REXX knows are character strings, numbers

are expressed as character strings), so the output (below) was limited to four times the default size, but the actual limit may be

around eight million bytes (for some REXXes).   REXX programmers have no need to know what the host's word size is.

```rexx
/*REXX program finds the  first and last  set bit  of  "integer"  and  "long interger". */
      do cycle=1  for 2;  w=digits()             /*W:   is used for a nice display.     */
      base=word(42 1302,cycle)                   /*pick an integer for this cycle.      */
      call sep '─┬─'                             /*this is part of the separator.       */
      say center(base'**n',w)  '│'  center('rlwb',4)   '│'   centre('rupb',4) '│     bits'
      call sep '─┼─'                             /*this is part of the separator.       */
                       do j=-1                   /*traipse through all the bits.        */
                       if j==-1  then x=0
                                 else x=base**j
                       if pos('E', x)\==0  then leave       /*does it have an exponent? */
                       say right(x,w)  '│' right(rlwb(x),4) '│' right(rupb(x),4)  '│' bits
                       end    /*j*/
      call sep '─┴─'                             /*this is part of the separator.       */
      say;  say;  say;  say;       numeric digits  digits() * 4
      end   /*cycle*/                            /*        [↑]  this quadruples  DIGITS.*/
exit                                             /*stick a fork in it, we're done.*/
/*──────────────────────────────────subroutines─────────────────────────────────────────────*/
n2b:  bits=word(strip(x2b(d2x(arg(1))),'L',0) 0,1);                return bits
rlwb: arg n;call n2b n; if n==0 then return 0; return length(n2b(n))-length(strip(bits,'T',0))
rupb: arg n;call n2b n; if n==0 then return -1;                    return length(n2b(n))-1
sep:  say copies('─',w)arg(1)copies('─',4)arg(1)copies('─',4)arg(1) ||,
          copies('─',length(n2b(10**(digits()-1))));               return
```

'''output'''

```txt

──────────┬──────┬──────┬────────────────────────────
  42**n   │ rlwb │ rupb │     bits
──────────┼──────┼──────┼────────────────────────────
        0 │    0 │   -1 │ 0
        1 │    0 │    0 │ 1
       42 │    1 │    5 │ 101010
     1764 │    2 │   10 │ 11011100100
    74088 │    3 │   16 │ 10010000101101000
  3111696 │    4 │   21 │ 1011110111101100010000
130691232 │    5 │   26 │ 111110010100011000010100000
──────────┴──────┴──────┴────────────────────────────




─────────────────────────────────────┬──────┬──────┬──────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
              1302**n                │ rlwb │ rupb │     bits
─────────────────────────────────────┼──────┼──────┼──────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
                                   0 │    0 │   -1 │ 0
                                   1 │    0 │    0 │ 1
                                1302 │    1 │   10 │ 10100010110
                             1695204 │    2 │   20 │ 110011101110111100100
                          2207155608 │    3 │   31 │ 10000011100011101000010110011000
                       2873716601616 │    4 │   41 │ 101001110100010110110110110111001100010000
                    3741579015304032 │    5 │   51 │ 1101010010101111001001000000000110110011001101100000
                 4871535877925849664 │    6 │   62 │ 100001110011011001011000001001000001010010101110100101001000000
              6342739713059456262528 │    7 │   72 │ 1010101111101011100110010001000111100000010010111111100111010000110000000
           8258247106403412053811456 │    8 │   82 │ 11011010100110000000111100100000001110101011000010011010001000101110110000100000000
       10752237732537242494062515712 │    9 │   93 │ 1000101011111000001010111001110110111101010011111100010111111101101100111001110101011000000000
    13999413527763489727269395457024 │   10 │  103 │ 10110000101100101000101101110101000100000011010011101110001111100001001111100000100011110110010000000000
 18227236413148063624904752885045248 │   11 │  113 │ 111000001010101100000100010100010101100000011011010011001110101111101110010001100000011001010001101001100000000000
─────────────────────────────────────┴──────┴──────┴──────────────────────────────────────────────────────────────────────────────────────────────────────────────────────


```



## Ruby

```ruby
def msb(x)
  x.bit_length - 1
end

def lsb(x)
  msb(x & -x)
end

6.times do |i|
  x = 42 ** i
  puts "%10d MSB: %2d LSB: %2d" % [x, msb(x), lsb(x)]
end

6.times do |i|
  x = 1302 ** i
  puts "%20d MSB: %2d LSB: %2d" % [x, msb(x), lsb(x)]
end
```


```txt

         1 MSB:  0 LSB:  0
        42 MSB:  5 LSB:  1
      1764 MSB: 10 LSB:  2
     74088 MSB: 16 LSB:  3
   3111696 MSB: 21 LSB:  4
 130691232 MSB: 26 LSB:  5
                   1 MSB:  0 LSB:  0
                1302 MSB: 10 LSB:  1
             1695204 MSB: 20 LSB:  2
          2207155608 MSB: 31 LSB:  3
       2873716601616 MSB: 41 LSB:  4
    3741579015304032 MSB: 51 LSB:  5

```



## Seed7

The library [http://seed7.sourceforge.net/libraries/integer.htm integer.s7i] defines the functions
[http://seed7.sourceforge.net/libraries/integer.htm#bitLength%28in_integer%29 bitLength] and
[http://seed7.sourceforge.net/libraries/integer.htm#lowestSetBit%28in_integer%29 lowestSetBit], which compute the
most- and least-significant set bit in a binary value expressed in LSB 0 bit numbering.


```seed7
$ include "seed7_05.s7i";
  include "bigint.s7i";

const func integer: rlwb (in integer: num) is
  return lowestSetBit(num);

const func integer: rupb (in integer: num) is
  return bitLength(num);

const func integer: rlwb (in bigInteger: num) is
  return lowestSetBit(num);

const func integer: rupb (in bigInteger: num) is
  return bitLength(num);

const proc: main is func
  local
    var integer: i is 0;
    var integer: num is 0;
    var bigInteger: bigNum is 0_;
  begin
    for i range 0 to 5 do
      num := 42 ** i;
      writeln(num lpad 10 <& " " <& num radix 2 lpad0 32 <&
              " MSB: " <& rupb(num) lpad 2 <& " LSB: " <& rlwb(num) lpad 2);
    end for;
    for i range 0 to 9 do
      bigNum := 1302_ ** i;
      writeln(bigNum lpad 30 <& " " <& bigNum radix 16 lpad0 26 <&
              " MSB: " <& rupb(bigNum) lpad 2 <& " LSB: " <& rlwb(bigNum) lpad 2);
    end for;
  end func;
```


```txt

         1 00000000000000000000000000000001 MSB:  1 LSB:  0
        42 00000000000000000000000000101010 MSB:  6 LSB:  1
      1764 00000000000000000000011011100100 MSB: 11 LSB:  2
     74088 00000000000000010010000101101000 MSB: 17 LSB:  3
   3111696 00000000001011110111101100010000 MSB: 22 LSB:  4
 130691232 00000111110010100011000010100000 MSB: 27 LSB:  5
                             1 00000000000000000000000001 MSB:  1 LSB:  0
                          1302 00000000000000000000000516 MSB: 11 LSB:  1
                       1695204 0000000000000000000019dde4 MSB: 21 LSB:  2
                    2207155608 000000000000000000838e8598 MSB: 32 LSB:  3
                 2873716601616 00000000000000029d16db7310 MSB: 42 LSB:  4
              3741579015304032 0000000000000d4af2401b3360 MSB: 52 LSB:  5
           4871535877925849664 0000000000439b2c120a574a40 MSB: 63 LSB:  6
        6342739713059456262528 0000000157d73223c097f3a180 MSB: 73 LSB:  7
     8258247106403412053811456 000006d4c07901d584d1176100 MSB: 83 LSB:  8
 10752237732537242494062515712 0022be0ae76f53f17f6ce75600 MSB: 94 LSB:  9

```



## Sidef

Sidef has arbitrary sized integers.
```ruby
func msb(n) {
    var b = 0
    while(n >>= 1) { ++b }
    return b
}

func lsb(n) {
    msb(n & -n)
}
```


Test cases:
```ruby
func table (base,power) {
    var digits = length(base**power)
    printf("%#{digits}s  lsb msb\n", 'number')
    for n in (0..power) {
        var x = base**n
        printf("%#{digits}s  %2s  %3s\n", x, lsb(x), msb(x))
    }
}

table(42, 20)
table(1302, 20)
```



## Tcl


```tcl
proc lwb {x} {
    if {$x == 0} {return -1}
    set n 0
    while {($x&1) == 0} {
	set x [expr {$x >> 1}]
	incr n
    }
    return $n
}
proc upb {x} {
    if {$x == 0} {return -1}
    if {$x < 0} {error "no well-defined max bit for negative numbers"}
    set n 0
    while {$x != 1} {
	set x [expr {$x >> 1}]
	incr n
    }
    return $n
}
```

Code to use the above:

```tcl
package require Tcl 8.6; # For convenient bit string printing

proc powsTo {pow bits} {
    set result {}
    for {set n 1} {$n < 2**$bits} {set n [expr {$n * $pow}]} {
	lappend result $n
    }
    return $result
}
proc printPows {pow pows} {
    set len [string length [lindex $pows end]]
    puts [format "%8s | %*s | LWB | UPB | Bits" "What" $len "Number"]
    set n 0
    foreach p $pows {
	puts [format "%4d**%-2d = %*lld | %3d | %3d | %b" \
		  $pow $n $len $p [lwb $p] [upb $p] $p]
	incr n
    }
}

puts "Powers of 42 up to machine word size:"
printPows 42 [powsTo 42 [expr {$tcl_platform(wordSize) * 8}]]
puts "Powers of 1302 up to 128 bits"
printPows 1302 [powsTo 1302 128]
```

Output:

```txt

Powers of 42 up to machine word size:
    What |    Number | LWB | UPB | Bits
  42**0  =         1 |   0 |   0 | 1
  42**1  =        42 |   1 |   5 | 101010
  42**2  =      1764 |   2 |  10 | 11011100100
  42**3  =     74088 |   3 |  16 | 10010000101101000
  42**4  =   3111696 |   4 |  21 | 1011110111101100010000
  42**5  = 130691232 |   5 |  26 | 111110010100011000010100000
Powers of 1302 up to 128 bits
    What |                                 Number | LWB | UPB | Bits
1302**0  =                                      1 |   0 |   0 | 1
1302**1  =                                   1302 |   1 |  10 | 10100010110
1302**2  =                                1695204 |   2 |  20 | 110011101110111100100
1302**3  =                             2207155608 |   3 |  31 | 10000011100011101000010110011000
1302**4  =                          2873716601616 |   4 |  41 | 101001110100010110110110110111001100010000
1302**5  =                       3741579015304032 |   5 |  51 | 1101010010101111001001000000000110110011001101100000
1302**6  =                    4871535877925849664 |   6 |  62 | 100001110011011001011000001001000001010010101110100101001000000
1302**7  =                 6342739713059456262528 |   7 |  72 | 1010101111101011100110010001000111100000010010111111100111010000110000000
1302**8  =              8258247106403412053811456 |   8 |  82 | 11011010100110000000111100100000001110101011000010011010001000101110110000100000000
1302**9  =          10752237732537242494062515712 |   9 |  93 | 1000101011111000001010111001110110111101010011111100010111111101101100111001110101011000000000
1302**10 =       13999413527763489727269395457024 |  10 | 103 | 10110000101100101000101101110101000100000011010011101110001111100001001111100000100011110110010000000000
1302**11 =    18227236413148063624904752885045248 |  11 | 113 | 111000001010101100000100010100010101100000011011010011001110101111101110010001100000011001010001101001100000000000
1302**12 = 23731861809918778839625988256328912896 |  12 | 124 | 10001110110101001011100011111110101101101100001101011011001001101111110110111011000001001000010001101000010010001000000000000

```



## zkl

This uses the Int method log2 (== MSB position), which returns the log base 2 of self. log2 is implemented with shifts and ors (it is a 5 step loop (for 64 bit ints) which could obviously be unrolled). See http://graphics.stanford.edu/~seander/bithacks.html.

```zkl
fcn lsb(n){ n.bitAnd(n.bitNot()+1).log2() }
fcn msb(n){ n.log2() }

foreach p in (200){
  n:=(42).pow(p);
  println("42^%2d = %18d(x%015x): MSB(%2d) LSB(%2d)".fmt(
	  p,n,n, msb(n), lsb(n)));
  if (n>=(1).MAX / 42) break;
}
```

```txt

42^ 0 =                  1(x000000000000001): MSB( 0) LSB( 0)
42^ 1 =                 42(x00000000000002a): MSB( 5) LSB( 1)
42^ 2 =               1764(x0000000000006e4): MSB(10) LSB( 2)
42^ 3 =              74088(x000000000012168): MSB(16) LSB( 3)
42^ 4 =            3111696(x0000000002f7b10): MSB(21) LSB( 4)
42^ 5 =          130691232(x000000007ca30a0): MSB(26) LSB( 5)
42^ 6 =         5489031744(x0000001472bfa40): MSB(32) LSB( 6)
42^ 7 =       230539333248(x0000035ad370e80): MSB(37) LSB( 7)
42^ 8 =      9682651996416(x00008ce6b086100): MSB(43) LSB( 8)
42^ 9 =    406671383849472(x00171dd8f5fea00): MSB(48) LSB( 9)
42^10 =  17080198121677824(x03cae5985bc6400): MSB(53) LSB(10)
42^11 = 717368321110468608(x9f49aaff0e86800): MSB(59) LSB(11)

```


