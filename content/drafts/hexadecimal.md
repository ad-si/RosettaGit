+++
title = "Hexadecimal"
description = ""
date = 2010-06-09T20:56:51Z
aliases = []
[extra]
id = 3325
[taxonomies]
categories = []
tags = []
+++

[[Category:Encyclopedia]]Hexadecimal is a counting system that uses sixteen digits.

Instead of using only 0's and 1's like binary, or the characters 0 to 9 of the decimal number system; hexadecimal uses the characters '0' to '9' to represent the numbers 0 to 9, and also the single characters 'A' to 'F' (or sometimes 'a' to 'f', but usually not mixing case), to represent the numbers 10 through to 15, in order. 

== Uses ==
The hexadecimal number system is used widely in the Electronics and Computer Industry, as although digital electronics is based on gates with only two states and is therefore fundamentally binary, binary numbers can quickly become long and hard to transcribe without errors. Their hexadecimal equivalents are much shorter and easier to remember, and have a straight-forward way of conversion to/from binary.


###  Hex Dump 

A textual representation of data where values are expressed in hexadecimal. Often used to show the contents of regions of memory where both the memory addresses as well as the memory contents may be expressed in hexadecimal.

== Comparing counts from zero in different number systems ==
C.f. [[Common number base formatting]] and [[Common number base parsing]]
      Binary
            Decimal
                  Hexadecimal
      0     0     0
      1     1     1
     10     2     2
     11     3     3
    100     4     4
    101     5     5
    110     6     6
    111     7     7
   1000     8     8
   1001     9     9
   1010    10     A
   1011    11     B
   1100    12     C
   1101    13     D
   1110    14     E
   1111    15     F
  10000    16    10
  10001    17    11
  10010    18    12
  10011    19    13
  10100    20    14
  10101    21    15
  10110    22    16
  10111    23    17
  11000    24    18
  11001    25    19
  11010    26    1A
  11011    27    1B
  11100    28    1C
  11101    29    1D
  11110    30    1E
  11111    31    1F
 100000    32    20
 100001    33    21

== Converting binary to hexadecimal ==
# Split a binary number into groups of four digits, counting from right to left.
# Pad the leftmost group of binary digits with zeros on their left if there are less than four digits.
# Use the following table to translate each group of four binary digits, in order, to its hexadecimal equivalent. 
 Binary digits
       Hexadecimal equivalent digit
 0000  0
 0001  1
 0010  2
 0011  3
 0100  4
 0101  5
 0110  6
 0111  7
 1000  8
 1001  9
 1010  A
 1011  B
 1100  C
 1101  D
 1110  E
 1111  F


###  An example conversion 

      Binary Number:     1011010111
              Split:   10 1101 0111
                Pad: 0010 1101 0111
   Translate groups:    2    D    7
 Hexadecimal answer: 2D7

== Hex words ==
It is common when working at the assembler level, and when designing digital hardware to arrange for self-checking tests to denote pass or failure by writing distinctive values to a memory location, or for un-initialised memory to be distinctive values. Many engineers use values whose value when expressed in a hex dump stands out as valid english words such as the values DEAD and BEEF.

A scan of the word list used in [[Anagrams]] leads to this list of four digit hexadecimal values that are also words:

```txt
aaa5    babe    be1a    b01d    cede    c0de    dead    d011    fa11    f0a1    1ead    1011    5ea1    50fa
abbe    bade    be11    b01e    ce11    c0ed    deaf    d05e    feed    f01d    1eaf    105e    5eed    501d
abed    ba1d    be55    b010    c1ad    c01a    dea1    ea5e    fee1    f00d    1e55    1055    5e1f    501e
abe1    ba1e    b1ab    b05e    c10d    c01d    deed    ee0c    fe11    f001    10ad    0b0e    5e11    5010
ab1e    ba11    b1ed    b055    c0a1    c01e    de11    e1ba    f1ea    f055    10af    01af    51ab
a1ba    ba5e    b10b    cafe    c0bb    c001    d0dd    e11a    f1ed    1ace    10be    0510    51ed
a1ec    ba55    b10c    ca1f    c0ca    dada    d0d0    e15e    f1ee    1a05    10b0    5afe    510b
a10e    bead    b0ca    ca11    c0c0    dade    d0ff    face    f10c    1a5e    10eb    5a1e    510e
a150    beef    b0de    ca5e    c0da    da1e    d01e    fade    f10e    1a55    101a    5cab    50da
```

