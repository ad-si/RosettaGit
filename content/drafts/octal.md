+++
title = "Octal"
description = ""
date = 2009-06-09T08:16:18Z
aliases = []
[extra]
id = 3329
[taxonomies]
categories = []
tags = []
+++

[[Category:Encyclopedia]]Octal is a counting system that uses eight digits.

Instead of using only 0's and 1's like binary, or the characters '0' to '9' of the decimal number system; octal uses the characters '0' to '7', so does not need what would normally be classed as alphabetic characters to represent digits as [[Hexadecimal]] does.

== Uses ==
The octal number system was used widely in the Electronics and Computer Industry, as although digital electronics is based on gates with only two states and is therefore fundamentally binary, binary numbers can quickly become long and hard to transcribe without errors. Their octal equivalents are much shorter and easier to remember, and have a straight-forward way of conversion to/from binary.

The [http://en.wikipedia.org/wiki/Pdp-11 PDP-11] computer made by the [http://en.wikipedia.org/wiki/Digital_Equipment_Corporation Digital Equipment Corporation] used the octal numeric system exclusively for displaying memory addresses and content.

[[Unix]] file system permissions have three sets (user, group, others) of three bit permissions (read, write, execute), which is naturally represented in octal.

The use of octal numbers has declined as most modern computers no longer base their word length on multiples of three bits, (they are based on multiples of four bits, so [[hexadecimal]] is more widely used).

== Comparing counts from zero in different number systems ==
C.f. [[Common number base formatting]] and [[Common number base parsing]]
      Binary
          Octal
             Decimal
      0   0  0
      1   1  1
     10   2  2
     11   3  3
    100   4  4
    101   5  5
    110   6  6
    111   7  7
   1000  10  8
   1001  11  9
   1010  12 10
   1011  13 11
   1100  14 12
   1101  15 13
   1110  16 14
   1111  17 15
  10000  20 16
  10001  21 17
  10010  22 18
  10011  23 19
  10100  24 20
  10101  25 21

== Converting binary to octal ==
# Split a binary number into groups of three digits, counting from right to left.
# Pad the leftmost group of binary digits with zeros on their left if their are less than three digits.
# Use the following table to translate each group of three binary digits, in order, to its octal equivalent. 
 Binary digits
      Octal equivalent digit
 000  0
 001  1
 010  2
 011  3
 100  4
 101  5
 110  6
 111  7


###  An example conversion 

      Binary Number:     1011010111
              Split:  1 011 010 111
                Pad:001 011 010 111
   Translate groups:  1   3   2   7
       Octal answer:           1327
