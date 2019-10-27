+++
title = "Floating point"
description = ""
date = 2012-03-24T10:40:32Z
aliases = []
[extra]
id = 11557
[taxonomies]
categories = []
tags = []
+++

[[Category:Encyclopedia]]
[[wp:Floating point|Floating point]] is a numeric system for approximating real numbers. Each floating-point number stores some ''digits'' and an ''exponent'' (plus a ''sign'', which is either ''1'' or ''-1'') taking the form

: ''value = sign &times; digits &times; RADIX<sup>exponent</sup>''

This design uses a constant ''RADIX'' and limits the maximum number of digits. Calculations are fast but inexact, because the limit on digits causes round-off errors.
It should be noted that, with an appropriate ''exponent'', a floating point number can represent a substantial range of integers exactly (though less than the range that could fit in the same space with a “pure” integer).

The most common floating-point formats in modern practice are those based on the [[wp:IEEE 754|IEEE 754]] standard, in particular with the ''RADIX'' being ''2'', and the ''digits'' and ''exponent'' being a fixed number of binary digits that fit (together with the ''sign'') in a piece of memory of size 32 bits (4 bytes, <code>float</code>) or 64 bits (8 bytes, <code>double</code>).
