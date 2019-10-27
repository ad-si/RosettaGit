+++
title = "UTF-8"
description = ""
date = 2009-08-19T09:46:15Z
aliases = []
[extra]
id = 2101
[taxonomies]
categories = []
tags = []
+++

[[Category:Encyclopedia]]'''Unicode Transformation Format, 8-bit representation''' or UTF-8 is a particular encoding of [[Unicode]] code-points into eight-bit octets. It was originally developed for [[Bell Labs]]' [[Plan 9]] operating system by Ken Thompson (inventor of [[Unix]]) and Rob Pike in 1992. It is widely used on Unix-like systems and for XML documents.

Some advantages of UTF-8:
* byte-order independent
* subsumes 7-bit ASCII
* one can detect the start of characters
* one can scan characters in both directions forward and backward
* can encode code-points at least 32-bits long

Challenges:
* characters do not have a fixed size. One needs to walk an entire string to determine the [[String Character Length|character length of a string]].
* biased towards European scripts. Japanese code points are more compactly stored in other encodings, such as UTF-16 or UCS-2.
