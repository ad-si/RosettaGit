+++
title = "Category:Scheme/SRFIs"
description = ""
date = 2017-04-06T21:59:32Z
aliases = []
[extra]
id = 21331
[taxonomies]
categories = []
tags = []
+++

The SRFIs are Scheme Requests for Implementations, and form an unofficial standard library for [[:Category:Scheme|Scheme]] programs to draw upon.
Each SRFI is a library for a particular set of functionality.  They include additional functions for working with lists, strings or 
vectors; additional data types, such as hash sets or time data; and alternative expression types, such as eager comprehensions and generators.

http://srfi.schemers.org is the official list of SRFIs.  There are currently more than 150 SRFIs, although some of these are still in draft status, or have been withdrawn.  All (almost) SRFIs come with a sample implementation.

The latest standard for Scheme, R7RS, has been divided into two parts.  R7RS-small provides the core language definition, in the same spirit as R5RS.  R7RS-large is currently in progress, and will formally sanction a standard library: known as the [http://trac.sacrideo.us/wg/wiki/RedEdition Red Edition], this library looks like being a selection from established SRFIs.  For example, the (scheme list) library is SRFI-1; the (scheme vector) library is SRFI-133, etc.

Different implementations of Scheme provide varying support for the SRFIs and, to run programs relying on SRFIs, you will need to find an implementation which provides it: for example, Larceny includes more than half of the SRFIs.

Some important SRFIs, as found within the Scheme examples in RC, include:

; SRFI 1 List:  Provides extra functions for lists, including filter, fold, drop/take-while etc.
; SRFI 13 Strings: Provides string-tokenize to divide a string into sections depending on a separator.
; SRFI 14 Character-Sets: Particularly useful for SRFI 13 operations, such as specifying the characters to use when tokenising a string
; SRFI 27 Random bits:  There is no standard random function in Scheme, and this SRFI provides a portable way of creating random numbers across implementations.
; SRFI 69 and 125 Hash Tables:  Both provide a hash-table data structure, but with slightly different syntax.  (Several implementations support SRFI 69, but SRFI 125 has been selected for R7RS-large.)
; SRFI 132 Sorting library:  A collection of procedures to sort lists and vectors.
