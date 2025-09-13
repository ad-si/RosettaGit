+++
title = "PL/I"
description = ""
date = 2016-06-05T23:53:26Z
aliases = []
[extra]
id = 2625
[taxonomies]
categories = []
tags = []
+++
PL/I is a general purpose programming language suitable for commercial, scientific, non-scientific, and system programming.


It provides the following data types:

::*   Floating-point,
::*   Decimal integer,
::*   Binary integer,
::*   Fixed-point decimal   (with a fractional part),
::*   Fixed-point binary   (that is, with a fractional part),
::*   Pointers,
::*   Character strings of two kinds:
::::#   fixed-length,   and
::::#   varying-length.
::*   Bit strings of two kinds:
::::#   fixed-length,   and
::::#   varying length.



The   float,   integer,   and   fixed-point   types can be   real   or   complex.


Multiple precisions are available for binary fixed-point:
::*   8 bits,
::*   16 bits,
::*   32 bits,   and
::*   64 bits.


Multiple precisions are available for floating point:
::*   32 bits,
::*   64 bits,   and
::*   80 bits.


The language provides for static and dynamic arrays.   Of the latter, there are   automatic,   controlled,   and   based.

Controlled can be applied to any data type, including scalar, structure, as well as arrays.   With controlled, a push-down and pop-up stack is automatically used.


PL/I has four kinds of I/O:
::#   For simple I/O commands, list-directed input and output requires only the names of the variables.   Default format is used, based on the variable's declaration.
::#   For simple I/O commands, data-directed input and output requires only the names of the variables.   For this form, both the names of the variables and their values are transmitted.
::#   When precise layouts of input and output data is required, edit-directed I/O is used.   A format is specified by the user.   The format is flexible, and permits the number of digits, and the number of places after the decimal point to be specified dynamically.   The format may also be specified in picture form.
::#   For files held on storage media, record-oriented transmission is often used, either for   sequential   or   random access.


PL/I has built-in checking for such programmer conditions including
::*   subscript-range checking,
::*   floating-point overflow,
::*   fixed-point overflow,
::*   division by zero,
::*   sub-string range checking,   and
::*   string-size checking.



Any of those may be enabled or disabled by the user.

When any of those conditions occurs, the user/programmer may trap them and recover from them and continue execution.

PL/I has a unique and powerful pre-processor which is a subset of the full PL/I language so it can be used to perform   (among other things):
::*   source file inclusion,
::*   conditional compilation,   and
::*   macro expansion.



The pre-processor keywords are prefixed with a   <big>%</big>   (percent symbol).



