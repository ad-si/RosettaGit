+++
title = "Category:PL/M"
description = ""
date = 2017-09-27T19:14:47Z
aliases = []
[extra]
id = 5436
[taxonomies]
categories = []
tags = []
+++

{{stub}}{{language}}

PL/M (Programming Language for Microcomputers) is (as the name suggests) a language designed for microcomputer software, particularly system software.




It is approximately a very small subset of PL/1 (though not a strict subset).


The following statements from PL/1 (with some changes) were available:
* assignment
* CALL
* DECLARE
* DO-END
* IF-THEN-ELSE
* GOTO
* PROCEDURE-END
* RETURN
Additionally, a HALT statement, interrupt related statements and a number of compiler directive statements existed.

There are no built-in I/O statements - calls to appropriate routines would be made instead.



Unlike PL1, PL/M keywords are reserved and so cannot be used as identifiers. The Boolean operators are reserved words: AND, OR and NOT instead of the symbols: &, |, ¬.


Available datatypes (BYTE, WORD, etc.) reflected the available types of the microprocessors.



The declaration of structures in PL/M does not use level-numbers, instead a syntax more like C structs is used, e.g.:

```PLM
DECLARE A STRUCTURE ( B BYTE, C WORD );
```

declares a structure A with two members, B and C.




PL/M was used in the development of the CP/M operating system and associated applications.


Compilers were available for a number of microprocessors including the 8080 Z80.




==See Also==
* [[wp:PL/M|Wikipedia page on PL/M]]: [https://en.wikipedia.org/wiki/PL/M]
* [[PL/1]]
