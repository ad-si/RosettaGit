+++
title = "CIS COBOL"
description = ""
date = 2019-07-01T18:57:54Z
aliases = []
[extra]
id = 22399
[taxonomies]
categories = []
tags = []
+++
In the late seventies, the company Micro Focus created Compact Interactive Standard COBOL (CIS COBOL) for 8-bit microcomputers.  CIS COBOL is based on the ANSI COBOL standard X3.23 (1974). Due to the memory constraints of 64 kilobyte RAM only Level 1 and a few features from Level 2 are implemented.

## Source format
The free format was not introduced until 2002. ANSI-74 COBOL therefore requires the original COBOL source format, which divides each COBOL source record into 72 columns. These columns are used in the following way:
* Columns 1 to 6 - Sequence number
* Column 7 - Indicator area
* Columns 8 to 11 - Area A
* Columns 12 to 72 - Area B

A sequence number of six digits may be used to identify each source program line. An asterisk * in the Indicator area marks the line as documentary comment only. Section names and paragraph names begin in Area A and are followed by a period and a space or newline. Level indications FD, 01 and 77 begin in Area A and are followed in Area B by the appropriate file and
record description. Program sentences may commence anywhere in Area B.

A COBOL program consists of four divisions:

# IDENTIFICATION DIVISION - An identification of the program
# ENVIRONMENT DIVISION - A description of the equipment to be used to compile and run the program
# DATA DIVISION - A description of the data to be processed
# PROCEDURE DIVISION - A set of procedures to specify the operations to be performed on the data

Each division is divided into sections which are further divided into paragraphs which in turn are made
up of sentences. Within these subdivisions of a COBOL program, further subdivisions exist as clauses and statements. A clause is an ordered set of COBOL elements that specify an attribute of an entry, and a statement is a combination of elements in the Procedure Division that include a COBOL verb and constitute a program instruction.
