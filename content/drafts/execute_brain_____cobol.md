+++
title = "Execute Brain****/COBOL"
description = ""
date = 2013-07-02T16:47:03Z
aliases = []
[extra]
id = 14241
[taxonomies]
categories = []
tags = []
+++

This is a simple [[Brainf***]] interpreter written in [[COBOL]], which receives its program from standard input.
{{works with|OpenCOBOL}}

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. Brainfuck-Interpreter.
 
       DATA DIVISION.
       LOCAL-STORAGE SECTION.
 
       01  Nesting-Level   PIC 999.
 
       01  Array-Area.
           03  Array OCCURS 30000 TIMES INDEXED BY Table-Index.
               05  Array-Table USAGE BINARY-CHAR.
               05  Array-Table-Char REDEFINES Array-Table PIC X.
 
       01  Input-Char       PIC X.
 
*     *>> Note: This limit is mostly arbitrary.
       01  Max-Program-Size CONSTANT 2048.
       01  Input-Program    PIC X(Max-Program-Size).
       01  Program-Index    USAGE BINARY-LONG UNSIGNED.
 
       PROCEDURE DIVISION.
       Main.
           DISPLAY "Enter program: " WITH NO ADVANCING
           ACCEPT Input-Program
 
           PERFORM Process-Statement VARYING Program-Index FROM 1 BY 1
                   UNTIL Max-Program-Size < Program-Index
 
           GOBACK
           .
 
       Process-Statement.
           EVALUATE Input-Program (Program-Index:1)
               WHEN ">"
                   SET Table-Index UP BY 1
 
               WHEN "<"
                   SET Table-Index DOWN BY 1
 
               WHEN "+"
                   ADD 1 TO Array-Table (Table-Index)
 
               WHEN "-"
                   SUBTRACT 1 FROM Array-Table (Table-Index)
 
               WHEN "."
                   DISPLAY Array-Table-Char (Table-Index)
 
               WHEN ","
                   ACCEPT Array-Table-Char (Table-Index)
 
                WHEN "["
                    IF Array-Table (Table-Index) = ZERO
                        PERFORM Jump-To-Block-End
                    END-IF
 
                WHEN "]"
                    IF Array-Table (Table-Index) NOT = ZERO
                        PERFORM Jump-To-Block-Start
                    END-IF
           END-EVALUATE
           .
 
*     *>> Move Program-Index back to position of matching '['
       Jump-To-Block-Start.
           SUBTRACT 1 FROM Program-Index
           PERFORM VARYING Program-Index FROM Program-Index BY -1
                   UNTIL ((Input-Program (Program-Index:1) = "[")
                       AND (Nesting-Level = 0))
                   OR (Program-Index = 0)
               EVALUATE Input-Program (Program-Index:1)
                   WHEN "["
                       SUBTRACT 1 FROM Nesting-Level
 
                   WHEN "]"
                       ADD 1 TO Nesting-Level
               END-EVALUATE
           END-PERFORM
 
           PERFORM Check-Mismatched-Brackets
           .
 
*     *>> Move Program-Index forward to position of matching ']'
       Jump-To-Block-End.
           ADD 1 TO Program-Index
           PERFORM VARYING Program-Index FROM Program-Index BY 1
                   UNTIL ((Input-Program (Program-Index:1) = "]")
                       AND (Nesting-Level = 0))
                   OR (Input-Program (Program-Index:1) = SPACE)
 
               EVALUATE Input-Program (Program-Index:1)
                   WHEN "["
                       ADD 1 TO Nesting-Level
 
                   WHEN "]"
                       SUBTRACT 1 FROM Nesting-Level
               END-EVALUATE
           END-PERFORM
 
           PERFORM Check-Mismatched-Brackets
           .
 
       Check-Mismatched-Brackets.
           IF (Program-Index = 0)
                   OR (Input-Program (Program-Index:1) = SPACE)
               DISPLAY "Mismatched square brackets. Aborting..."
               GOBACK
           END-IF
           .
```

