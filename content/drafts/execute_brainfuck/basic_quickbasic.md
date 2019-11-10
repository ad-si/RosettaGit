+++
title = "Execute Brainfuck/BASIC/QuickBasic"
description = ""
date = 2010-02-06T14:17:00Z
aliases = []
[extra]
id = 2733
[taxonomies]
categories = []
tags = []
+++

{{implementation|Brainfuck}}{{collection|RCBF}}
This implementation is very limited by [[BASIC]], especially QuickBasic. The memory space cannot be resized (not Turing complete). It is initially set to 20000, but can be changed before running the program. Input will not accept return characters at all. A buffer is programmed in for input, as well.

The program first asks for a filename for BF source code. If no filename is given (return hit without typing anything), the program accepts source from std in. To end your std input, enter a blank line (completely blank, no whitespace). Source files don't need a blank line since BASIC has EOF support. The program then cleans up the code similarly to [[RCBF/Java|the Java implementation]] and reports an uneven bracket error if it exists. Input works with a programmed buffer. It takes in an entire line if the buffer is empty, or it takes the first character from the buffer if it's not (just like the way you'd expect a buffer to work). Looping works the same as in the Java implementation, as well.

{{works with|QuickBasic|4.5}}

```qbasic
CLS
memsize = 20000
instChars$ = "+-<>.,[]" 'valid characters
ptr = 0 'memory pointer

INPUT "Filename (blank to use std in)...? ", filename$
IF filename$ = "" THEN
        DO
                LINE INPUT line$
                source$ = source$ + line$
        LOOP WHILE line$ <> ""
ELSE
        OPEN filename$ FOR INPUT AS #1
        DO
                LINE INPUT #1, line$
                source$ = source$ + line$
        LOOP UNTIL EOF(1)
END IF
CLOSE 'close the file if it's open

'let's clean the code up
bktCnt = 0
FOR i = 1 TO LEN(source$)
        char$ = MID$(source$, i, 1)
        in = 0
        'check to see if this is a valid instruction character
        IF instr(instChars$,char$) THEN
                code$ = code$ + char$
                'count brackets
                IF char$ = "[" THEN bktCnt = bktCnt + 1
                IF char$ = "]" THEN bktCnt = bktCnt - 1
        END IF
NEXT i

IF bktCnt THEN 'mismatched brackets
        PRINT "Uneven brackets"
        END
END IF

DIM SHARED memory(0 TO memsize) AS INTEGER 'fixed-size memory space :(
inLine$ = "" 'input buffer
FOR i = 1 TO LEN(code$) 'loop through the code
        DIM inChar AS STRING * 1 'single character for input
        instruction$ = MID$(code$, i, 1) 'get the instruction we're on
        SELECT CASE instruction$
            CASE "+"
                memory(ptr) = memory(ptr) + 1
            CASE "-"
                memory(ptr) = memory(ptr) - 1
            CASE "."
                PRINT CHR$(memory(ptr));
            CASE ","
                IF inLine$ = "" THEN LINE INPUT inLine$ 'buffer input
                inChar = LEFT$(inLine$, 1) 'take the first char off the buffer
                inLine$ = MID$(inLine$, 2) 'delete it from the buffer
                memory(ptr) = ASC(inChar) 'use it
            CASE ">"
                ptr = ptr + 1
                IF ptr > 20000 THEN
                        PRINT "Memory pointer out of range"
                        END
                END IF
            CASE "<"
                ptr = ptr - 1
                IF ptr < 0 THEN
                        PRINT "Memory pointer out of range"
                        END
                END IF
            CASE "["
                IF memory(ptr) = 0 THEN
                        bktCnt = 1'count the bracket we're on
                        i = i + 1'move the code pointer to the next char
                        WHILE bktCnt <> 0
                                'count nested loops till we find the matching one
                                IF MID$(code$, i, 1) = "]" THEN bktCnt = bktCnt - 1
                                IF MID$(code$, i, 1) = "[" THEN bktCnt = bktCnt + 1
                                i = i + 1 'search forward
                        WEND
                END IF
            CASE "]"
                IF memory(ptr) <> 0 THEN
                        bktCnt = -1'count the bracket we're on
                        i = i - 1'move the code pointer back a char
                        WHILE bktCnt <> 0
                                'count nested loops till we fine the matching one
                                IF MID$(code$, i, 1) = "]" THEN bktCnt = bktCnt - 1
                                IF MID$(code$, i, 1) = "[" THEN bktCnt = bktCnt + 1
                                i = i - 1 'search backwards
                        WEND

                END IF
        END SELECT
NEXT i
```

