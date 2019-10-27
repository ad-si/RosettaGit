+++
title = "RCSNUSP/COBOL"
description = ""
date = 2014-01-14T19:16:23Z
aliases = []
[extra]
id = 16970
[taxonomies]
categories = []
tags = []
+++

This an SNUSP interpreter written in COBOL. It supports Modular and Bloated SNUSP.

The file path to the code is passed as a parameter to the program. The file is assumed to be a text file with lines having a maximum length of 100 characters. It is also assumed the file will not be more than 1024 lines long.

The memory is a 2048 byte array, the stack has a size of 512 and there is a maximum of 16 threads.

{{works with|GNU Cobol|2.0}}

snusp.cob:

```cobol>       >
SOURCE FREE
IDENTIFICATION DIVISION.
PROGRAM-ID. snusp-interpreter.

ENVIRONMENT DIVISION.
CONFIGURATION SECTION.
REPOSITORY.
    FUNCTION find-initial
    FUNCTION ALL INTRINSIC
    .
DATA DIVISION.
WORKING-STORAGE SECTION.
COPY "dd-program-arguments.cpy".
 
COPY "dd-code-area.cpy".
    
01  program-state-flag                  PIC X VALUE SPACE.
    88  program-ok                      VALUE SPACE.
    88  empty-stack                     VALUE "E".
    88  out-of-code-space               VALUE "O".
 
01  memory-area.
    03  memory-rows                     OCCURS 1024 TIMES.
        05  memory-cols                 OCCURS 1024 TIMES.
            07  memory-cell             USAGE BINARY-CHAR.
            07  memory-cell-char        REDEFINES memory-cell PIC X.
 
01  num-threads                         PIC 99 COMP VALUE 1.
01  threads-data-area.
    03  threads-data                    OCCURS 1 TO 16 TIMES
                                        DEPENDING ON num-threads
                                        INDEXED BY thread-idx.
        05  thread-status-flag          PIC X VALUE SPACE.
            88  thread-started          VALUE "Y".
        05  call-stack.
            07  calls                   OCCURS 512 TIMES
                                        INDEXED BY stack-idx.
                09  direction           PIC X.
                    88  up-dir          VALUE "U".
                    88  down-dir        VALUE "D".
                    88  left-dir        VALUE "L".
                    88  right-dir       VALUE "R".
                09  instruction-ptr.
                    11  ip-line         USAGE INDEX.
                    11  ip-char         USAGE INDEX.
        05  memory-pointer.
            07  row-idx                 USAGE INDEX.
            07  col-idx                 USAGE INDEX.
 
01  input-char                          PIC X.
 
01  current-thread-idx                  USAGE INDEX.
 
PROCEDURE DIVISION.
000-main SECTION.
001-prepare-code.
    CALL "parse-arguments" USING program-arguments
    IF code-file-path = SPACES
        DISPLAY "No file path specified."
        STOP RUN
    END-IF
    
    CALL "read-code-file" USING CONTENT code-file-path, REFERENCE code-area

    MOVE find-initial(code-area) TO instruction-ptr (1, 1)
    .
010-interpret-code.
    SET right-dir (1, 1) TO TRUE
    SET stack-idx, thread-idx, row-idx (1), col-idx (1) TO 1
    PERFORM UNTIL num-threads = 0
        PERFORM VARYING thread-idx FROM 1 BY 1 UNTIL thread-idx > num-threads
            PERFORM 100-move-instruction-ptr
            IF out-of-code-space
                PERFORM 200-stop-thread
            END-IF
 
            EVALUATE code-chars (ip-line (thread-idx, stack-idx),
                    ip-char (thread-idx, stack-idx))
                *> Core SNUSP
                WHEN "<" *> LEFT
                    SET col-idx (thread-idx) DOWN BY 1
 
                WHEN ">" *> RIGHT
                    SET col-idx (thread-idx) UP BY 1
 
                WHEN "+" *> INCR
                    ADD 1 TO
                       memory-cell (row-idx (thread-idx), col-idx (thread-idx))
 
                WHEN "-" *> DECR
                    SUBTRACT 1 FROM
                        memory-cell (row-idx (thread-idx), col-idx (thread-idx))
 
                WHEN "." *> WRITE
                    IF NOT write-numbers
                        DISPLAY memory-cell-char (row-idx (thread-idx),
                            col-idx (thread-idx))
                    ELSE
                        DISPLAY memory-cell (row-idx (thread-idx),
                            col-idx (thread-idx))
                    END-IF
 
                WHEN "," *> READ
                    IF NOT read-numbers
                        ACCEPT memory-cell-char (row-idx (thread-idx),
                            col-idx (thread-idx))
                    ELSE
                        ACCEPT memory-cell (row-idx (thread-idx),
                            col-idx (thread-idx))
                    END-IF
 
                *> LURD (/ is not used as it is mucks up syntax highlighting.)
                WHEN X"5C"
                    EVALUATE TRUE
                        WHEN up-dir (thread-idx, stack-idx)
                            SET left-dir (thread-idx, stack-idx) TO TRUE
                        WHEN down-dir (thread-idx, stack-idx)
                            SET right-dir (thread-idx, stack-idx) TO TRUE
                        WHEN left-dir (thread-idx, stack-idx)
                            SET up-dir (thread-idx, stack-idx) TO TRUE
                        WHEN right-dir (thread-idx, stack-idx)
                            SET down-dir (thread-idx, stack-idx) TO TRUE
                    END-EVALUATE
 
 
                WHEN "/" *> RULD
                    EVALUATE TRUE
                        WHEN up-dir (thread-idx, stack-idx)
                            SET right-dir (thread-idx, stack-idx) TO TRUE
                        WHEN down-dir (thread-idx, stack-idx)
                            SET left-dir (thread-idx, stack-idx) TO TRUE
                        WHEN left-dir (thread-idx, stack-idx)
                            SET down-dir (thread-idx, stack-idx) TO TRUE
                        WHEN right-dir (thread-idx, stack-idx)
                            SET up-dir (thread-idx, stack-idx) TO TRUE
                    END-EVALUATE
 
                WHEN "!" *> SKIP
                    PERFORM 100-move-instruction-ptr
 
                WHEN "?" *> SKIPZ
                    IF memory-cell (row-idx (thread-idx), col-idx (thread-idx))
                            = 0
                        PERFORM 100-move-instruction-ptr
                    END-IF
 
                *> Modular SNUSP
                WHEN "@" *> ENTER
                    *> Push current direction and IP location onto call stack
                    MOVE calls (thread-idx, stack-idx)
                        TO calls (thread-idx, stack-idx + 1)
                    SET stack-idx UP BY 1
 
                WHEN "#" *> LEAVE
                    IF stack-idx <> 1
                        *> Pop direction and IP location off call stack and
                        *> advance the IP one step.
                        SET stack-idx DOWN BY 1
                        PERFORM 100-move-instruction-ptr
                    ELSE
                        PERFORM 200-stop-thread
                    END-IF
 
                *> Bloated SNUSP
                WHEN ":" *> UP
                    SET row-idx (thread-idx) UP BY 1
 
                WHEN ";" *> DOWN
                    SET row-idx (thread-idx) DOWN BY 1
 
                WHEN "&" *> SPLIT
                    *> Create a new thread
                    ADD 1 TO num-threads
                    MOVE call-stack (thread-idx) TO call-stack (num-threads)
                    MOVE memory-pointer (thread-idx) TO call-stack (num-threads)
                    SET thread-started (thread-idx) TO TRUE
 
                WHEN "%" *> RAND
                    COMPUTE memory-cell (row-idx (thread-idx),
                            col-idx (thread-idx)) =
                        FUNCTION MOD(FUNCTION RANDOM,
                            memory-cell (row-idx (thread-idx),
                                col-idx (thread-idx)) + 1)
                                
                WHEN OTHER
                    CONTINUE
            END-EVALUATE
 
            IF out-of-code-space
                PERFORM 200-stop-thread
            END-IF
        END-PERFORM
    END-PERFORM
    .
099-terminate.
    GOBACK
    .
100-move-instruction-ptr SECTION.
    EVALUATE TRUE
        WHEN up-dir (thread-idx, stack-idx)
            SET ip-line (thread-idx, stack-idx) DOWN BY 1
        WHEN down-dir (thread-idx, stack-idx)
            SET ip-line (thread-idx, stack-idx) UP BY 1
        WHEN left-dir (thread-idx, stack-idx)
            SET ip-char (thread-idx, stack-idx) DOWN BY 1
        WHEN right-dir (thread-idx, stack-idx)
            SET ip-char (thread-idx, stack-idx) UP BY 1
    END-EVALUATE
    .
199-exit.
    EXIT
    .
200-stop-thread SECTION.
    *> Shift data from following threads over stopped thread.
    SET current-thread-idx TO thread-idx
    PERFORM VARYING thread-idx FROM thread-idx BY 1
            UNTIL NOT thread-started (thread-idx + 1)
                OR thread-idx = num-threads
        MOVE threads-data (thread-idx + 1) TO threads-data (thread-idx)
    END-PERFORM
 
    SUBTRACT 1 FROM num-threads
    SET thread-idx TO current-thread-idx
    .
299-exit.
    EXIT
    .
END PROGRAM snusp-interpreter.


IDENTIFICATION DIVISION.
PROGRAM-ID. parse-arguments.

DATA DIVISION.
LOCAL-STORAGE SECTION.
01  num-args                            PIC 9 COMP.
01  arg-num                             PIC 9 COMP.
01  arg                                 PIC X(100).

COPY "dd-flag-constants.cpy".

01  program-flag                        PIC X.
    88  help-flag                       VALUE Help-Flag-Char.
    88  read-num-flag                   VALUE Read-Num-Flag-Char.
    88  write-num-flag                  VALUE Write-Num-Flag-Char.

LINKAGE SECTION.
COPY "dd-program-arguments.cpy".

PROCEDURE DIVISION USING program-arguments.
    ACCEPT num-args FROM ARGUMENT-NUMBER
    IF num-args = 0
        CALL "display-help"
        STOP RUN
    END-IF

    PERFORM VARYING arg-num FROM 1 BY 1 UNTIL arg-num > num-args
        DISPLAY arg-num UPON ARGUMENT-NUMBER
        ACCEPT arg FROM ARGUMENT-VALUE
        EVALUATE TRUE
            WHEN arg (1:1) = Flag-Indicator
                MOVE arg (2:1) TO program-flag
                EVALUATE TRUE
                    WHEN help-flag
                        CALL "display-help"
                        STOP RUN
                    WHEN read-num-flag
                        SET read-numbers TO TRUE
                    WHEN write-num-flag
                        SET write-numbers TO TRUE
                    WHEN OTHER
                        DISPLAY "Flag '" FUNCTION TRIM(arg) "' not recongnized."
                END-EVALUATE
 
            WHEN code-file-path <> SPACES
                DISPLAY "Argument " arg-num " ignored - only one source code "
                    "file can be interpreted."
 
            WHEN OTHER
                MOVE arg TO code-file-path
        END-EVALUATE
    END-PERFORM
    .
END PROGRAM parse-arguments.


IDENTIFICATION DIVISION.
PROGRAM-ID. read-code-file.

ENVIRONMENT DIVISION.
INPUT-OUTPUT SECTION.
FILE-CONTROL.
    SELECT code-file ASSIGN code-file-path
        ORGANIZATION LINE SEQUENTIAL
        FILE STATUS code-file-status.
 
DATA DIVISION.
FILE SECTION.
FD  code-file.
01  code-record                         PIC X(100).
 
LOCAL-STORAGE SECTION.
01  code-file-status                    PIC 99.
    88  end-of-code-file                VALUE 10.

LINKAGE SECTION.
COPY "dd-code-area.cpy".

01  code-file-path                      PIC X(100).

PROCEDURE DIVISION USING code-file-path, code-area.
DECLARATIVES.
code-file-error SECTION.
    USE AFTER ERROR ON code-file.
 
    DISPLAY "An error occurred while using " FUNCTION TRIM(code-file-path)
    DISPLAY "Error code " code-file-status
    DISPLAY "The program will terminate."
 
    STOP RUN
    .
END DECLARATIVES.

    OPEN INPUT code-file
    PERFORM VARYING line-idx FROM 1 BY 1 UNTIL end-of-code-file
        READ code-file INTO code-lines (line-idx)
            NOT AT END
                ADD 1 TO num-lines
            AT END
                EXIT PERFORM
        END-READ
    END-PERFORM
 
    CLOSE code-file
    .
END PROGRAM read-code-file.


IDENTIFICATION DIVISION.
FUNCTION-ID. find-initial.

DATA DIVISION.
LINKAGE SECTION.
COPY "dd-code-area.cpy".

01  instruction-ptr.
    03  ip-line                         USAGE INDEX.
    03  ip-char                         USAGE INDEX.

PROCEDURE DIVISION USING code-area RETURNING instruction-ptr.
    PERFORM VARYING ip-line FROM 1 BY 1 UNTIL ip-line > num-lines
            AFTER ip-char FROM 1 BY 1 UNTIL ip-char > 100
        IF code-chars (ip-line, ip-char) = "$"
            EXIT PERFORM
        END-IF
    END-PERFORM
 
    *> Set position to first char if no initial characters were found.
    IF ip-line > num-lines
        SET ip-line, ip-char TO 1
    END-IF
    .
END FUNCTION find-initial.


IDENTIFICATION DIVISION.
PROGRAM-ID. display-help.

DATA DIVISION.
WORKING-STORAGE SECTION.
COPY "dd-flag-constants.cpy".

01  Tab-Char                            CONSTANT X"09".

PROCEDURE DIVISION.
    DISPLAY "This is a interpreter for SNUSP written in COBOL."
    DISPLAY "The file path to the source code should be specified as a "
        "command-line argument."
    DISPLAY "This program supports the following flags as arguments:"
    DISPLAY Tab-Char Flag-Indicator Help-Flag-Char ": Displays this help "
        "message."
    DISPLAY Tab-Char Flag-Indicator Write-Num-Flag-Char ": Display memory "
        "contents as numbers."
    DISPLAY Tab-Char Flag-Indicator Read-Num-Flag-Char ": Reads a byte to  "
        "memory as a number."
    .
END PROGRAM display-help.
```


dd-code-area.cpy:

```cobol
01  code-area.
    03  num-lines                       PIC 9(4) COMP.
    03  code-lines                      OCCURS 1 TO 1024 TIMES
                                        DEPENDING ON num-lines
                                        INDEXED BY line-idx.
        05  code-chars                  PIC X OCCURS 100 TIMES.
```


dd-flag-constants.cpy:

```cobol
01  Flag-Indicator                      CONSTANT "-".
01  Help-Flag-Char                      CONSTANT "h".
01  Read-Num-Flag-Char                  CONSTANT "r".
01  Write-Num-Flag-Char                 CONSTANT "w".
```


dd-program-arguments.cpy:

```cobol
01  program-arguments.
    03  code-file-path                  PIC X(100).
    03  read-flag                       PIC X.
        88 read-numbers                 VALUE "N". 
    03  write-flag                      PIC X.
        88  write-numbers               VALUE "N".
```

