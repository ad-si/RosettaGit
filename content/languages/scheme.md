+++
title = "Scheme"
description = ""
date = 2017-03-02T22:31:52Z
aliases = []
[extra]
id = 1709
[taxonomies]
categories = []
tags = []
+++
'''Scheme''' is a multi-[paradigm](https://rosettacode.org/wiki/:Category:Programming_Paradigms) programming language. It is one of the two main dialects of [derived from::Lisp](https://rosettacode.org/wiki/derived_from::Lisp) and supports a number of programming paradigms; however it is best known for its support of [functional programming](https://rosettacode.org/wiki/functional_programming). It was developed by Guy L. Steele and Gerald Jay Sussman in the 1970s. Scheme was introduced to the academic world via a series of papers, now referred to as Sussman and Steele's Lambda Papers. There are two standards that define the Scheme language: the official [IEEE](https://rosettacode.org/wiki/IEEE) standard, and a de facto standard called the ''Revised<sup>n</sup> Report on the Algorithmic Language Scheme'', nearly always abbreviated R''n''RS, where ''n'' is the number of the revision. The current standard is '''R7RS''', with '''R5RS''' and, less common, '''R6RS''' still in use.

Scheme's philosophy is minimalist. Scheme provides as few primitive notions as possible, and, where practical, lets everything else be provided by programming libraries.

Scheme was the first dialect of Lisp to choose static (a.k.a. lexical) over dynamic variable scope. It was also one of the first programming languages to support first-class continuations.

## Running Examples
Some examples from this site require particular versions of Scheme, or libraries, to run.

* R7RS programs typically begin with a line such as <tt>(import (scheme base) ...)</tt>
* R6RS programs with a line such as <tt>(import (rnrs) ...)</tt>
* R5RS programs don't require any preamble.


A semi-standard set of libraries for Scheme is the collection [SRFIs](https://rosettacode.org/wiki/:Category:Scheme/SRFIs) (from Scheme Requests For Implementation).  These libraries provide additional functions operating on core data structures, such as SRFI-1 for lists and SRFI-13 for strings; additional data structures, such as SRFI-69 or SRFI-125 for hash tables; or additional functionality, such as SRFI-42 providing eager comprehensions.  Example programs which require one or more SRFIs must be run on implementations which support that SRFI.

Scheme does not directly support a GUI library: some examples use [PsTk](https://rosettacode.org/wiki/:Category:Scheme/PsTk).

## Citations
* [Wikipedia:Scheme (programming language)](https://en.wikipedia.org/wiki/Scheme_%28programming_language%29)
* [http://trac.sacrideo.us/wg/wiki/R7RSHomePage R7RS Scheme home page]
* [http://www.r6rs.org/ R6RS Scheme home page]
* [http://www.schemers.org/Documents/Standards/R5RS/ R5RS Scheme documentation]
## Merged content



Expects a Brainfuck source code file as a command-line argument.

```scheme

; A Brainfuck interpreter in Scheme
(define bf-stack-size 30000)

(define bf-file "")
(if (> (length (command-line)) 1)
    (set! bf-file (open-input-file (list-ref (command-line) 1))))

; Main program
(define (bf-run program stack-size)
    ; Create the stack filled with 0s
    (define stack (make-vector stack-size 0))
    ; Get first command and set program counter and stack pointer to initial address
    (let loop ((command (vector-ref program 0)) (program-counter 0) (stack-pointer 0))
        ; Execute the current command
        (cond
            ((equal? command #\>)
                (set! stack-pointer (+ stack-pointer 1))
                (if (equal? stack-pointer stack-size)
                    (bf-error (string-append
                            "Stack Overflow at "
                            (number->string program-counter)))))
            ((equal? command #\<)
                (set! stack-pointer (- stack-pointer 1))
                (if (negative? stack-pointer)
                    (bf-error (string-append
                            "Stack Underflow at "
                            (number->string program-counter)))))
            ((equal? command #\+)
                (vector-set! stack stack-pointer (+ (vector-ref stack stack-pointer) 1)))
            ((equal? command #\-)
                (vector-set! stack stack-pointer (- (vector-ref stack stack-pointer) 1)))
            ((equal? command #\.)
                (display (integer->char (vector-ref stack stack-pointer))))
            ((equal? command #\,)
                (vector-set! stack stack-pointer
                    (let ((c (read-char)))
                        (if (eof-object? c)
                            0
                            (char->integer c)))))
            ((equal? command #\[)
                (if (zero? (vector-ref stack stack-pointer))
                    (let loop ((cmd (vector-ref program program-counter))(depth 0))
                        (cond
                            ((equal? cmd #\[)
                                (set! depth (+ depth 1)))
                            ((equal? cmd #\])
                                (set! depth (- depth 1))))
                        (set! program-counter (+ program-counter 1))
                        (if (not (zero? depth))
                            (loop (vector-ref program program-counter) depth)))
                        (set! program-counter (+ program-counter 1))))
            ((equal? command #\])
                (if (not (zero? (vector-ref stack stack-pointer)))
                    (let loop ((cmd (vector-ref program program-counter))(depth 0))
                        (cond
                            ((equal? cmd #\])
                                (set! depth (+ depth 1)))
                            ((equal? cmd #\[)
                                (set! depth (- depth 1))))
                        (set! program-counter (- program-counter 1))
                        (if (not (zero? depth))
                            (loop (vector-ref program program-counter) depth))))
                (set! program-counter (+ program-counter 1)))
            (else
                (bf-error (string-append "Invalid command found at " (number->string program-counter)))))
        ; Increment program-counter if needed (if not changed by [ or ] that is)
        (if (and (not (equal? command #\[)) (not (equal? command #\])))
            (set! program-counter (+ program-counter 1)))
        ; Repeat for next command if available. Quit otherwise.
        (cond
            ((and (< program-counter (vector-length program)) (>= program-counter 0))
                (loop (vector-ref program program-counter) program-counter stack-pointer))
            (else
                (quit 0)))))

; Read the program from file striping out any characters that aren't commands
(define (bf-get-program-from-file file)
    (define program '())
    (let loop ((c (read-char file)))
        (cond
            ((eof-object? c)
                (list->vector program))
            ((or (equal? c #\>) (equal? c #\<) (equal? c #\+) (equal? c #\-)
                 (equal? c #\.) (equal? c #\,) (equal? c #\[) (equal? c #\]))
                (set! program (append program (list c)))
                (loop (read-char file)))
            (else
                (loop (read-char file))))))

; Execute the file
(bf-run (bf-get-program-from-file bf-file) bf-stack-size)

```

