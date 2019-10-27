+++
title = "Execute Brain****/Scheme"
description = ""
date = 2011-09-09T09:10:09Z
aliases = []
[extra]
id = 10492
[taxonomies]
categories = []
tags = []
+++

{{implementation|Brainf***}}{{collection|RCBF}}
{{works with|Guile}}
Expects a Brainf*** source code file as a command-line argument.

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

