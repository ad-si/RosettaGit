+++
title = "Common Lisp"
description = ""
date = 2013-01-14T20:08:33Z
aliases = []
[extra]
id = 1714
[taxonomies]
categories = []
tags = []
+++
'''Common Lisp''', commonly abbreviated '''CL''', is a dialect of the [derived from::Lisp](https://rosettacode.org/wiki/derived_from::Lisp) programming language, standardised by [ANSI](https://rosettacode.org/wiki/ANSI) X3.226-1994. Developed as a common successor to Maclisp and Lisp Machine Lisp, it is not an implementation but a language specification. Several implementations of the Common Lisp standard are available, including commercial products and [open source](https://rosettacode.org/wiki/open_source) software.

Common Lisp is a general-purpose programming language, in contrast to Lisp variants such as [Emacs Lisp](https://rosettacode.org/wiki/Emacs_Lisp) and AutoLISP which are embedded extension languages in particular products. Unlike many earlier Lisps, Common Lisp (like [derived from::Scheme](https://rosettacode.org/wiki/derived_from::Scheme)) uses lexical variable scope.

The "CL-USER> " prompt seen in front of some examples shows that the code was run in a Lisp [read-eval-print loop](https://rosettacode.org/wiki/read-eval-print_loop), or REPL, running interactively in an environment such as [SLIME](https://rosettacode.org/wiki/SLIME).

## Citations
* [Wikipedia:Common Lisp](https://en.wikipedia.org/wiki/Common_lisp)


## Merged content




This is an implementation of [Brainfuck](https://rosettacode.org/wiki/Brainfuck) written in [Common Lisp](https://rosettacode.org/wiki/Common_Lisp).


```lisp
(defstruct bf-state
  (program)
  (program-counter 0)
  (memory (make-array 1 :initial-element 0 :adjustable t))
  (memory-pointer 0)
  (stack))

(defmacro with-bf-slots ((program-sym program-counter-sym
                          memory-sym memory-pointer-sym
                          stack-sym)
                         obj-expr &body body)
 "Macro to replace cumbersome structure slot references with
local lexical macros resembling local variables."
  `(symbol-macrolet ((,program-sym (bf-state-program ,obj-expr))
                     (,program-counter-sym (bf-state-program-counter ,obj-expr))
                     (,memory-sym (bf-state-memory ,obj-expr))
                     (,memory-pointer-sym (bf-state-memory-pointer ,obj-expr))
                     (,stack-sym (bf-state-stack ,obj-expr)))
     ,@body))

(defun adjust-memory (state)
 "Modifies memory and memory-pointer such that memory-pointer is
a valid index to the memory array. If it is too large, the array
is extended; if it is negative, the array is extended, its
contents are shifted forward and the memory-pointer is incremented,
by an amount to make the memory ."
  (with-bf-slots (program pc mem ptr stack) state
    (cond ((>= ptr (length mem))
           (adjust-array mem (1+ ptr) :initial-element 0))
          ((minusp ptr)
           (let ((extent (- ptr)))
             (incf ptr extent)
             (let ((old-memory (copy-seq mem)))
               (setf mem (make-array (+ (length old-memory) extent)))
               (setf (subseq mem extent) old-memory)))))))

(defun matching-bracket-for (program bracket-index)
  (loop with depth = 1
        for index from (1+ bracket-index)
        when (>= index (length program))
          do (error "unmatched [ bracket")
        do (case (aref program index)
             (#\[ (incf depth))
             (#\] (decf depth)))
        until (zerop depth)
        finally (return index)))

(defun brainfuck-eval (state &optional (stream *standard-output*))
  (with-bf-slots (program pc mem ptr stack) state
    (loop while (< pc (length program)) do
      (case (aref program pc)
        (#\+ (incf (aref mem ptr)))
        (#\- (decf (aref mem ptr)))
        (#\> (incf ptr) (adjust-memory state))
        (#\< (decf ptr) (adjust-memory state))
        (#\[ (if (/= 0 (aref mem ptr))
                 (push (1- pc) stack)
                 (setf pc (matching-bracket-for program pc))))
        (#\] (setf pc (pop stack)))
        (#\. (write-char (code-char (aref mem ptr)) stream)))
      (incf pc))))

(defun brainfuck-compile-guts (program &optional (start 0) (until-bracket nil))
  (loop for insn from start below (length program)
        appending (case (aref program insn)
                    (#\+ `((incf (aref mem ptr))))
                    (#\- `((decf (aref mem ptr))))
                    (#\> `((incf ptr) (adjust-memory state)))
                    (#\< `((decf ptr) (adjust-memory state)))
                    (#\[ (let ((end (matching-bracket-for program insn)))
                           (prog1
                             `((do () ((= 0 (aref mem ptr)))
                               ,@(brainfuck-compile-guts program (1+ insn) end)))
                             (setf insn end))))
                    (#\] (if until-bracket
                           (if (= until-bracket insn)
                             (loop-finish)
                             (error "internal problem matching brackets"))
                           (error "extra ] bracket")))
                    (#\. `((write-char (code-char (aref mem ptr)) stream))))))

(defun brainfuck-compile (program)
  (compile nil `(lambda (&optional (stream *standard-output*))
                  (let ((state (make-bf-state :program ,program)))
                    (with-bf-slots (program pc mem ptr stack) state
                                   ,@(brainfuck-compile-guts program))
                    (values)))))

(defun bf (program)
  (if (and (not (zerop (length program)))
           (char= #\! (aref program 0)))
    (funcall (brainfuck-compile program))
    (brainfuck-eval (make-bf-state :program program))))

(defun bf-repl ()
  "read-eval-print loop for bf. Code prefixed with ! is compiled, otherwise interpreted"
  (loop do (fresh-line)
           (princ "BRAINFUCK> ")
           (bf (read-line))))
```

