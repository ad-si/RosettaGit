+++
title = "Clojure"
description = ""
date = 2011-09-19T03:00:46Z
aliases = []
[extra]
id = 2901
[taxonomies]
categories = []
tags = []
+++
Clojure is a dynamic programming language that targets the [runs on vm::Java Virtual Machine](https://rosettacode.org/wiki/runs_on_vm::Java_Virtual_Machine). It is designed to be a general-purpose language, combining the approachability and interactive development of a scripting language with an efficient and robust infrastructure for multithreaded programming. Clojure is a compiled language - it compiles directly to JVM [bytecode](https://rosettacode.org/wiki/bytecode), yet remains completely dynamic. Every feature supported by Clojure is supported at runtime. Clojure provides easy access to the [Java](https://rosettacode.org/wiki/Java) frameworks, with optional type hints and type inference, to ensure that calls to Java can avoid reflection.

Clojure is a dialect of [Lisp](https://rosettacode.org/wiki/Lisp), and shares with Lisp the code-as-data philosophy and a powerful macro system. Clojure is predominantly a functional programming language, and features a rich set of immutable, persistent data structures. When mutable state is needed, Clojure offers a software transactional memory system and reactive Agent system that ensure clean, correct, multithreaded designs.

## See Also
* [http://richhickey.github.com/clojure-contrib/ clojure-contrib] -- Clojure's contrib library


## Merged content



This is a [brainfuck](https://rosettacode.org/wiki/brainfuck) interpreter in [Clojure](https://rosettacode.org/wiki/Clojure) which completely refrains from mutating any state. The nature of Clojure (namely, that hashes and sets are functions) facilitates a rather clean <tt>loop</tt>/<tt>recur</tt> approach. At each iteration, the components of the execution model (the program counter, byte pointer, and tape) are "redefined" as the application of some function (informed by the current instruction) to their previous value.

```Clojure
(defn pair-brackets [indexed]
  "indexed is a sequence of 2-tuples of the form [index instruction].
  This is reduced into a hash of indices that is used to inform jumps."
  (reduce
    (fn [pairs cur]
      (condp = (cur 1)
        \[ (merge-with conj pairs {:pend (cur 0)}) ; new pending open bracket
        \] (merge pairs
                  {(first (pairs :pend)) (cur 0) ; store both
                   (cur 0) (first (pairs :pend)) ; directions
                   :pend (pop (pairs :pend))}) ; no longer pending
        pairs))
    {:pend ()} indexed))

(defn brainfuck [insns]
  (let
    [valid (set "[]<>-+.,")
     program (vec (keep #(valid %) insns)) ; sets are functions of their values
     jumps (pair-brackets
             (map vector (range) program))]
    (loop
      [pc 0 ; program counter
       bp 0 ; byte pointer
       tape {}]
      (if (< pc (count program))
        (let [insn (program pc)
              cur (tape bp 0)]
          (if (= \. insn)
            (print (char cur)))
          (recur
            ; program counter
            ((cond ; use jumps hash as the function if applicable, otherwise inc
               (and (= \[ insn) (= 0 cur)) jumps
               (and (= \] insn) (not= 0 cur)) jumps
               :else inc) pc)

            ; byte pointer
            (({\< dec \> inc} insn identity) bp) ; function hash with fallback

            ; tape
            (cond
              (#{\- \+} insn)
              (merge tape {bp (({\- dec \+ inc} insn) cur)})
              (= \, insn)
              (merge tape {bp (. System/in read)})
              :else tape)))))))

(-> *command-line-args*
  last slurp brainfuck)
```
```txt
$ echo "(> fvzcyr rnfl)" | lein exec bf.clj rot13.bf
(> simple easy)
```

