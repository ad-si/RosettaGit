+++
title = "Execute Brain****/Clojure"
description = ""
date = 2013-03-07T00:17:08Z
aliases = []
[extra]
id = 13082
[taxonomies]
categories = []
tags = []
+++

{{implementation|Brainf***}}{{collection|RCBF}}
This is a [[brainfuck]] interpreter in [[Clojure]] which completely refrains from mutating any state. The nature of Clojure (namely, that hashes and sets are functions) facilitates a rather clean <tt>loop</tt>/<tt>recur</tt> approach. At each iteration, the components of the execution model (the program counter, byte pointer, and tape) are "redefined" as the application of some function (informed by the current instruction) to their previous value.

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

{{out}}

```txt
$ echo "(> fvzcyr rnfl)" | lein exec bf.clj rot13.bf
(> simple easy)
```

