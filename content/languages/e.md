+++
title = "E"
description = ""
date = 2014-03-15T19:24:40Z
aliases = []
[extra]
id = 2006
[taxonomies]
categories = []
tags = []
+++

E is a programming language designed around object-capability security and event-loop concurrency, in order to support creation of highly robust and secure programs.

Using the [Java](https://rosettacode.org/wiki/Java) implementation of E, Java libraries can be used from E code, and its REPL (e.g. [Creating a Window](https://rosettacode.org/wiki/Creating_a_Window#E)).

## Trying E
[http://www.erights.org/download/ The current recommended E implementation ("E-on-Java") may be downloaded from erights.org].

To run an E program:


```sh
$ rune program.e
```


To get a [REPL](https://rosettacode.org/wiki/REPL) (aka prompt, shell, interactive interpreter):


```sh
$ rune
```


An online REPL is also available at [Rosetta Code IRC](https://rosettacode.org/wiki/Help:IRC) or <code>[irc://chat.freenode.net/erights #erights]</code> on <code>chat.[http://freenode.net/ freenode.net]</code>.

## Syntax of examples
While most of the examples on Rosetta Code are E expressions (programs), some may be written like this:

 ? ''expression''
 # value: ''print representation''

This is both the format of a transcript at an E [REPL](https://rosettacode.org/wiki/REPL), and the format employed by [http://wiki.erights.org/wiki/Updoc Updoc], a test framework for E. “?” is a prompt for user input (“&gt;” indicates multi-line input) and “# foo:” indicates responses.
* <code># value:</code> the return value of the ''expression'', printed
* <code># problem:</code> an exception thrown by evaluation of the ''expression''
* <code># syntax error:</code> an exception thrown by parsing of the ''expression''
* <code># stdout:</code> or <code># stderr:</code> text written to the <code>stdout</code> or <code>stderr</code> streams. It is typically only used in test scripts and not in ordinary interactive sessions.

To try out these examples for yourself, just install E and run the <code>rune</code> command to get the “?” prompt. Multi-line input is automatic for unbalanced brackets/parens and can be indicated in other cases by a trailing backslash.

## See Also
* [http://wiki.erights.org E Wiki]


## Merged content




This [BF](https://rosettacode.org/wiki/Brainfuck) implementation compiles BF programs into functions (from strings to strings) in the host language, [E](https://rosettacode.org/wiki/E).

It has a fixed-size memory defined in terms of characters, not bytes, and initialized to the null character. It does not permit the BF program to notice EOF.


```e
pragma.syntax("0.9")
pragma.enable("accumulator")

def makeLiteralExpr := <elang:evm.makeLiteralExpr>
def makeSeqExpr     := <elang:evm.makeSeqExpr>

def simpleInstructions := [
  '>' => e`ptr        +=        1`,
  '<' => e`ptr        -=        1`,
  '+' => e`array[ptr] next=     ()`,
  '-' => e`array[ptr] previous= ()`,
  '.' => e`out        +=        E.toString(array[ptr])`,
  ',' => e`array[ptr] :=        inputString[inPtr += 1]`,
]

def bfCompile(prog) {
  # Stack holding nested loop bodies collected so far
  def stack := [[]].diverge()

  for ch in prog {
    switch (ch) {

      match =='[' {
        stack.push([])
      }

      match ==']' {
        # take enclosed program off top of stack and append to next down inside a loop
        stack[stack.size() - 2] with= e`
          while (array[ptr] != '\u0000') {
            ${makeSeqExpr(null, stack.pop(), null)}
          }
        `
      }

      match _ ? (simpleInstructions.maps(ch)) {
        # XXX the {} will be unnecessary once e`` goes to using selfish temps
        stack[stack.size() - 1] with= e`{${simpleInstructions[ch]}}`
      }

      match _ {}
    }
  }

  # Take the one remaining stack item
  def [bodyExprs] exit fn _{throw("Too few right brackets")} \
                  := stack.snapshot()

  # How the BF function object will appear
  def printDesc := makeLiteralExpr(null, "<BF " + if (prog.indexOf(",") != -1) {"function"} else {"thunk"} + ">", null)

  return e`
    pragma.syntax("0.9")

    def bfo := def _ {
      to __printOn(out :TextWriter) {
        out.print($printDesc)
      }

      /** Equivalent to passing the empty string */
      to run() { return bfo("") }

      to run(inputString) {
        def array := ("\u0000" * 30000).diverge(char)
        var ptr := 0

        var inPtr := -1
        var out := ""

        ${makeSeqExpr(null, bodyExprs, null)}

        return out
      }
    }
  `.eval(safeScope)
}
```


Example usage:


```e
? def incr3 := bfCompiler(",>,>,><<<[+.>]")
# value: <BF function>

? incr3("abc")
# value: "bcd"
```

