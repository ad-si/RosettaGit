+++
title = "Execute HQ9+/E"
description = ""
date = 2010-02-06T14:28:50Z
aliases = []
[extra]
id = 4606
[taxonomies]
categories = []
tags = []
+++


[The original author of this example](https://rosettacode.org/wiki/User:Kevin_Reid) (written in [E](https://rosettacode.org/wiki/E)) would like to note the following deficiencies in [http://www.cliff.biffle.org/esoterica/hq9plus.html the HQ9+ specification]:

* The treatment of unrecognized characters other than whitespace is unspecified. However, the only implementation provided, in [OCaml](https://rosettacode.org/wiki/OCaml), rejects them, so this implementation also does. Additionally, rejecting unknown characters ensures that the compiler will not incorrectly compile [HQ9+](https://rosettacode.org/wiki/HQ9+) programs using future extensions.
* While the given "qqqq" example implies that whitespace (specifically, trailing newlines) is permissible and that operation codes are case-insensitive, neither of these are explicitly stated.
* The initial value of the accumulator is unspecified. This implementation has chosen 11472, which is of course the value such that executing the HQ9+ program "HQ9+" will result in the accumulator having a value equal to the length of the program's output.

<br clear=all>
```e>def makeSeqExpr := <elang:evm.makeSeqExpr

def makeLiteralExpr := <elang:evm.makeLiteralExpr>
def eParser := <elang:syntax.makeEParser>

# the extra $ protects this as an RC example by breaking up the close tag
def `@_
```e
@beerSource</la${""}ng>@_` :=
  <http://rosettacode.org/mw/index.php?title=99_Bottles_of_Beer&action=raw>.getText()
def beerProgram := eParser(beerSource)

def opcodes := [
  'H' => def h := e`stdout.println("Hello, World!")`,
  'Q' => def q := e`stdout.println(mySource)`,
  '9' =>          e`{ def println := stdout.println; $beerProgram }`,
  '+' =>          e`::"the accumulator" += 1`,
  'h' => h,
  'q' => q,
]

/** HQ9+ compiler.
    
    The provided program is compiled into a function taking the output stream as an argument. */
def ::"RCHQ9+"(program :String) {
  return e`
    def ::"RCHQ9+ program"(stdout) {
      def mySource := ${makeLiteralExpr(null, program, null)}
      var ::"the accumulator" := 11472
      ${
        var exprs := []
        for ch ? (E.toString(ch) =~ rx`\S`) in program {
          exprs with= opcodes.fetch(ch, fn {
            throw(`Unrecognized HQ9+ opcode: $ch`)
          })
        }
        makeSeqExpr(null, exprs, null)
      }
    }
  `.eval(safeScope)
  # Maintenance note: Do not change this from safeScope without considering
  # the consequences as the beerProgram is incorporated from a wiki source
  # over unsecured HTTP.
}
```

