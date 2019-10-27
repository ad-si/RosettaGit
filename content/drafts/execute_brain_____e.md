+++
title = "Execute Brain****/E"
description = ""
date = 2010-02-06T14:19:04Z
aliases = []
[extra]
id = 4208
[taxonomies]
categories = []
tags = []
+++

{{implementation|Brainf***}}{{collection|RCBF}}

This [[Brainf***|BF]] implementation compiles BF programs into functions (from strings to strings) in the host language, [[E]].

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

