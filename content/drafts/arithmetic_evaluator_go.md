+++
title = "Arithmetic Evaluator/Go"
description = ""
date = 2013-06-17T17:48:44Z
aliases = []
[extra]
id = 7194
[taxonomies]
categories = []
tags = []
+++

__TOC__
=Operator precedence parser=
This is an operator precedence parser. The number format used in calculation can be changed with the line "type Number int".


```go
package main

import (
   "bufio"
   "fmt"
   "os"
   "strings"
)

/* 
### = AST =
 */

type Number float64

type Node interface {
   Eval() (Number,bool)
}

// Binary operator AST node
type Binary struct {
   op byte
   left Node
   right Node
}

func (n *Binary) Init(op byte, left, right Node) Node {
   n.op = op
   n.left = left
   n.right = right
   return n
}

func (n *Binary) Eval() (Number,bool) {
   left, ok := n.left.Eval()
   if !ok { return 0, false }
   right, ok := n.right.Eval()
   if !ok { return 0, false }
   switch n.op {
      case '+': return left + right, true
      case '-': return left - right, true
      case '*': return left * right, true
      case '/':
         if right == 0 { return 0, false }
         return left / right, true
   }
   return 0, false
}

func (n *Binary) String() string {
   return fmt.Sprintf("(%s %c %s)", n.left, n.op, n.right)
}

// Leaf value AST node
type Leaf struct {
   value Number
}

func (n *Leaf) Init(value Number) Node {
   n.value = value
   return n
}

func (n *Leaf) Eval() (Number,bool) {
   return n.value,true
}

func (n *Leaf) String() string {
   return fmt.Sprintf("%v", n.value)  // %v = default format
}

/* 
### = Lexer =
 */

type Lexer struct {
   data string
   pos int
   Kind int
   Num Number
   Oper byte
}

const (
   ERR = iota  // error
   NUM         // number
   LPAR        // left parenthesis
   RPAR        // right parenthesis
   OP          // operator
)

func (lexer *Lexer) Init(data string) *Lexer {
   lexer.data = data
   lexer.pos = 0
   return lexer
}

func (l *Lexer) Next() int {
   n := len(l.data)
   l.Kind = ERR
   if l.pos < n {
      switch char := l.data[l.pos]; char {
         case '+', '-', '*', '/':
            l.pos++
            l.Kind = OP
            l.Oper = char
         case '(':
            l.pos++
            l.Kind = LPAR
            l.Oper = char
         case ')':
            l.pos++
            l.Kind = RPAR
            l.Oper = char
         case '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '.':
            var value Number = 0
            var divisor Number = 1
            for ; l.pos < n && '0' <= l.data[l.pos] && l.data[l.pos] <= '9'; l.pos++ {
               value = value * 10 + Number(l.data[l.pos] - '0')
            }
            if l.pos < n && l.data[l.pos] == '.' {
               l.pos++
               for ; l.pos < n && '0' <= l.data[l.pos] && l.data[l.pos] <= '9'; l.pos++ {
                  value = value * 10 + Number(l.data[l.pos] - '0')
                  divisor *= 10
               }
            }
            l.Kind = NUM
            l.Num = value / divisor
      }
   }
   return l.Kind
}

/* 
### = Parser =
 */

type Parser struct {
   lexer *Lexer
   precedence map[byte] int
}

func (p *Parser) Init(data string) *Parser {
   p.lexer = new(Lexer).Init(data)
   p.precedence = make(map[byte] int)
   p.lexer.Next()
   return p
}

func (p *Parser) AddOperator(op byte, precedence int) {
   p.precedence[op] = precedence
}

func (p *Parser) Parse() (Node,bool) {
   lhs, ok := p.parsePrimary()
   if !ok { return nil, false }
   // starting with 1 instead of 0, because
   // map[*]int returns 0 for non-existant items
   node, ok := p.parseOperators(lhs, 1)
   if !ok { return nil, false }
   return node, true
}

func (p *Parser) parsePrimary() (Node,bool) {
   switch p.lexer.Kind {
      case NUM:
         node := new(Leaf).Init(p.lexer.Num)
         p.lexer.Next()
         return node, true
      case LPAR:
         p.lexer.Next()
         node, ok := p.Parse()
         if (!ok) { return nil, false }
         if p.lexer.Kind == RPAR { p.lexer.Next() }
         return node, true
   }
   return nil, false
}

func (p *Parser) parseOperators(lhs Node, min_precedence int) (Node,bool) {
   var ok bool
   var rhs Node
   for p.lexer.Kind == OP && p.precedence[p.lexer.Oper] >= min_precedence {
      op := p.lexer.Oper
      p.lexer.Next()
      rhs, ok = p.parsePrimary()
      if (!ok) { return nil, false }
      for p.lexer.Kind == OP && p.precedence[p.lexer.Oper] > p.precedence[op] {
         op2 := p.lexer.Oper
         rhs, ok = p.parseOperators(rhs, p.precedence[op2])
         if (!ok) { return nil, false }
      }
      lhs = new(Binary).Init(op,lhs,rhs)
   }
   return lhs, true
}

/* 
### = Test =
 */

func main() {
   var node Node
   var result Number
   var p *Parser
   var parseOk, evalOk bool
   in := bufio.NewReader(os.Stdin)
   line, ioErr := in.ReadString('\n')
   for len(line) > 0 {
      line = strings.TrimSpace(line)
      fmt.Printf("Read: %q\n", line)  // %q = quoted string
      p = new(Parser).Init(line)
      p.AddOperator('+',1)
      p.AddOperator('-',1)
      p.AddOperator('*',2)
      p.AddOperator('/',2)
      node, parseOk = p.Parse()
      if parseOk {
         fmt.Printf("Parsed: %s\n", node)
         result, evalOk = node.Eval()
         if evalOk {
            fmt.Printf("Evaluated: %v\n", result)  // %v = default format
         } else {
            fmt.Printf("%s = Evaluation error\n", line)
         }
      } else {
         fmt.Printf("%s = Syntax error\n", line)
      }
      if ioErr != nil { return }
      line, ioErr = in.ReadString('\n')
   }
}

```


Example


```txt

1+2*3
Read: "1+2*3"
Parsed: (1 + (2 * 3))
Evaluated: 7
(1+2)*(3+4)*(5+6)
Read: "(1+2)*(3+4)*(5+6)"
Parsed: (((1 + 2) * (3 + 4)) * (5 + 6))
Evaluated: 231

```


External links
* [http://en.wikipedia.org/wiki/Operator-precedence_parser#Pseudo-code Wikipedia: Operator-precedence parser]
=Library=
Shown here is use of the package go/parser in the standard library.  For the Go 1 release, there is a parser in the standard library, but not an evaluator.  Evaluation is relatively easy though, once you have a parse tree.

Go expressions can be more complex than what is required for the task.  These will parse but then are caught and disallowed in the evaluator.

```go
package main

import (
    "errors"
    "fmt"
    "go/ast"
    "go/parser"
    "go/token"
    "reflect"
    "strconv"
)

var tests = []string{
    "(1+3)*7", // 28, example from task description.
    "1+3*7",   // 22, shows operator precedence.
    "7",       // 7, a single literal is a valid expression.
    "7/3",     // eval only does integer math.
    "7.3",     // this parses, but we disallow it in eval.
    "7^3",     // parses, but disallowed in eval.
    "go",      // a valid keyword, not valid in an expression.
    "3@7",     // error message is "illegal character."
    "",        // EOF seems a reasonable error message.
}

func main() {
    for _, exp := range tests {
        if r, err := parseAndEval(exp); err == nil {
            fmt.Println(exp, "=", r)
        } else {
            fmt.Printf("%s: %v\n", exp, err)
        }
    }
}

func parseAndEval(exp string) (int, error) {
    tree, err := parser.ParseExpr(exp)
    if err != nil {
        return 0, err
    }
    return eval(tree)
}

func eval(tree ast.Expr) (int, error) {
    switch n := tree.(type) {
    case *ast.BasicLit:
        if n.Kind != token.INT {
            return unsup(n.Kind)
        }
        i, _ := strconv.Atoi(n.Value)
        return i, nil
    case *ast.BinaryExpr:
        switch n.Op {
        case token.ADD, token.SUB, token.MUL, token.QUO:
        default:
            return unsup(n.Op)
        }
        x, err := eval(n.X)
        if err != nil {
            return 0, err
        }
        y, err := eval(n.Y)
        if err != nil {
            return 0, err
        }
        switch n.Op {
        case token.ADD:
            return x + y, nil
        case token.SUB:
            return x - y, nil
        case token.MUL:
            return x * y, nil
        case token.QUO:
            return x / y, nil
        }
    case *ast.ParenExpr:
        return eval(n.X)
    }
    return unsup(reflect.TypeOf(tree))
}

func unsup(i interface{}) (int, error) {
    return 0, errors.New(fmt.Sprintf("%v unsupported", i))
}
```

Output:

```txt

(1+3)*7 = 28
1+3*7 = 22
7 = 7
7/3 = 2
7.3: FLOAT unsupported
7^3: ^ unsupported
go: 1:1: expected operand, found 'go'
3@7: 1:2: illegal character U+0040 '@'
: 1:1: expected operand, found 'EOF'

```

