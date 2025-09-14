+++
title = "Arithmetic evaluation"
description = ""
date = 2019-08-21T23:09:59Z
aliases = []
[extra]
id = 2414
[taxonomies]
categories = ["Recursion", "task"]
tags = []
languages = [
  "11l",
  "ada",
  "algol_68",
  "autohotkey",
  "bbc_basic",
  "c",
  "clojure",
  "common_lisp",
  "cpp",
  "d",
  "e",
  "elena",
  "emacs_lisp",
  "erre",
  "factor",
  "freebasic",
  "go",
  "groovy",
  "haskell",
  "j",
  "java",
  "javascript",
  "jq",
  "jsish",
  "julia",
  "kotlin",
  "liberty_basic",
  "lua",
  "m2000_interpreter",
  "nim",
  "ocaml",
  "oorexx",
  "oz",
  "pascal",
  "perl",
  "perl_6",
  "phix",
  "picolisp",
  "pop11",
  "prolog",
  "python",
  "racket",
  "rexx",
  "ruby",
  "rust",
  "scala",
  "scheme",
  "sidef",
  "standard_ml",
  "tcl",
  "txr",
  "ursala",
  "zkl",
  "zx_spectrum_basic",
]
+++

## Task
Create a program which parses and evaluates arithmetic expressions.

;Requirements:
* An [[wp:Abstract_syntax_tree|abstract-syntax tree]] (AST) for the expression must be created from parsing the input.
* The AST must be used in evaluation, also, so the input may not be directly evaluated (e.g. by calling eval or a similar language feature.)
* The expression will be a string or list of symbols like "(1+3)*7".
* The four symbols + - * / must be supported as binary operators with conventional precedence rules.
* Precedence-control parentheses must also be supported.



;Note:
For those who don't remember, mathematical precedence is as follows:
* Parentheses
* Multiplication/Division (left to right)
* Addition/Subtraction (left to right)



;C.f:
* [[24 game Player]].
* [[Parsing/RPN calculator algorithm]].
* [[Parsing/RPN to infix conversion]].





## 11l

[[wp:Pratt parser|Pratt parser]]

```11l
T Symbol
   String id
   Int lbp
   Int nud_bp
   Int led_bp
   (ASTNode -> ASTNode) nud
   (ASTNode, ASTNode -> ASTNode) led

   F set_nud_bp(nud_bp, nud)
      .nud_bp = nud_bp
      .nud = nud

   F set_led_bp(led_bp, led)
      .led_bp = led_bp
      .led = led

T ASTNode
   Symbol& symbol
   Int value
   ASTNode? first_child
   ASTNode? second_child

   F eval()
      S .symbol.id
         ‘(number)’
            R .value
         ‘+’
            R .first_child.eval() + .second_child.eval()
         ‘-’
            R I .second_child == N {-.first_child.eval()} E .first_child.eval() - .second_child.eval()
         ‘*’
            R .first_child.eval() * .second_child.eval()
         ‘/’
            R .first_child.eval() / .second_child.eval()
         ‘(’
            R .first_child.eval()
         E
            assert(0B)
            R 0

Dict[String, Symbol] symbol_table
Array[String] tokens
V tokeni = -1
ASTNode token_node

F advance(sid = ‘’)
   I sid != ‘’
      assert(:token_node.symbol.id == sid)
   :tokeni++
   :token_node = ASTNode()
   I :tokeni == tokens.len
      :token_node.symbol = :symbol_table[‘(end)’]
      R
   V token = :tokens[:tokeni]
   :token_node.symbol = :symbol_table[I token.is_digit() {‘(number)’} E token]
   I token.is_digit()
      :token_node.value = Int(token)

F expression(rbp = 0)
   ASTNode t = :token_node
   advance()
   V left = t.symbol.nud(t)
   L rbp < :token_node.symbol.lbp
      t = :token_node
      advance()
      left = t.symbol.led(t, left)
   R left

F parse(expr_str) -> ASTNode
   :tokens = re:‘\s*(\d+|.)’.find_strings(expr_str)
   :tokeni = -1
   advance()
   R expression()

F symbol(id, bp = 0) -> &
   I !(id C :symbol_table)
      V s = Symbol()
      s.id = id
      s.lbp = bp
      :symbol_table[id] = s
   R :symbol_table[id]

F infix(id, bp)
   F led(ASTNode self, left)
      self.first_child = left
      self.second_child = expression(self.symbol.led_bp)
      R self
   symbol(id, bp).set_led_bp(bp, led)

F prefix(id, bp)
   F nud(ASTNode self)
      self.first_child = expression(self.symbol.nud_bp)
      R self
   symbol(id).set_nud_bp(bp, nud)

infix(‘+’, 1)
infix(‘-’, 1)
infix(‘*’, 2)
infix(‘/’, 2)
prefix(‘-’, 3)

symbol(‘(number)’).nud = self -> self
symbol(‘(end)’)

F nud_parens(ASTNode self)
   V expr = expression()
   advance(‘)’)
   R expr
symbol(‘(’).nud = nud_parens
symbol(‘)’)

L(expr_str) [‘-2 / 2 + 4 + 3 * 2’,
             ‘2 * (3 + (4 * 5 + (6 * 7) * 8) - 9) * 10’]
   print(expr_str‘ = ’parse(expr_str).eval())
```

{{out}}

```txt

-2 / 2 + 4 + 3 * 2 = 9
2 * (3 + (4 * 5 + (6 * 7) * 8) - 9) * 10 = 7000

```



## Ada

See [[Arithmetic Evaluator/Ada]].


## ALGOL 68

{{works with|ALGOL 68|Revision 1 - no extensions to language used}}
{{works with|ALGOL 68G|Any - tested with release [http://sourceforge.net/projects/algol68/files/algol68g/algol68g-1.18.0/algol68g-1.18.0-9h.tiny.el5.centos.fc11.i386.rpm/download 1.18.0-9h.tiny]}}
{{wont work with|ELLA ALGOL 68|Any (with appropriate job cards) - tested with release [http://sourceforge.net/projects/algol68/files/algol68toc/algol68toc-1.8.8d/algol68toc-1.8-8d.fc9.i386.rpm/download 1.8-8d] - A68RS has not implemented forward declarations}}

```algol68
INT base=10;
MODE FIXED = LONG REAL; # numbers in the format 9,999.999 #

#IF build abstract syntax tree and then EVAL tree #
MODE AST = UNION(NODE, FIXED);
MODE NUM = REF AST;
MODE NODE = STRUCT(NUM a, PROC (FIXED,FIXED)FIXED op, NUM b);

OP EVAL = (NUM ast)FIXED:(
  CASE ast IN
    (FIXED num): num,
    (NODE fork): (op OF fork)(EVAL( a OF fork), EVAL (b OF fork))
  ESAC
);

OP + = (NUM a,b)NUM: ( HEAP AST := NODE(a, (FIXED a,b)FIXED:a+b, b) );
OP - = (NUM a,b)NUM: ( HEAP AST := NODE(a, (FIXED a,b)FIXED:a-b, b) );
OP * = (NUM a,b)NUM: ( HEAP AST := NODE(a, (FIXED a,b)FIXED:a*b, b) );
OP / = (NUM a,b)NUM: ( HEAP AST := NODE(a, (FIXED a,b)FIXED:a/b, b) );
OP **= (NUM a,b)NUM: ( HEAP AST := NODE(a, (FIXED a,b)FIXED:a**b, b) );

#ELSE simply use REAL arithmetic with no abstract syntax tree at all # CO
MODE NUM = FIXED, AST = FIXED;
OP EVAL = (FIXED num)FIXED: num;
#FI# END CO

MODE LEX = PROC (TOK)NUM;
MODE MONADIC =PROC (NUM)NUM;
MODE DIADIC = PROC (NUM,NUM)NUM;

MODE TOK = CHAR;
MODE ACTION = UNION(STACKACTION, LEX, MONADIC, DIADIC);
MODE OPVAL = STRUCT(INT prio, ACTION action);
MODE OPITEM = STRUCT(TOK token, OPVAL opval);

[256]STACKITEM stack;
MODE STACKITEM = STRUCT(NUM value, OPVAL op);
MODE STACKACTION = PROC (REF STACKITEM)VOID;

PROC begin = (REF STACKITEM top)VOID: prio OF op OF top -:= +10;
PROC end = (REF STACKITEM top)VOID: prio OF op OF top -:= -10;

OP ** = (COMPL a,b)COMPL: complex exp(complex ln(a)*b);

[8]OPITEM op list :=(
#  OP  PRIO ACTION #
  ("^", (8, (NUM a,b)NUM: a**b)),
  ("*", (7, (NUM a,b)NUM: a*b)),
  ("/", (7, (NUM a,b)NUM: a/b)),
  ("+", (6, (NUM a,b)NUM: a+b)),
  ("-", (6, (NUM a,b)NUM: a-b)),
  ("(",(+10, begin)),
  (")",(-10, end)),
  ("?", (9, LEX:SKIP))
);

PROC op dict = (TOK op)REF OPVAL:(
# This can be unrolled to increase performance #
  REF OPITEM candidate;
  FOR i TO UPB op list WHILE
    candidate := op list[i];
# WHILE # op /= token OF candidate DO
    SKIP
  OD;
  opval OF candidate
);

PROC build ast = (STRING expr)NUM:(

  INT top:=0;

  PROC compress ast stack = (INT prio, NUM in value)NUM:(
    NUM out value := in value;
    FOR loc FROM top BY -1 TO 1 WHILE
      REF STACKITEM stack top := stack[loc];
  # WHILE # ( top >= LWB stack | prio <= prio OF op OF stack top | FALSE ) DO
      top := loc - 1;
      out value :=
        CASE action OF op OF stack top IN
          (MONADIC op): op(value OF stack top), # not implemented #
          (DIADIC op): op(value OF stack top,out value)
        ESAC
    OD;
    out value
  );

  NUM value := NIL;
  FIXED num value;
  INT decimal places;

  FOR i TO UPB expr DO
    TOK token = expr[i];
    REF OPVAL this op := op dict(token);
    CASE action OF this op IN
      (STACKACTION action):(
        IF prio OF thisop = -10 THEN
          value := compress ast stack(0, value)
        FI;
        IF top >= LWB stack THEN
          action(stack[top])
        FI
      ),
      (LEX):( # a crude lexer #
        SHORT INT digit = ABS token - ABS "0";
        IF 0<= digit AND digit < base THEN
          IF NUM(value) IS NIL THEN # first digit #
            decimal places := 0;
            value := HEAP AST := num value := digit
          ELSE
            NUM(value) := num value := IF decimal places = 0
              THEN
                num value * base + digit
              ELSE
                decimal places *:= base;
                num value + digit / decimal places
              FI
          FI
        ELIF token = "." THEN
          decimal places := 1
        ELSE
          SKIP # and ignore spaces and any unrecognised characters #
        FI
      ),
      (MONADIC): SKIP, # not implemented #
      (DIADIC):(
        value := compress ast stack(prio OF this op, value);
        IF top=UPB stack THEN index error FI;
        stack[top+:=1]:=STACKITEM(value, this op);
        value:=NIL
      )
    ESAC
  OD;
  compress ast stack(-max int, value)
);

test:(
   printf(($" euler's number is about: "g(-long real width,long real width-2)l$,
     EVAL build ast("1+1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+1/15)/14)/13)/12)/11)/10)/9)/8)/7)/6)/5)/4)/3)/2")));
  SKIP EXIT
  index error:
    printf(("Stack over flow"))
)
```

{{out}}

```txt

 euler's number is about: 2.71828182845899446428546958

```



## AutoHotkey

{{works with|AutoHotkey_L}}

```AutoHotkey
/*
hand coded recursive descent parser
expr	: term ( ( PLUS | MINUS )  term )* ;
term	: factor ( ( MULT | DIV ) factor )* ;
factor	: NUMBER | '(' expr ')';
*/

calcLexer := makeCalcLexer()
string := "((3+4)*(7*9)+3)+4"
tokens := tokenize(string, calcLexer)
msgbox % printTokens(tokens)
ast := expr()
msgbox % printTree(ast)
msgbox % expression := evalTree(ast)
filedelete expression.ahk
fileappend, % "msgbox % " expression, expression.ahk
run, expression.ahk
return


expr()
{
  global tokens
  ast := object(1, "expr")
  if node := term()
    ast._Insert(node)
  loop
  {
    if peek("PLUS") or peek("MINUS")
    {
      op := getsym()
      newop := object(1, op.type, 2, op.value)
      node := term()
      ast._Insert(newop)
      ast._Insert(node)
    }
    Else
      Break
  }
  return ast
}

term()
{
  global tokens
  tree := object(1, "term")
  if node := factor()
    tree._Insert(node)
  loop
  {
    if  peek("MULT") or peek("DIV")
    {
      op := getsym()
      newop := object(1, op.type, 2, op.value)
      node := factor()
      tree._Insert(newop)
      tree._Insert(node)
    }
    else
      Break
  }
  return tree
}

factor()
{
  global tokens
  if peek("NUMBER")
  {
    token := getsym()
    tree := object(1, token.type, 2, token.value)
    return tree
  }
  else if  peek("OPEN")
  {
    getsym()
    tree := expr()
    if  peek("CLOSE")
    {
      getsym()
      return tree
    }
    else
      error("miss closing parentheses ")
  }
  else
    error("no factor found")
}

peek(type, n=1)
{
global tokens
  if (tokens[n, "type"] == type)
  return 1
}

getsym(n=1)
{
global tokens
return token := tokens._Remove(n)
}

error(msg)
{
global tokens
msgbox % msg " at:`n" printToken(tokens[1])
}


printTree(ast)
{
if !ast
return

n := 0
  loop
  {
  n += 1
    if !node := ast[n]
      break
    if !isobject(node)
      treeString .= node
    else
      treeString .= printTree(node)
  }
  return ("(" treeString ")" )
}

evalTree(ast)
{
if !ast
return

n := 1
  loop
  {
  n += 1
    if !node := ast[n]
      break
    if !isobject(node)
      treeString .= node
    else
      treeString .= evalTree(node)
  }
if (n == 3)
return treeString
  return ("(" treeString ")" )
}

#include calclex.ahk
```

calclex.ahk
```AutoHotkey
tokenize(string, lexer)
{
  stringo := string  ; store original string
  locationInString := 1
  size := strlen(string)
  tokens := object()

start:
  Enum := Lexer._NewEnum()
  While Enum[type, value]  ; loop through regular expression lexing rules
  {
    if (1 == regexmatch(string, value, tokenValue))
    {
      token := object()
      token.pos := locationInString
      token.value := tokenValue
      token.length := strlen(tokenValue)
      token.type := type
      tokens._Insert(token)
      locationInString += token.length
      string := substr(string, token.length + 1)
      goto start
    }
    continue
  }
  if (locationInString < size)
    msgbox % "unrecognized token at " substr(stringo, locationInstring)
  return tokens
}

makeCalcLexer()
{
  calcLexer := object()
  PLUS := "\+"
  MINUS := "-"
  MULT := "\*"
  DIV := "/"
  OPEN := "\("
  CLOSE := "\)"
  NUMBER := "\d+"
  WS := "[ \t\n]+"
  END := "\."
  RULES := "PLUS,MINUS,MULT,DIV,OPEN,CLOSE,NUMBER,WS,END"
  loop, parse, rules, `,
  {
    type := A_LoopField
    value := %A_LoopField%
    calcLexer._Insert(type, value)
  }
  return calcLexer
}

printTokens(tokens)
{
  loop % tokens._MaxIndex()
  {
    tokenString .= printToken(tokens[A_Index]) "`n`n"
  }
  return tokenString
}


printToken(token)
{
  string := "pos= " token.pos "`nvalue= " token.value "`ntype= " token.type
  return string
}
```



## BBC BASIC

{{works with|BBC BASIC for Windows}}

```bbcbasic
      Expr$ = "1 + 2 * (3 + (4 * 5 + 6 * 7 * 8) - 9) / 10"
      PRINT "Input = " Expr$
      AST$ = FNast(Expr$)
      PRINT "AST =   " AST$
      PRINT "Value = " ;EVAL(AST$)
      END

      DEF FNast(RETURN in$)
      LOCAL ast$, oper$
      REPEAT
        ast$ += FNast1(in$)
        WHILE ASC(in$)=32 in$ = MID$(in$,2) : ENDWHILE
        oper$ = LEFT$(in$,1)
        IF oper$="+" OR oper$="-" THEN
          ast$ += oper$
          in$ = MID$(in$,2)
        ELSE
          EXIT REPEAT
        ENDIF
      UNTIL FALSE
      = "(" + ast$ + ")"

      DEF FNast1(RETURN in$)
      LOCAL ast$, oper$
      REPEAT
        ast$ += FNast2(in$)
        WHILE ASC(in$)=32 in$ = MID$(in$,2) : ENDWHILE
        oper$ = LEFT$(in$,1)
        IF oper$="*" OR oper$="/" THEN
          ast$ += oper$
          in$ = MID$(in$,2)
        ELSE
          EXIT REPEAT
        ENDIF
      UNTIL FALSE
      = "(" + ast$ + ")"

      DEF FNast2(RETURN in$)
      LOCAL ast$
      WHILE ASC(in$)=32 in$ = MID$(in$,2) : ENDWHILE
      IF ASC(in$)<>40 THEN = FNnumber(in$)
      in$ = MID$(in$,2)
      ast$ = FNast(in$)
      in$ = MID$(in$,2)
      = ast$

      DEF FNnumber(RETURN in$)
      LOCAL ch$, num$
      REPEAT
        ch$ = LEFT$(in$,1)
        IF INSTR("0123456789.", ch$) THEN
          num$ += ch$
          in$ = MID$(in$,2)
        ELSE
          EXIT REPEAT
        ENDIF
      UNTIL FALSE
      = num$
```

{{out}}

```txt

Input = 1 + 2 * (3 + (4 * 5 + 6 * 7 * 8) - 9) / 10
AST =   ((1)+(2*((3)+(((4*5)+(6*7*8)))-(9))/10))
Value = 71

```



## C

See [[Arithmetic Evaluator/C]].


## C++

{{Works with|g++|4.1.2 20061115 (prerelease) (SUSE Linux)}}

{{libheader|Boost.Spirit|1.8.4}}

```cpp
#include <boost/spirit.hpp>
#include <boost/spirit/tree/ast.hpp>
#include <string>
#include <cassert>
#include <iostream>
#include <istream>
#include <ostream>

using boost::spirit::rule;
using boost::spirit::parser_tag;
using boost::spirit::ch_p;
using boost::spirit::real_p;

using boost::spirit::tree_node;
using boost::spirit::node_val_data;

// The grammar
struct parser: public boost::spirit::grammar<parser>
{
   enum rule_ids { addsub_id, multdiv_id, value_id, real_id };

   struct set_value
   {
     set_value(parser const& p): self(p) {}
     void operator()(tree_node<node_val_data<std::string::iterator,
                                             double> >& node,
                     std::string::iterator begin,
                     std::string::iterator end) const
     {
       node.value.value(self.tmp);
     }
     parser const& self;
   };

   mutable double tmp;

   template<typename Scanner> struct definition
   {
     rule<Scanner, parser_tag<addsub_id> > addsub;
     rule<Scanner, parser_tag<multdiv_id> > multdiv;
     rule<Scanner, parser_tag<value_id> > value;
     rule<Scanner, parser_tag<real_id> > real;

     definition(parser const& self)
     {
       using namespace boost::spirit;
       addsub = multdiv
         >> *((root_node_d[ch_p('+')] | root_node_d[ch_p('-')]) >> multdiv);
       multdiv = value
         >> *((root_node_d[ch_p('*')] | root_node_d[ch_p('/')]) >> value);
       value = real | inner_node_d[('(' >> addsub >> ')')];
       real = leaf_node_d[access_node_d[real_p[assign_a(self.tmp)]][set_value(self)]];
     }

     rule<Scanner, parser_tag<addsub_id> > const& start() const
     {
       return addsub;
     }
   };
 };

 template<typename TreeIter>
 double evaluate(TreeIter const& i)
 {
   double op1, op2;
   switch (i->value.id().to_long())
   {
   case parser::real_id:
     return i->value.value();
   case parser::value_id:
   case parser::addsub_id:
   case parser::multdiv_id:
     op1 = evaluate(i->children.begin());
     op2 = evaluate(i->children.begin()+1);
     switch(*i->value.begin())
     {
     case '+':
       return op1 + op2;
     case '-':
       return op1 - op2;
     case '*':
       return op1 * op2;
     case '/':
       return op1 / op2;
     default:
       assert(!"Should not happen");
     }
   default:
     assert(!"Should not happen");
   }
   return 0;
 }

 // the read/eval/write loop
 int main()
 {
   parser eval;
   std::string line;
   while (std::cout << "Expression: "
          && std::getline(std::cin, line)
          && !line.empty())
   {
     typedef boost::spirit::node_val_data_factory<double> factory_t;
     boost::spirit::tree_parse_info<std::string::iterator, factory_t> info =
       boost::spirit::ast_parse<factory_t>(line.begin(), line.end(),
                                           eval, boost::spirit::space_p);
     if (info.full)
     {
       std::cout << "Result: " << evaluate(info.trees.begin()) << std::endl;
     }
     else
     {
       std::cout << "Error in expression." << std::endl;
     }
   }
 };
```



## Clojure


```Clojure
(def precedence '{* 0, / 0
		  + 1, - 1})

(defn order-ops
  "((A x B) y C) or (A x (B y C)) depending on precedence of x and y"
  [[A x B y C & more]]
  (let [ret (if (<=  (precedence x)
		     (precedence y))
	      (list (list A x B) y C)
	      (list A x (list B y C)))]
    (if more
      (recur (concat ret more))
      ret)))

(defn add-parens
  "Tree walk to add parens.  All lists are length 3 afterwards."
  [s]
  (clojure.walk/postwalk
   #(if (seq? %)
      (let [c (count %)]
	(cond (even? c) (throw (Exception. "Must be an odd number of forms"))
	      (= c 1) (first %)
	      (= c 3) %
	      (>= c 5) (order-ops %)))
      %)
   s))

(defn make-ast
  "Parse a string into a list of numbers, ops, and lists"
  [s]
  (-> (format "'(%s)" s)
      (.replaceAll , "([*+-/])" " $1 ")
      load-string
      add-parens))

(def ops {'* *
	  '+ +
	  '- -
	  '/ /})

(def eval-ast
     (partial clojure.walk/postwalk
	      #(if (seq? %)
		 (let [[a o b] %]
		   ((ops o) a b))
		 %)))

(defn evaluate [s]
  "Parse and evaluate an infix arithmetic expression"
  (eval-ast (make-ast s)))

user> (evaluate "1 + 2*(3 - 2*(3 - 2)*((2 - 4)*5 - 22/(7 + 2*(3 - 1)) - 1)) + 1")
60
```



## Common Lisp


The following code processes the data in a pipeline of steps which are combined in the <code>evaluate</code> function.

First, the string is converted into a sequence of tokens, represented as a list.  Operator tokens are represented directly by the corresponding Lisp symbols, and the integer terms are represented by Lisp integer objects. The symbols <code>:lparen</code> and <code>:rparen</code> represent the the parentheses. So for instance the input
<code>"1*(3+2)"</code> tokenizes as <code>(1 * :lparen 3 + 2 :rparen)</code>.

Next, that sequence of tokens is then transformed by eliminating the parentheses. Subsequences of the form <code>:lparen ... :rparen</code> with a sublist containing the tokens between the <code>:lparen</code> and <code>:rparen</code>. The sequence now has an intermediate tree structure, in which parenthesized fragments like <code>1 + 2 * 3 + 4 / 9</code> still remain flat.

At this point, another processing stage parses the operator precedence, and fully parenthesizes fragments, turning <code>(1 + 2 / 3 + 5)</code> into <code>(1 + (2 / 3) + 5)</code>. The result is a Lisp-ified infix representation.

Finally, this infix representation can be easily converted to prefix, forming the final AST which is a Lisp expression.
(Lisp expressions '''are''' abstract syntax trees!) This representation evaluates directly with <code>eval</code>.

This implementation can read integers, and produce integral and rational values.


```lisp
(defun tokenize-stream (stream)
  (labels ((whitespace-p (char)
             (find char #(#\space #\newline #\return #\tab)))
           (consume-whitespace ()
             (loop while (whitespace-p (peek-char nil stream nil #\a))
                   do (read-char stream)))
           (read-integer ()
             (loop while (digit-char-p (peek-char nil stream nil #\space))
                   collect (read-char stream) into digits
                   finally (return (parse-integer (coerce digits 'string))))))
    (consume-whitespace)
    (let* ((c (peek-char nil stream nil nil)))
           (token (case c
                     (nil nil)
                     (#\( :lparen)
                     (#\) :rparen)
                     (#\* '*)
                     (#\/ '/)
                     (#\+ '+)
                     (#\- '-)
                     (otherwise
                       (unless (digit-char-p c)
                         (cerror "Skip it." "Unexpected character ~w." c)
                         (read-char stream)
                         (return-from tokenize-stream
                                      (tokenize-stream stream)))
                       (read-integer)))))
        (unless (or (null token) (integerp token))
          (read-char stream))
        token)))

(defun group-parentheses (tokens &optional (delimited nil))
  (do ((new-tokens '()))
      ((endp tokens)
       (when delimited
         (cerror "Insert it."  "Expected right parenthesis."))
       (values (nreverse new-tokens) '()))
    (let ((token (pop tokens)))
      (case token
        (:lparen
         (multiple-value-bind (group remaining-tokens)
             (group-parentheses tokens t)
           (setf new-tokens (cons group new-tokens)
                 tokens remaining-tokens)))
        (:rparen
         (if (not delimited)
           (cerror "Ignore it." "Unexpected right parenthesis.")
           (return (values (nreverse new-tokens) tokens))))
        (otherwise
         (push token new-tokens))))))

(defun group-operations (expression)
  (flet ((gop (exp) (group-operations exp)))
    (if (integerp expression)
      expression
      (destructuring-bind (a &optional op1 b op2 c &rest others)
                          expression
        (unless (member op1 '(+ - * / nil))
          (error "syntax error: in expr ~a expecting operator, not ~a"
                 expression op1))
        (unless (member op2 '(+ - * / nil))
          (error "syntax error: in expr ~a expecting operator, not ~a"
                 expression op2))
        (cond
         ((not op1) (gop a))
         ((not op2) `(,(gop a) ,op1 ,(gop b)))
         (t (let ((a (gop a)) (b (gop b)) (c (gop c)))
              (if (and (member op1 '(+ -)) (member op2 '(* /)))
                (gop `(,a ,op1 (,b ,op2 ,c) ,@others))
                (gop `((,a ,op1 ,b) ,op2 ,c ,@others))))))))))

(defun infix-to-prefix (expression)
  (if (integerp expression)
    expression
    (destructuring-bind (a op b) expression
      `(,op ,(infix-to-prefix a) ,(infix-to-prefix b)))))

(defun evaluate (string)
  (with-input-from-string (in string)
    (eval
      (infix-to-prefix
        (group-operations
          (group-parentheses
            (loop for token = (tokenize-stream in)
                  until (null token)
                  collect token)))))))
```


Examples

 > (evaluate "1 - 5 * 2 / 20 + 1")
 3/2

 > (evaluate "(1 - 5) * 2 / (20 + 1)")
 -8/21

 > (evaluate "2 * (3 + ((5) / (7 - 11)))")
 7/2

 > (evaluate "(2 + 3) / (10 - 5)")
 1

Examples of error handling


```txt
> (evaluate "(3 * 2) a - (1 + 2) / 4")

 Error: Unexpected character a.
  1 (continue) Skip it.
  2 (abort) Return to level 0.
  3 Return to top loop level 0.

Type :b for backtrace, :c <option number> to proceed,  or :? for other options

 : 1 > :c 1
21/4
```



```txt
> (evaluate "(3 * 2) - (1 + 2) / (4")

Error: Expected right parenthesis.
  1 (continue) Insert it.
  2 (abort) Return to level 0.
  3 Return to top loop level 0.

Type :b for backtrace, :c <option number> to proceed,  or :? for other options

: 1 > :c 1
21/4
```



## D

After the AST tree is constructed, a visitor pattern is used to display the AST structure and calculate the expression value.

```d
import std.stdio, std.string, std.ascii, std.conv, std.array,
       std.exception, std.traits;

struct Stack(T) {
    T[] data;
    alias data this;
    void push(T top) pure nothrow @safe { data ~= top; }

    T pop(bool discard = true)() pure @nogc @safe {
      immutable static exc = new immutable(Exception)("Stack Empty");
      if (data.empty)
        throw exc;
      auto top = data[$ - 1];
      static if (discard)
        data.popBack;
      return top;
    }
}

enum Type {         Num, OBkt, CBkt, Add, Sub, Mul, Div }
immutable opChar = ["#", "(",  ")",  "+", "-", "*", "/"];
immutable opPrec = [ 0,  -9,   -9,    1,   1,   2,   2];

abstract class Visitor { void visit(XP e) pure @safe; }

final class XP {
  immutable Type type;
  immutable string str;
  immutable int pos; // Optional, to dispaly AST struct.
  XP LHS, RHS;

  this(string s=")", int p = -1) pure nothrow @safe {
    str = s;
    pos = p;
    auto localType = Type.Num;
    foreach_reverse (immutable t; [EnumMembers!Type[1 .. $]])
      if (opChar[t] == s)
        localType = t;
    this.type = localType;
  }

  override int opCmp(Object other) pure @safe {
    auto rhs = cast(XP)other;
    enforce(rhs !is null);
    return opPrec[type] - opPrec[rhs.type];
  }

  void accept(Visitor v) pure @safe { v.visit(this); }
}

final class AST {
  XP root;
  Stack!XP opr, num;
  string xpr, token;
  int xpHead, xpTail;

  void joinXP(XP x) pure @safe {
    x.RHS = num.pop;
    x.LHS = num.pop;
    num.push(x);
  }

  string nextToken() pure @safe {
    while (xpHead < xpr.length && xpr[xpHead] == ' ')
      xpHead++; // Skip spc.
    xpTail = xpHead;
    if (xpHead < xpr.length) {
      token = xpr[xpTail .. xpTail + 1];
      switch (token) {
        case "(", ")", "+", "-", "*", "/": // Valid non-number.
          xpTail++;
          return token;
        default: // Should be number.
          if (token[0].isDigit) {
            while (xpTail < xpr.length && xpr[xpTail].isDigit())
              xpTail++;
            return xpr[xpHead .. xpTail];
          } // Else may be error.
      } // End switch.
    }
    if (xpTail < xpr.length)
      throw new Exception("Invalid Char <" ~ xpr[xpTail] ~ ">");
    return null;
  } // End nextToken.

  AST parse(in string s) /*@safe*/ {
    bool expectingOP;
    xpr = s;
    try {
      xpHead = xpTail = 0;
      num = opr = null;
      root = null;
      opr.push(new XP); // CBkt, prevent evaluate null OP precedence.
      while ((token = nextToken) !is null) {
        XP tokenXP = new XP(token, xpHead);
        if (expectingOP) { // Process OP-alike XP.
          switch (token) {
            case ")":
              while (opr.pop!false.type != Type.OBkt)
                joinXP(opr.pop);
              opr.pop;
              expectingOP = true;
              break;
            case "+", "-", "*", "/":
              while (tokenXP <= opr.pop!false)
                joinXP(opr.pop());
              opr.push(tokenXP);
              expectingOP = false;
              break;
            default:
              throw new Exception("Expecting Operator or ), not <"
                                  ~ token ~ ">");
          }
        } else { // Process Num-alike XP.
          switch (token) {
            case "+", "-", "*", "/", ")":
              throw new Exception("Expecting Number or (, not <"
                                  ~ token ~ ">");
            case "(":
              opr.push(tokenXP);
              expectingOP = false;
              break;
            default: // Number.
              num.push(tokenXP);
              expectingOP = true;
          }
        }
        xpHead = xpTail;
      } // End while.

      while (opr.length > 1) // Join pending Op.
        joinXP(opr.pop);
    } catch(Exception e) {
      writefln("%s\n%s\n%s^", e.msg, xpr, " ".replicate(xpHead));
      root = null;
      return this;
    }

    if (num.length != 1) { // Should be one XP left.
      "Parse Error...".writefln;
      root = null;
    } else {
      root = num.pop;
    }
    return this;
  } // End Parse.
}  // End class AST.

// To display AST fancy struct.
void ins(ref char[][] s, in string v, in int p, in int l)
pure nothrow @safe {
  if (l + 1 > s.length)
    s.length++;
  while (s[l].length < p + v.length + 1)
    s[l] ~= " ";
  s[l][p .. p + v.length] = v[];
}

final class CalcVis : Visitor {
  int result, level;
  string resultStr;
  char[][] Tree;

  static void opCall(AST a) @safe {
    if (a && a.root) {
      auto c = new CalcVis;
      a.root.accept(c);
      foreach (immutable i; 1 .. c.Tree.length) { // More fancy.
        bool flipflop = false;
        enum char mk = '.';
        foreach (immutable j; 0 .. c.Tree[i].length) {
          while (j >= c.Tree[i - 1].length)
            c.Tree[i - 1] ~= " ";
          immutable c1 = c.Tree[i][j];
          immutable c2 = c.Tree[i - 1][j];
          if (flipflop && (c1 == ' ') && c2 == ' ')
            c.Tree[i - 1][j] = mk;
          if (c1 != mk && c1 != ' ' &&
              (j == 0 || !isDigit(c.Tree[i][j - 1])))
            flipflop = !flipflop;
        }
      }
      foreach (const t; c.Tree)
        t.writefln;
      writefln("\n%s ==>\n%s = %s", a.xpr, c.resultStr, c.result);
    } else
      "Evalute invalid or null Expression.".writefln;
  }

  // Calc. the value, display AST struct and eval order.
  override void visit(XP xp) @safe {
    ins(Tree, xp.str, xp.pos, level);
    level++;
    if (xp.type == Type.Num) {
      resultStr ~= xp.str;
      result = xp.str.to!int;
    } else {
      resultStr ~= "(";
      xp.LHS.accept(this);
      immutable int lhs = result;
      resultStr ~= opChar[xp.type];
      xp.RHS.accept(this);
      resultStr ~= ")";
      switch (xp.type) {
        case Type.Add: result = lhs + result; break;
        case Type.Sub: result = lhs - result; break;
        case Type.Mul: result = lhs * result; break;
        case Type.Div: result = lhs / result; break;
        default: throw new Exception("Invalid type");
      }
    }
    level--;
  }
}

void main(string[] args) /*@safe*/ {
  immutable exp0 = "1 + 2*(3 - 2*(3 - 2)*((2 - 4)*5" ~
                   " - 22/(7 + 2*(3 - 1)) - 1)) + 1";
  immutable exp = (args.length > 1) ? args[1 .. $].join(' ') : exp0;
  new AST().parse(exp).CalcVis; // Should be 60.
}
```

{{out}}

```txt
   ........................................................+.
 .+..                                                        1
1    *...
    2   .-..........
       3     .......*................................
            *...                 ....................-.
           2   .-.            ..-...                   1
              3   2       ...*      /...
                        .-.   5   22   .+..
                       2   4          7    *...
                                          2   .-.
                                             3   1

1 + 2*(3 - 2*(3 - 2)*((2 - 4)*5 - 22/(7 + 2*(3 - 1)) - 1)) + 1 ==>
((1+(2*(3-((2*(3-2))*((((2-4)*5)-(22/(7+(2*(3-1)))))-1)))))+1) = 60
```



## E


While the task requirements specify not evaluating using the language's built-in eval, they don't say that you have to write your own ''parser''...


```e>def eParser := <elang:syntax.makeEParser

def LiteralExpr := <elang:evm.makeLiteralExpr>.asType()
def arithEvaluate(expr :String) {
  def ast := eParser(expr)

  def evalAST(ast) {
    return switch (ast) {
      match e`@a + @b` { evalAST(a) + evalAST(b) }
      match e`@a - @b` { evalAST(a) - evalAST(b) }
      match e`@a * @b` { evalAST(a) * evalAST(b) }
      match e`@a / @b` { evalAST(a) / evalAST(b) }
      match e`-@a` { -(evalAST(a)) }
      match l :LiteralExpr { l.getValue() }
    }
  }

  return evalAST(ast)
}
```


Parentheses are handled by the parser.


```e
? arithEvaluate("1 + 2")
# value: 3

? arithEvaluate("(1 + 2) * 10 / 100")
# value: 0.3

? arithEvaluate("(1 + 2 / 2) * (5 + 5)")
# value: 20.0
```



## Elena

ELENA 4.x :

```elena
import system'routines;
import extensions;
import extensions'text;

class Token
{
    object theValue;

    rprop int Level;

    constructor new(int level)
    {
        theValue := new StringWriter();
        Level := level + 9;
    }

    append(ch)
    {
        theValue.write(ch)
    }

    Number = theValue.toReal();
}

class Node
{
    prop object Left;
    prop object Right;
    rprop int Level;

    constructor new(int level)
    {
        Level := level
    }
}

class SummaryNode : Node
{
    constructor new(int level)
        <= new(level + 1);

    Number = Left.Number + Right.Number;
}

class DifferenceNode : Node
{
    constructor new(int level)
        <= new(level + 1);

    Number = Left.Number - Right.Number;
}

class ProductNode : Node
{
    constructor new(int level)
        <= new(level + 2);

    Number = Left.Number * Right.Number;
}

class FractionNode : Node
{
    constructor new(int level)
        <= new(level + 2);

    Number = Left.Number / Right.Number;
}

class Expression
{
    rprop int Level;
    prop object Top;

    constructor new(int level)
    {
        Level := level
    }

    prop object Right
    {
        get() = Top;

        set(object node)
        {
            Top := node
        }
    }

    get Number() => Top;
}

singleton operatorState
{
    eval(ch)
    {
        ch =>
            $40 {      // (
                ^ __target.newBracket().gotoStarting()
            }
            : {
                ^ __target.newToken().append(ch).gotoToken()
            }
    }
}

singleton tokenState
{
    eval(ch)
    {
        ch =>
            $41 {      // )
                ^ __target.closeBracket().gotoToken()
            }
            $42 {      // *
                ^ __target.newProduct().gotoOperator()
            }
            $43 {      // +
                ^ __target.newSummary().gotoOperator()
            }
            $45 {      // -
                ^ __target.newDifference().gotoOperator()
            }
            $47 {      // /
                ^ __target.newFraction().gotoOperator()
            }
            : {
                ^ __target.append:ch
            }
    }
}

singleton startState
{
    eval(ch)
    {
        ch =>
            $40 {      // (
                ^ __target.newBracket().gotoStarting()
            }
            $45 { // -
                ^ __target.newToken().append("0").newDifference().gotoOperator()
            }
            : {
                ^ __target.newToken().append:ch.gotoToken()
            }
    }
}

class Scope
{
    object theState;
    int    theLevel;
    object theParser;
    object theToken;
    object theExpression;

    constructor new(parser)
    {
        theState := startState;
        theLevel := 0;
        theExpression := Expression.new(0);
        theParser := parser
    }

    newToken()
    {
        theToken := theParser.appendToken(theExpression, theLevel)
    }

    newSummary()
    {
        theToken := nil;

        theParser.appendSummary(theExpression, theLevel)
    }

    newDifference()
    {
        theToken := nil;

        theParser.appendDifference(theExpression, theLevel)
    }

    newProduct()
    {
        theToken := nil;

        theParser.appendProduct(theExpression, theLevel)
    }

    newFraction()
    {
        theToken := nil;

        theParser.appendFraction(theExpression, theLevel)
    }

    newBracket()
    {
        theToken := nil;

        theLevel := theLevel + 10;

        theParser.appendSubexpression(theExpression, theLevel)
    }

    closeBracket()
    {
        if (theLevel < 10)
            { InvalidArgumentException.new:"Invalid expression".raise() };

        theLevel := theLevel - 10
    }

    append(ch)
    {
        if(ch >= $48 && ch < $58)
        {
            theToken.append:ch
        }
        else
        {
            InvalidArgumentException.new:"Invalid expression".raise()
        }
    }

    append(string s)
    {
        s.forEach:(ch){ self.append:ch }
    }

    gotoStarting()
    {
        theState := startState
    }

    gotoToken()
    {
        theState := tokenState
    }

    gotoOperator()
    {
        theState := operatorState
    }

    get Number() => theExpression;

    dispatch() => theState;
}

class Parser
{
    appendToken(object expression, int level)
    {
        var token := Token.new(level);

        expression.Top := self.append(expression.Top, token);

        ^ token
    }

    appendSummary(object expression, int level)
    {
        expression.Top := self.append(expression.Top, SummaryNode.new(level))
    }

    appendDifference(object expression, int level)
    {
        expression.Top := self.append(expression.Top, DifferenceNode.new(level))
    }

    appendProduct(object expression, int level)
    {
        expression.Top := self.append(expression.Top, ProductNode.new(level))
    }

    appendFraction(object expression, int level)
    {
        expression.Top := self.append(expression.Top, FractionNode.new(level))
    }

    appendSubexpression(object expression, int level)
    {
        expression.Top := self.append(expression.Top, Expression.new(level))
    }

    append(object lastNode, object newNode)
    {
        if(nil == lastNode)
            { ^ newNode };

        if (newNode.Level <= lastNode.Level)
            { newNode.Left := lastNode; ^ newNode };

        var parent := lastNode;
        var current := lastNode.Right;
        while (nil != current && newNode.Level > current.Level)
            { parent := current; current := current.Right };

        if (nil == current)
        {
            parent.Right := newNode
        }
        else
        {
            newNode.Left := current; parent.Right := newNode
        };

        ^ lastNode
    }

    run(text)
    {
        var scope := Scope.new(self);

        text.forEach:(ch){ scope.eval:ch };

        ^ scope.Number
    }
}

public program()
{
    var text := new StringWriter();
    var parser := new Parser();

    while (console.readLine().saveTo(text).Length > 0)
    {
        try
        {
            console.printLine("=",parser.run:text)
        }
        catch(Exception e)
        {
            console.writeLine(e.Printable)

            //console.writeLine:"Invalid Expression"
        };

        text.clear()
    }
}
```



## Emacs Lisp


```lisp
#!/usr/bin/env emacs --script
;; -*- mode: emacs-lisp; lexical-binding: t -*-
;;> ./arithmetic-evaluation '(1 + 2) * 3'

(defun advance ()
  (let ((rtn (buffer-substring-no-properties (point) (match-end 0))))
    (goto-char (match-end 0))
    rtn))

(defvar current-symbol nil)

(defun next-symbol ()
  (when (looking-at "[ \t\n]+")
    (goto-char (match-end 0)))

  (cond
   ((eobp)
    (setq current-symbol 'eof))
   ((looking-at "[0-9]+")
    (setq current-symbol (string-to-number (advance))))
   ((looking-at "[-+*/()]")
    (setq current-symbol (advance)))
   ((looking-at ".")
    (error "Unknown character '%s'" (advance)))))

(defun accept (sym)
  (when (equal sym current-symbol)
    (next-symbol)
    t))

(defun expect (sym)
  (unless (accept sym)
    (error "Expected symbol %s, but found %s" sym current-symbol))
  t)

(defun p-expression ()
  " expression = term  { ('+' | '-') term } . "
  (let ((rtn (p-term)))
    (while (or (equal current-symbol "+") (equal current-symbol "-"))
      (let ((op current-symbol)
            (left rtn))
        (next-symbol)
        (setq rtn (list op left (p-term)))))
    rtn))

(defun p-term ()
  " term = factor  { ('*' | '/') factor } . "
  (let ((rtn (p-factor)))
    (while (or (equal current-symbol "*") (equal current-symbol "/"))
      (let ((op current-symbol)
            (left rtn))
        (next-symbol)
        (setq rtn (list op left (p-factor)))))
    rtn))

(defun p-factor ()
  " factor = constant | variable | '('  expression  ')' . "
  (let (rtn)
    (cond
     ((numberp current-symbol)
      (setq rtn current-symbol)
      (next-symbol))
     ((accept "(")
      (setq rtn (p-expression))
      (expect ")"))
     (t (error "Syntax error")))
    rtn))

(defun ast-build (expression)
  (let (rtn)
    (with-temp-buffer
      (insert expression)
      (goto-char (point-min))
      (next-symbol)
      (setq rtn (p-expression))
      (expect 'eof))
    rtn))

(defun ast-eval (v)
  (pcase v
    ((pred numberp) v)
    (`("+" ,a ,b) (+ (ast-eval a) (ast-eval b)))
    (`("-" ,a ,b) (- (ast-eval a) (ast-eval b)))
    (`("*" ,a ,b) (* (ast-eval a) (ast-eval b)))
    (`("/" ,a ,b) (/ (ast-eval a) (float (ast-eval b))))
    (_ (error "Unknown value %s" v))))

(dolist (arg command-line-args-left)
  (let ((ast (ast-build arg)))
    (princ (format "       ast = %s\n" ast))
    (princ (format "     value = %s\n" (ast-eval ast)))
    (terpri)))
(setq command-line-args-left nil)

```


{{out}}

```txt

$ ./arithmetic-evaluation '(1 + 2) * 3'
       ast = (* (+ 1 2) 3)
     value = 9

$ ./arithmetic-evaluation '1 + 2 * (3 + (4 * 5 + 6 * 7 * 8) - 9) / 10'
       ast = (+ 1 (/ (* 2 (- (+ 3 (+ (* 4 5) (* (* 6 7) 8))) 9)) 10))
     value = 71.0

$ ./arithmetic-evaluation '1 + 2*(3 - 2*(3 - 2)*((2 - 4)*5 - 22/(7 + 2*(3 - 1)) - 1)) + 1'
       ast = (+ (+ 1 (* 2 (- 3 (* (* 2 (- 3 2)) (- (- (* (- 2 4) 5) (/ 22 (+ 7 (* 2 (- 3 1))))) 1))))) 1)
     value = 60.0

$ ./arithmetic-evaluation '(1 + 2) * 10 / 100'
       ast = (/ (* (+ 1 2) 10) 100)
     value = 0.3

```


## ERRE


```ERRE

PROGRAM EVAL

!
! arithmetic expression evaluator
!

!$KEY

LABEL 98,100,110

DIM STACK$[50]

PROCEDURE DISEGNA_STACK
  !$RCODE="LOCATE 3,1"
  !$RCODE="COLOR 0,7"
  PRINT(TAB(35);"S T A C K";TAB(79);)
  !$RCODE="COLOR 7,0"
  FOR TT=1 TO 38 DO
     IF TT>=20 THEN
        !$RCODE="LOCATE 3+TT-19,40"
      ELSE
        !$RCODE="LOCATE 3+TT,1"
     END IF
     IF TT=NS THEN PRINT(">";)  ELSE PRINT(" ";)  END IF
     PRINT(RIGHT$(STR$(TT),2);"³ ";STACK$[TT];"    ")
  END FOR
  REPEAT
   GET(Z$)
  UNTIL LEN(Z$)<>0
END PROCEDURE

PROCEDURE COMPATTA_STACK
   IF NS>1 THEN
      R=1
      WHILE R<NS DO
         IF INSTR(OP_LIST$,STACK$[R])=0 AND INSTR(OP_LIST$,STACK$[R+1])=0 THEN
            FOR R1=R TO NS-1 DO
                STACK$[R1]=STACK$[R1+1]
            END FOR
            NS=NS-1
         END IF
         R=R+1
      END WHILE
   END IF
   DISEGNA_STACK
END PROCEDURE

PROCEDURE CALC_ARITM
     L=NS1
     WHILE L<=NS2 DO
        IF STACK$[L]="^" THEN
            IF L>=NS2 THEN GOTO 100 END IF
            N1#=VAL(STACK$[L-1])  N2#=VAL(STACK$[L+1])  NOP=NOP-1
            IF STACK$[L]="^" THEN
                RI#=N1#^N2#
            END IF
            STACK$[L-1]=STR$(RI#)
            N=L
            WHILE N<=NS2-2 DO
               STACK$[N]=STACK$[N+2]
               N=N+1
            END WHILE
            NS2=NS2-2
            L=NS1-1
        END IF
        L=L+1
     END WHILE

     L=NS1
     WHILE L<=NS2 DO
        IF STACK$[L]="*" OR STACK$[L]="/" THEN
            IF L>=NS2 THEN GOTO 100 END IF
            N1#=VAL(STACK$[L-1])  N2#=VAL(STACK$[L+1])  NOP=NOP-1
            IF STACK$[L]="*" THEN RI#=N1#*N2# ELSE RI#=N1#/N2# END IF
            STACK$[L-1]=STR$(RI#)
            N=L
            WHILE N<=NS2-2 DO
               STACK$[N]=STACK$[N+2]
               N=N+1
            END WHILE
            NS2=NS2-2
            L=NS1-1
        END IF
        L=L+1
     END WHILE

     L=NS1
     WHILE L<=NS2 DO
        IF STACK$[L]="+" OR STACK$[L]="-" THEN
            EXIT IF L>=NS2
            N1#=VAL(STACK$[L-1])  N2#=VAL(STACK$[L+1])  NOP=NOP-1
            IF STACK$[L]="+" THEN RI#=N1#+N2# ELSE RI#=N1#-N2# END IF
            STACK$[L-1]=STR$(RI#)
            N=L
            WHILE N<=NS2-2 DO
               STACK$[N]=STACK$[N+2]
               N=N+1
            END WHILE
            NS2=NS2-2
            L=NS1-1
        END IF
        L=L+1
     END WHILE
100:
     IF NOP<2 THEN   ! operator priority
          DB#=VAL(STACK$[NS1])
       ELSE
          IF NOP<3 THEN
               DB#=VAL(STACK$[NS1+2])
             ELSE
               DB#=VAL(STACK$[NS1+4])
          END IF
     END IF
END PROCEDURE

PROCEDURE SVOLGI_PAR
   NPA=NPA-1
   FOR J=NS TO 1 STEP -1 DO
      EXIT IF STACK$[J]="("
   END FOR
   IF J=0 THEN
       NS1=1  NS2=NS  CALC_ARITM
       NERR=7
     ELSE
       FOR R=J TO NS-1 DO
          STACK$[R]=STACK$[R+1]
       END FOR
       NS1=J  NS2=NS-1  CALC_ARITM
       IF NS1=2 THEN NS1=1  STACK$[1]=STACK$[2] END IF
       NS=NS1
       COMPATTA_STACK
   END IF
END PROCEDURE

BEGIN
     OP_LIST$="+-*/^("
     NOP=0  NPA=0  NS=1  K$=""
     STACK$[1]="@"              ! init stack

     PRINT(CHR$(12);)
     INPUT(LINE,EXPRESSION$)

     FOR W=1 TO LEN(EXPRESSION$) DO
        LOOP
           CODE=ASC(MID$(EXPRESSION$,W,1))
           IF (CODE>=48 AND CODE<=57) OR CODE=46 THEN
                K$=K$+CHR$(CODE)
                W=W+1  IF W>LEN(EXPRESSION$) THEN GOTO 98 END IF
              ELSE
                EXIT IF K$=""
                IF NS>1 OR (NS=1 AND STACK$[1]<>"@") THEN NS=NS+1 END IF
                IF FLAG=0 THEN STACK$[NS]=K$ ELSE STACK$[NS]=STR$(VAL(K$)*FLAG) END IF
                K$=""  FLAG=0
                EXIT
           END IF
        END LOOP
        IF CODE=43 THEN K$="+" END IF
        IF CODE=45 THEN K$="-" END IF
        IF CODE=42 THEN K$="*" END IF
        IF CODE=47 THEN K$="/" END IF
        IF CODE=94 THEN K$="^" END IF

     CASE CODE OF
       43,45,42,47,94->
             IF MID$(EXPRESSION$,W+1,1)="-" THEN FLAG=-1  W=W+1 END IF
             IF INSTR(OP_LIST$,STACK$[NS])<>0 THEN
                 NERR=5
               ELSE
                 NS=NS+1  STACK$[NS]=K$  NOP=NOP+1
                 IF NOP>=2 THEN
                    FOR J=NS TO 1 STEP -1 DO
                       IF STACK$[J]<>"(" THEN
                           CONTINUE FOR
                       END IF
                       IF J<NS-2 THEN
                           EXIT
                         ELSE
                            GOTO 110
                       END IF
                    END FOR
                    NS1=J+1  NS2=NS  CALC_ARITM
                    NS=NS2  STACK$[NS]=K$
                    REGISTRO_X#=VAL(STACK$[NS-1])
                 END IF
             END IF
110:
       END ->

       40->
             IF NS>1 OR (NS=1 AND STACK$[1]<>"@") THEN NS=NS+1 END IF
             STACK$[NS]="("  NPA=NPA+1
             IF MID$(EXPRESSION$,W+1,1)="-" THEN FLAG=-1  W=W+1 END IF
       END ->

       41->
             SVOLGI_PAR
             IF NERR=7 THEN
                  NERR=0  NOP=0  NPA=0  NS=1
               ELSE
                  IF NERR=0 OR NERR=1 THEN
                      DB#=VAL(STACK$[NS])
                      REGISTRO_X#=DB#
                    ELSE
                      NOP=0  NPA=0  NS=1
                  END IF
            END IF
       END ->

       OTHERWISE
            NERR=8
   END CASE
   K$=""
   DISEGNA_STACK
END FOR

98:
   IF K$<>"" THEN
        IF NS>1 OR (NS=1 AND STACK$[1]<>"@") THEN NS=NS+1 END IF
        IF FLAG=0 THEN STACK$[NS]=K$ ELSE STACK$[NS]=STR$(VAL(K$)*FLAG) END IF
   END IF
   DISEGNA_STACK
   IF INSTR(OP_LIST$,STACK$[NS])<>0 THEN
         NERR=6
       ELSE
         WHILE NPA<>0 DO
             SVOLGI_PAR
         END WHILE
         IF NERR<>7 THEN NS1=1  NS2=NS  CALC_ARITM  END IF
    END IF
    NS=1  NOP=0  NPA=0
    !$RCODE="LOCATE 23,1"
    IF NERR>0 THEN PRINT("Internal Error #";NERR)  ELSE PRINT("Value is ";DB#) END IF
END PROGRAM

```

This solution is based on a stack: as a plus there is a power (^) operator. Unary operator "-" is accepted. Program shows the stack after every operation and you must press a key to go on (this feature can be avoided by removing the final REPEAT..UNTIL loop at the end of "DISEGNA_STACK" procedure).


## Factor


```factor
USING: accessors kernel locals math math.parser peg.ebnf ;
IN: rosetta.arith

TUPLE: operator left right ;
TUPLE: add < operator ;   C: <add> add
TUPLE: sub < operator ;   C: <sub> sub
TUPLE: mul < operator ;   C: <mul> mul
TUPLE: div < operator ;   C: <div> div

EBNF: expr-ast
spaces   = [\n\t ]*
digit    = [0-9]
number   = (digit)+                         => [[ string>number ]]

value    =   spaces number:n                => [[ n ]]
           | spaces "(" exp:e spaces ")"    => [[ e ]]

fac      =   fac:a spaces "*" value:b       => [[ a b <mul> ]]
           | fac:a spaces "/" value:b       => [[ a b <div> ]]
           | value

exp      =   exp:a spaces "+" fac:b         => [[ a b <add> ]]
           | exp:a spaces "-" fac:b         => [[ a b <sub> ]]
           | fac

main     = exp:e spaces !(.)                => [[ e ]]
;EBNF

GENERIC: eval-ast ( ast -- result )

M: number eval-ast ;

: recursive-eval ( ast -- left-result right-result )
    [ left>> eval-ast ] [ right>> eval-ast ] bi ;

M: add eval-ast recursive-eval + ;
M: sub eval-ast recursive-eval - ;
M: mul eval-ast recursive-eval * ;
M: div eval-ast recursive-eval / ;

: evaluate ( string -- result )
    expr-ast eval-ast ;
```


=={{header|F_Sharp|F#}}==
Using FsLex and FsYacc from the F# PowerPack, we implement this with multiple source files:

<code>AbstractSyntaxTree.fs</code>:

```fsharp
module AbstractSyntaxTree

type Expression =
  | Int    of int
  | Plus   of Expression * Expression
  | Minus  of Expression * Expression
  | Times  of Expression * Expression
  | Divide of Expression * Expression
```


<code>Lexer.fsl</code>:

```fsharp
{
module Lexer

open Parser  // we need the terminal tokens from the Parser

let lexeme = Lexing.LexBuffer<_>.LexemeString
}

let intNum     = '-'? ['0'-'9']+
let whitespace = ' ' | '\t'
let newline    = '\n' | '\r' '\n'

rule token = parse
    | intNum     { INT (lexeme lexbuf |> int) }
    | '+'        { PLUS }
    | '-'        { MINUS }
    | '*'        { TIMES }
    | '/'        { DIVIDE }
    | '('        { LPAREN }
    | ')'        { RPAREN }
    | whitespace { token lexbuf }
    | newline    { lexbuf.EndPos <- lexbuf.EndPos.NextLine; token lexbuf }
    | eof        { EOF }
    | _          { failwithf "unrecognized input: '%s'" <| lexeme lexbuf }
```


<code>Parser.fsy</code>:

```fsharp
%{
open AbstractSyntaxTree
%}

%start Expr

// terminal tokens
%token <int> INT
%token PLUS MINUS TIMES DIVIDE LPAREN RPAREN
%token EOF

// associativity and precedences
%left PLUS MINUS
%left TIMES DIVIDE

// return type of Expr
%type <Expression> Expr

%%

Expr: INT                     { Int $1 }
    | Expr PLUS Expr          { Plus ($1, $3) }
    | Expr MINUS Expr         { Minus ($1, $3) }
    | Expr TIMES Expr         { Times ($1, $3) }
    | Expr DIVIDE Expr        { Divide ($1, $3) }
    | LPAREN Expr RPAREN      { $2 }
```


<code>Program.fs</code>:

```fsharp
open AbstractSyntaxTree
open Lexer
open Parser

let parse txt =
  txt
  |> Lexing.LexBuffer<_>.FromString
  |> Parser.Expr Lexer.token

let rec eval = function
  | Int i        -> i
  | Plus (a,b)   -> eval a + eval b
  | Minus (a,b)  -> eval a - eval b
  | Times (a,b)  -> eval a * eval b
  | Divide (a,b) -> eval a / eval b

do
  "((11+15)*15)*2-(3)*4*1"
  |> parse
  |> eval
  |> printfn "%d"
```



## FreeBASIC


```FreeBASIC

'Arithmetic evaluation
'
'Create a program which parses and evaluates arithmetic expressions.
'
'Requirements
'
'    * An abstract-syntax tree (AST) for the expression must be created from parsing the
'      input.
'    * The AST must be used in evaluation, also, so the input may not be directly evaluated
'      (e.g. by calling eval or a similar language feature.)
'    * The expression will be a string or list of symbols like "(1+3)*7".
'    * The four symbols + - * / must be supported as binary operators with conventional
'      precedence rules.
'    * Precedence-control parentheses must also be supported.
'
'Standard mathematical precedence should be followed:
'
'    Parentheses
'    Multiplication/Division (left to right)
'    Addition/Subtraction (left to right)
'
'  test cases:
'  2*-3--4+-0.25 : returns -2.25
'  1 + 2 * (3 + (4 * 5 + 6 * 7 * 8) - 9) / 10 : returns 71

enum
    false = 0
    true = -1
end enum

enum Symbol
    unknown_sym
    minus_sym
    plus_sym
    lparen_sym
    rparen_sym
    number_sym
    mul_sym
    div_sym
    unary_minus_sym
    unary_plus_sym
    done_sym
    eof_sym
end enum

type Tree
    as Tree ptr leftp, rightp
    op as Symbol
    value as double
end type

dim shared sym as Symbol
dim shared tokenval as double
dim shared usr_input as string

declare function expr(byval p as integer) as Tree ptr

function isdigit(byval ch as string) as long
    return ch <> "" and Asc(ch) >= Asc("0") and Asc(ch) <= Asc("9")
end function

sub error_msg(byval msg as string)
    print msg
    system
end sub

' tokenize the input string
sub getsym()
    do
        if usr_input = "" then
            line input usr_input
            usr_input += chr(10)
        endif
        dim as string ch = mid(usr_input, 1, 1) ' get the next char
        usr_input = mid(usr_input, 2)           ' remove it from input

        sym = unknown_sym
        select case ch
            case " ":     continue do
            case chr(10), "": sym = done_sym: return
            case "+":     sym = plus_sym:     return
            case "-":     sym = minus_sym:    return
            case "*":     sym = mul_sym:      return
            case "/":     sym = div_sym:      return
            case "(":     sym = lparen_sym:   return
            case ")":     sym = rparen_sym:   return
            case else
                if isdigit(ch) then
                    dim s as string = ""
                    dim dot as integer = 0
                    do
                        s += ch
                        if ch = "." then dot += 1
                        ch = mid(usr_input, 1, 1)       ' get the next char
                        usr_input = mid(usr_input, 2)   ' remove it from input
                    loop while isdigit(ch) orelse ch = "."
                    if ch = "." or dot > 1 then error_msg("bogus number")
                    usr_input = ch + usr_input          ' prepend the char to input
                    tokenval = val(s)
                    sym = number_sym
                end if
                return
        end select
    loop
end sub

function make_node(byval op as Symbol, byval leftp as Tree ptr, byval rightp as Tree ptr) as Tree ptr
    dim t as Tree ptr

    t = callocate(len(Tree))
    t->op = op
    t->leftp = leftp
    t->rightp = rightp
    return t
end function

function is_binary(byval op as Symbol) as integer
    select case op
        case mul_sym, div_sym, plus_sym, minus_sym: return true
        case else: return false
    end select
end function

function prec(byval op as Symbol) as integer
    select case op
        case unary_minus_sym, unary_plus_sym:  return 100
        case mul_sym, div_sym:                 return  90
        case plus_sym, minus_sym:              return  80
        case else:                             return   0
    end select
end function

function primary as Tree ptr
    dim t as Tree ptr = 0

    select case sym
        case minus_sym, plus_sym
            dim op as Symbol = sym
            getsym()
            t = expr(prec(unary_minus_sym))
            if op = minus_sym then return make_node(unary_minus_sym, t, 0)
            if op = plus_sym  then return make_node(unary_plus_sym,  t, 0)
        case lparen_sym
            getsym()
            t = expr(0)
            if sym <> rparen_sym then error_msg("expecting rparen")
            getsym()
            return t
        case number_sym
            t = make_node(sym, 0, 0)
            t->value = tokenval
            getsym()
            return t
        case else: error_msg("expecting a primary")
    end select
end function

function expr(byval p as integer) as Tree ptr
    dim t as Tree ptr = primary()

    while is_binary(sym) andalso prec(sym) >= p
        dim t1 as Tree ptr
        dim op as Symbol = sym
        getsym()
        t1 = expr(prec(op) + 1)
        t = make_node(op, t, t1)
    wend
    return t
end function

function eval(byval t as Tree ptr) as double
    if t <> 0 then
        select case t->op
            case minus_sym:       return eval(t->leftp) - eval(t->rightp)
            case plus_sym:        return eval(t->leftp) + eval(t->rightp)
            case mul_sym:         return eval(t->leftp) * eval(t->rightp)
            case div_sym:         return eval(t->leftp) / eval(t->rightp)
            case unary_minus_sym: return -eval(t->leftp)
            case unary_plus_sym:  return  eval(t->leftp)
            case number_sym:      return t->value
            case else:            error_msg("unexpected tree node")
        end select
    end if
    return 0
end function

do
    getsym()
    if sym = eof_sym then exit do
    if sym = done_sym then continue do
    dim t as Tree ptr = expr(0)
    print"> "; eval(t)
    if sym = eof_sym then exit do
    if sym <> done_sym then error_msg("unexpected input")
loop

```


{{out}}

```txt

>calc
1 + 2 * (3 + (4 * 5 + 6 * 7 * 8) - 9) / 10
>  71

```



## Go


See [[Arithmetic Evaluator/Go]]



## Groovy

Solution:

```groovy
enum Op {
    ADD('+', 2),
    SUBTRACT('-', 2),
    MULTIPLY('*', 1),
    DIVIDE('/', 1);

    static {
        ADD.operation = { a, b -> a + b }
        SUBTRACT.operation = { a, b -> a - b }
        MULTIPLY.operation = { a, b -> a * b }
        DIVIDE.operation = { a, b -> a / b }
    }

    final String symbol
    final int precedence
    Closure operation

    private Op(String symbol, int precedence) {
        this.symbol = symbol
        this.precedence = precedence
    }

    String toString() { symbol }

    static Op fromSymbol(String symbol) {
        Op.values().find { it.symbol == symbol }
    }
}

interface Expression {
    Number evaluate();
}

class Constant implements Expression {
    Number value

    Constant (Number value) { this.value = value }

    Constant (String str) {
        try { this.value = str as BigInteger }
        catch (e) { this.value = str as BigDecimal }
    }

    Number evaluate() { value }

    String toString() { "${value}" }
}

class Term implements Expression {
    Op op
    Expression left, right

    Number evaluate() { op.operation(left.evaluate(), right.evaluate()) }

    String toString() { "(${op} ${left} ${right})" }
}

void fail(String msg, Closure cond = {true}) {
    if (cond()) throw new IllegalArgumentException("Cannot parse expression: ${msg}")
}

Expression parse(String expr) {
    def tokens = tokenize(expr)
    def elements = groupByParens(tokens, 0)
    parse(elements)
}

List tokenize(String expr) {
    def tokens = []
    def constStr = ""
    def captureConstant = { i ->
        if (constStr) {
            try { tokens << new Constant(constStr) }
            catch (NumberFormatException e) { fail "Invalid constant '${constStr}' near position ${i}" }
            constStr = ''
        }
    }
    for(def i = 0; i<expr.size(); i++) {
        def c = expr[i]
        def constSign = c in ['+','-'] && constStr.empty && (tokens.empty || tokens[-1] != ')')
        def isConstChar = { it in ['.'] + ('0'..'9') || constSign }
        if (c in ([')'] + Op.values()*.symbol) && !constSign) { captureConstant(i) }
        switch (c) {
            case ~/\s/:               break
            case isConstChar:         constStr += c; break
            case Op.values()*.symbol: tokens << Op.fromSymbol(c); break
            case ['(',')']:           tokens << c; break
            default:                  fail "Invalid character '${c}' at position ${i+1}"
        }
    }
    captureConstant(expr.size())
    tokens
}

List groupByParens(List tokens, int depth) {
    def deepness = depth
    def tokenGroups = []
    for (def i = 0; i < tokens.size(); i++) {
        def token = tokens[i]
        switch (token) {
            case '(':
                fail("'(' too close to end of expression") { i+2 > tokens.size() }
                def subGroup = groupByParens(tokens[i+1..-1], depth+1)
                tokenGroups << subGroup[0..-2]
                i += subGroup[-1] + 1
                break
            case ')':
                fail("Unbalanced parens, found extra ')'") { deepness == 0 }
                tokenGroups << i
                return tokenGroups
            default:
                tokenGroups << token
        }
    }
    fail("Unbalanced parens, unclosed groupings at end of expression") { deepness != 0 }
    def n = tokenGroups.size()
    fail("The operand/operator sequence is wrong") { n%2 == 0 }
    (0..<n).each {
        def i = it
        fail("The operand/operator sequence is wrong") { (i%2 == 0) == (tokenGroups[i] instanceof Op) }
    }
    tokenGroups
}

Expression parse(List elements) {
    while (elements.size() > 1) {
        def n = elements.size()
        fail ("The operand/operator sequence is wrong") { n%2 == 0 }
        def groupLoc = (0..<n).find { i -> elements[i] instanceof List }
        if (groupLoc != null) {
            elements[groupLoc] = parse(elements[groupLoc])
            continue
        }
        def opLoc = (0..<n).find { i -> elements[i] instanceof Op && elements[i].precedence == 1 } \
                        ?: (0..<n).find { i -> elements[i] instanceof Op && elements[i].precedence == 2 }
        if (opLoc != null) {
            fail ("Operator out of sequence") { opLoc%2 == 0 }
            def term = new Term(left:elements[opLoc-1], op:elements[opLoc], right:elements[opLoc+1])
            elements[(opLoc-1)..(opLoc+1)] = [term]
            continue
        }
    }
    return elements[0] instanceof List ? parse(elements[0]) : elements[0]
}
```


Test:

```groovy
def testParse = {
    def ex = parse(it)
    print """
Input: ${it}
AST:   ${ex}
value: ${ex.evaluate()}
"""
}


testParse('1+1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+1/15)/14)/13)/12)/11)/10)/9)/8)/7)/6)/5)/4)/3)/2')
assert (parse('1+1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+1/15)/14)/13)/12)/11)/10)/9)/8)/7)/6)/5)/4)/3)/2')
        .evaluate() - Math.E).abs() < 0.0000000000001
testParse('1 + 2*(3 - 2*(3 - 2)*((2 - 4)*5 - 22/(7 + 2*(3 - 1)) - 1)) + 1')
testParse('1 - 5 * 2 / 20 + 1')
testParse('(1 - 5) * 2 / (20 + 1)')
testParse('2 * (3 + ((5) / (7 - 11)))')
testParse('(2 + 3) / (10 - 5)')
testParse('(1 + 2) * 10 / 100')
testParse('(1 + 2 / 2) * (5 + 5)')
testParse('2*-3--4+-.25')
testParse('2*(-3)-(-4)+(-.25)')
testParse('((11+15)*15)*2-(3)*4*1')
testParse('((11+15)*15)* 2 + (3) * -4 *1')
testParse('(((((1)))))')
testParse('-35')
println()

try { testParse('((11+15)*1') } catch (e) { println e }
try { testParse('((11+15)*1)))') } catch (e) { println e }
try { testParse('((11+15)*x)') } catch (e) { println e }
try { testParse('+++++') } catch (e) { println e }
try { testParse('1 /') } catch (e) { println e }
try { testParse('1++') } catch (e) { println e }
try { testParse('*1') } catch (e) { println e }
try { testParse('/ 1 /') } catch (e) { println e }
```


{{out}}
<pre style="height:30ex;overflow:scroll;">Input: 1+1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+(1+1/15)/14)/13)/12)/11)/10)/9)/8)/7)/6)/5)/4)/3)/2
AST:   (+ (+ 1 1) (/ (+ 1 (/ (+ 1 (/ (+ 1 (/ (+ 1 (/ (+ 1 (/ (+ 1 (/ (+ 1 (/ (+ 1 (/ (+ 1 (/ (+ 1 (/ (+ 1 (/ (+ 1 (/ (+ 1 (/ 1 15)) 14)) 13)) 12)) 11)) 10)) 9)) 8)) 7)) 6)) 5)) 4)) 3)) 2))
value: 2.7182818284589946

Input: 1 + 2*(3 - 2*(3 - 2)*((2 - 4)*5 - 22/(7 + 2*(3 - 1)) - 1)) + 1
AST:   (+ (+ 1 (* 2 (- 3 (* (* 2 (- 3 2)) (- (- (* (- 2 4) 5) (/ 22 (+ 7 (* 2 (- 3 1))))) 1))))) 1)
value: 60

Input: 1 - 5 * 2 / 20 + 1
AST:   (+ (- 1 (/ (* 5 2) 20)) 1)
value: 1.5

Input: (1 - 5) * 2 / (20 + 1)
AST:   (/ (* (- 1 5) 2) (+ 20 1))
value: -0.3809523810

Input: 2 * (3 + ((5) / (7 - 11)))
AST:   (* 2 (+ 3 (/ 5 (- 7 11))))
value: 3.50

Input: (2 + 3) / (10 - 5)
AST:   (/ (+ 2 3) (- 10 5))
value: 1

Input: (1 + 2) * 10 / 100
AST:   (/ (* (+ 1 2) 10) 100)
value: 0.3

Input: (1 + 2 / 2) * (5 + 5)
AST:   (* (+ 1 (/ 2 2)) (+ 5 5))
value: 20

Input: 2*-3--4+-.25
AST:   (+ (- (* 2 -3) -4) -0.25)
value: -2.25

Input: 2*(-3)-(-4)+(-.25)
AST:   (+ (- (* 2 -3) -4) -0.25)
value: -2.25

Input: ((11+15)*15)*2-(3)*4*1
AST:   (- (* (* (+ 11 15) 15) 2) (* (* 3 4) 1))
value: 768

Input: ((11+15)*15)* 2 + (3) * -4 *1
AST:   (+ (* (* (+ 11 15) 15) 2) (* (* 3 -4) 1))
value: 768

Input: (((((1)))))
AST:   1
value: 1

Input: -35
AST:   -35
value: -35

java.lang.IllegalArgumentException: Cannot parse expression: Unbalanced parens, unclosed groupings at end of expression
java.lang.IllegalArgumentException: Cannot parse expression: Unbalanced parens, found extra ')'
java.lang.IllegalArgumentException: Cannot parse expression: Invalid character 'x' at position 10
java.lang.IllegalArgumentException: Cannot parse expression: Invalid constant '+' near position 1
java.lang.IllegalArgumentException: Cannot parse expression: The operand/operator sequence is wrong
java.lang.IllegalArgumentException: Cannot parse expression: Invalid constant '+' near position 3
java.lang.IllegalArgumentException: Cannot parse expression: The operand/operator sequence is wrong
java.lang.IllegalArgumentException: Cannot parse expression: The operand/operator sequence is wrong
```



## Haskell


```haskell
{-# LANGUAGE FlexibleContexts #-}

import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Combinator
import Data.Functor
import Data.Function (on)

data Exp
  = Num Int
  | Add Exp
        Exp
  | Sub Exp
        Exp
  | Mul Exp
        Exp
  | Div Exp
        Exp

expr
  :: Stream s m Char
  => ParsecT s u m Exp
expr = buildExpressionParser table factor
  where
    table =
      [ [op "*" Mul AssocLeft, op "/" Div AssocLeft]
      , [op "+" Add AssocLeft, op "-" Sub AssocLeft]
      ]
    op s f = Infix (f <$ string s)
    factor = (between `on` char) '(' ')' expr <|> (Num . read <$> many1 digit)

eval
  :: Integral a
  => Exp -> a
eval (Num x) = fromIntegral x
eval (Add a b) = eval a + eval b
eval (Sub a b) = eval a - eval b
eval (Mul a b) = eval a * eval b
eval (Div a b) = eval a `div` eval b

solution
  :: Integral a
  => String -> a
solution = either (const (error "Did not parse")) eval . parse expr ""

main :: IO ()
main = print $ solution "(1+3)*7"
```

{{Out}}

```txt
28
```


=={{header|Icon}} and {{header|Unicon}}==
A compact recursive descent parser using Hanson's device.  This program
*  handles left and right associativity and different precedences
*  is ready to handle any number of infix operators without adding more functions to handle the precedences
*  accepts integers, reals, and radix constants (e.g. 3r10 is 3 in base 3)
*  currently accepts the Icon operators + - * / % (remainder) and ^ (exponentiation) and unary operators + and -
*  string invocation is used to evaluate binary operators hence other Icon binary operators (including handle multiple character ones) can be easily added
*  uses Icon style type coercion on operands
*  represents the AST as a nested list eliminating unneeded parenthesis

*  Notice that the code looks remarkably like a typical grammar, rather than being an opaque cryptic solution
*  Does not rely on any library to silently solve 1/2 the problem; in fact, this code would probably suit being put into a library almost verbatim

```Icon
procedure main()                                                #: simple arithmetical parser / evaluator
   write("Usage: Input expression = Abstract Syntax Tree = Value, ^Z to end.")
   repeat {
      writes("Input expression : ")
      if not writes(line := read()) then break
      if map(line) ? { (x := E()) & pos(0) } then
         write(" = ", showAST(x), " = ", evalAST(x))
      else
         write(" rejected")
   }
end

procedure evalAST(X)                                            #: return the evaluated AST
   local x

   if type(X) == "list" then {
      x := evalAST(get(X))
      while x := get(X)(x, evalAST(get(X) | stop("Malformed AST.")))
   }
   return \x | X
end

procedure showAST(X)                                            #: return a string representing the AST
   local x,s

   s := ""
   every x := !X do
      s ||:= if type(x) == "list" then "(" || showAST(x) || ")" else x
   return s
end

########
# When you're writing a big parser, a few utility recognisers are very useful
#
procedure ws()    # skip optional whitespace
   suspend tab(many(' \t')) | ""
end

procedure digits()
   suspend tab(many(&digits))
end

procedure radixNum(r)    # r sets the radix
   static chars
   initial chars := &digits || &lcase
   suspend tab(many(chars[1 +: r]))
end
########

global token
record HansonsDevice(precedence,associativity)

procedure opinfo()
   static O
   initial {
      O := HansonsDevice([], table(&null))                         # parsing table
      put(O.precedence, ["+", "-"], ["*", "/", "%"], ["^"])        # Lowest to Highest precedence
      every O.associativity[!!O.precedence] := 1                   # default to 1 for LEFT associativity
      O.associativity["^"] := 0                                    # RIGHT associativity
   }
   return O
end

procedure E(k)                                                  #: Expression
   local lex, pL
   static opT
   initial opT := opinfo()

   /k := 1
   lex := []
   if not (pL := opT.precedence[k]) then                        # this op at this level?
      put(lex, F())
   else {
      put(lex, E(k + 1))
      while ws() & put(lex, token := =!pL) do
         put(lex, E(k + opT.associativity[token]))
   }
   suspend if *lex = 1 then lex[1] else lex                     # strip useless []
end

procedure F()                                                   #: Factor
   suspend ws() & (    # skip optional whitespace, and ...
      (="+" & F())              |          # unary + and a Factor, or ...
      (="-" || V())             |          # unary - and a Value, or ...
      (="-" & [-1, "*", F()])   |          # unary - and a Factor, or ...
     2(="(", E(), ws(), =")")   |          # parenthesized subexpression, or ...
       V()                                 # just a value
   )
end

procedure V()                                                   #: Value
   local r
   suspend ws() & numeric(    # skip optional whitespace, and ...
       =(r := 1 to 36) || ="r" || radixNum(r)             |     # N-based number, or ...
       digits() || (="." || digits() | "") || exponent()        # plain number with optional fraction
   )
end

procedure exponent()
   suspend tab(any('eE')) || =("+" | "-" | "") || digits() | ""
end
```


{{out|Sample Output}}

```txt
#matheval.exe

Usage: Input expression = Abstract Syntax Tree = Value, ^Z to end.
Input expression : 1
1 = 1 = 1
Input expression : -1
-1 = -1 = -1
Input expression : (-15/2.0)
(-15/2.0) = -15/2.0 = -7.5
Input expression : -(15/2.0)
-(15/2.0) = -1*(15/2.0) = -7.5
Input expression : 2+(3-4)*6/5^2^3%3
2+(3-4)*6/5^2^3%3 = 2+((3-4)*6/(5^(2^3))%3) = 2
Input expression : 1+2+3+4
1+2+3+4 = 1+2+3+4 = 10
Input expression : ((((2))))+3*5
((((2))))+3*5 = 2+(3*5) = 17
Input expression : 3r10*3
3r10*3 = 3r10*3 = 9
Input expression : ^Z
```



## J


Note that once you get beyond a few basic arithmetic operations, what we commonly call "mathematical precedence" stops making sense, and primary value for this kind of precedence has been that it allows polynomials to be expressed simply (but expressing polynomials as a sequence of coefficients, one for each exponent, is even simpler).

Nevertheless, this task deals only with simple arithmetic, so this kind of precedence is an arguably appropriate choice for this task.

The implementation here uses a shift/reduce parser to build a tree structure (which J happens to support) for evaluation:


```j
parse=:parse_parser_
eval=:monad define
  'gerund structure'=:y
  gerund@.structure
)

coclass 'parser'
classify=: '$()*/+-'&(((>:@#@[ # 2:) #: 2 ^ i.)&;:)

rules=: ''
patterns=: ,"0 assert 1

addrule=: dyad define
   rules=: rules,;:x
   patterns=: patterns,+./@classify"1 y
)

'Term'   addrule '$()',   '0',     '+-',: '0'
'Factor' addrule '$()+-', '0',     '*/',: '0'
'Parens' addrule '(',    '*/+-0', ')',:  ')*/+-0$'
rules=: rules,;:'Move'

buildTree=: monad define
  words=: ;:'$',y
  queue=: classify '$',y
  stack=: classify '$$$$'
  tokens=: ]&.>i.#words
  tree=: ''
  while.(#queue)+.6<#stack do.
    rule=: rules {~ i.&1 patterns (*./"1)@:(+./"1) .(*."1)4{.stack
    rule`:6''
  end.
  'syntax' assert 1 0 1 1 1 1 -: {:"1 stack
  gerund=: literal&.> (<,'%') (I. words=<,'/')} words
  gerund;1{tree
)

literal=:monad define ::]
  ".'t=.',y
  5!:1<'t'
)

Term=: Factor=: monad define
  stack=: ({.stack),(classify '0'),4}.stack
  tree=: ({.tree),(<1 2 3{tree),4}.tree
)

Parens=: monad define
  stack=: (1{stack),3}.stack
  tree=: (1{tree),3}.tree
)

Move=: monad define
  'syntax' assert 0<#queue
  stack=: ({:queue),stack
  queue=: }:queue
  tree=: ({:tokens),tree
  tokens=: }:tokens
)

parse=:monad define
  tmp=: conew 'parser'
  r=: buildTree__tmp y
  coerase tmp
  r
)
```

example use:

```j
   eval parse '1+2*3/(4-5+6)'
2.2
```


You can also display the syntax tree, for example:

```j
   parse '2*3/(4-5)'
┌─────────────────────────────────────────────────────┬───────────────────┐
│┌───┬───────┬───┬───────┬───┬─┬───────┬───┬───────┬─┐│┌───────┬─┬───────┐│
││┌─┐│┌─────┐│┌─┐│┌─────┐│┌─┐│(│┌─────┐│┌─┐│┌─────┐│)│││┌─┬─┬─┐│4│┌─┬─┬─┐││
│││$│││┌─┬─┐│││*│││┌─┬─┐│││%││ ││┌─┬─┐│││-│││┌─┬─┐││ ││││1│2│3││ ││6│7│8│││
││└─┘│││0│2│││└─┘│││0│3│││└─┘│ │││0│4│││└─┘│││0│5│││ │││└─┴─┴─┘│ │└─┴─┴─┘││
││   ││└─┴─┘││   ││└─┴─┘││   │ ││└─┴─┘││   ││└─┴─┘││ ││└───────┴─┴───────┘│
││   │└─────┘│   │└─────┘│   │ │└─────┘│   │└─────┘│ ││                   │
│└───┴───────┴───┴───────┴───┴─┴───────┴───┴───────┴─┘│                   │
└─────────────────────────────────────────────────────┴───────────────────┘
```


At the top level, the first box is a list of terminals, and the second box represents their parsed structure within the source sentence, with numbers indexing the respective terminals. Within the list of terminals - each terminal is contained with a box. Punctuation is simply the punctuation string (left or right parenthesis). Operators are strings inside of boxes (the leading $ "operator" in this example is not really an operator - it's just a placeholder that was used to help in the parsing). Numeric values are a box inside of a box where the inner box carries two further boxes. The first indicates data type ('0' for numbers) and the second carries the value.


## Java


Uses the [[Arithmetic/Rational/Java|BigRational class]] to handle arbitrary-precision numbers (rational numbers since basic arithmetic will result in rational values).


```java
import java.util.Stack;

public class ArithmeticEvaluation {

    public interface Expression {
        BigRational eval();
    }

    public enum Parentheses {LEFT}

    public enum BinaryOperator {
        ADD('+', 1),
        SUB('-', 1),
        MUL('*', 2),
        DIV('/', 2);

        public final char symbol;
        public final int precedence;

        BinaryOperator(char symbol, int precedence) {
            this.symbol = symbol;
            this.precedence = precedence;
        }

        public BigRational eval(BigRational leftValue, BigRational rightValue) {
            switch (this) {
                case ADD:
                    return leftValue.add(rightValue);
                case SUB:
                    return leftValue.subtract(rightValue);
                case MUL:
                    return leftValue.multiply(rightValue);
                case DIV:
                    return leftValue.divide(rightValue);
            }
            throw new IllegalStateException();
        }

        public static BinaryOperator forSymbol(char symbol) {
            for (BinaryOperator operator : values()) {
                if (operator.symbol == symbol) {
                    return operator;
                }
            }
            throw new IllegalArgumentException(String.valueOf(symbol));
        }
    }

    public static class Number implements Expression {
        private final BigRational number;

        public Number(BigRational number) {
            this.number = number;
        }

        @Override
        public BigRational eval() {
            return number;
        }

        @Override
        public String toString() {
            return number.toString();
        }
    }

    public static class BinaryExpression implements Expression {
        public final Expression leftOperand;
        public final BinaryOperator operator;
        public final Expression rightOperand;

        public BinaryExpression(Expression leftOperand, BinaryOperator operator, Expression rightOperand) {
            this.leftOperand = leftOperand;
            this.operator = operator;
            this.rightOperand = rightOperand;
        }

        @Override
        public BigRational eval() {
            BigRational leftValue = leftOperand.eval();
            BigRational rightValue = rightOperand.eval();
            return operator.eval(leftValue, rightValue);
        }

        @Override
        public String toString() {
            return "(" + leftOperand + " " + operator.symbol + " " + rightOperand + ")";
        }
    }

    private static void createNewOperand(BinaryOperator operator, Stack<Expression> operands) {
        Expression rightOperand = operands.pop();
        Expression leftOperand = operands.pop();
        operands.push(new BinaryExpression(leftOperand, operator, rightOperand));
    }

    public static Expression parse(String input) {
        int curIndex = 0;
        boolean afterOperand = false;
        Stack<Expression> operands = new Stack<>();
        Stack<Object> operators = new Stack<>();
        while (curIndex < input.length()) {
            int startIndex = curIndex;
            char c = input.charAt(curIndex++);

            if (Character.isWhitespace(c))
                continue;

            if (afterOperand) {
                if (c == ')') {
                    Object operator;
                    while (!operators.isEmpty() && ((operator = operators.pop()) != Parentheses.LEFT))
                        createNewOperand((BinaryOperator) operator, operands);
                    continue;
                }
                afterOperand = false;
                BinaryOperator operator = BinaryOperator.forSymbol(c);
                while (!operators.isEmpty() && (operators.peek() != Parentheses.LEFT) && (((BinaryOperator) operators.peek()).precedence >= operator.precedence))
                    createNewOperand((BinaryOperator) operators.pop(), operands);
                operators.push(operator);
                continue;
            }

            if (c == '(') {
                operators.push(Parentheses.LEFT);
                continue;
            }

            afterOperand = true;
            while (curIndex < input.length()) {
                c = input.charAt(curIndex);
                if (((c < '0') || (c > '9')) && (c != '.'))
                    break;
                curIndex++;
            }
            operands.push(new Number(BigRational.valueOf(input.substring(startIndex, curIndex))));
        }

        while (!operators.isEmpty()) {
            Object operator = operators.pop();
            if (operator == Parentheses.LEFT)
                throw new IllegalArgumentException();
            createNewOperand((BinaryOperator) operator, operands);
        }

        Expression expression = operands.pop();
        if (!operands.isEmpty())
            throw new IllegalArgumentException();
        return expression;
    }

    public static void main(String[] args) {
        String[] testExpressions = {
                "2+3",
                "2+3/4",
                "2*3-4",
                "2*(3+4)+5/6",
                "2 * (3 + (4 * 5 + (6 * 7) * 8) - 9) * 10",
                "2*-3--4+-.25"};
        for (String testExpression : testExpressions) {
            Expression expression = parse(testExpression);
            System.out.printf("Input: \"%s\", AST: \"%s\", value=%s%n", testExpression, expression, expression.eval());
        }
    }
}
```


{{out}}

```txt
Input: "2+3", AST: "(2 + 3)", value=5
Input: "2+3/4", AST: "(2 + (3 / 4))", value=11/4
Input: "2*3-4", AST: "((2 * 3) - 4)", value=2
Input: "2*(3+4)+5/6", AST: "((2 * (3 + 4)) + (5 / 6))", value=89/6
Input: "2 * (3 + (4 * 5 + (6 * 7) * 8) - 9) * 10", AST: "((2 * ((3 + ((4 * 5) + ((6 * 7) * 8))) - 9)) * 10)", value=7000
Input: "2*-3--4+-.25", AST: "(((2 * -3) - -4) + -1/4)", value=-9/4
```



## JavaScript

Numbers must have a digit before the decimal point, so 0.1 not .1.

Spaces are removed, expressions like <code>5--1</code> are treated as <code>5 - -1</code>


```javascript
function evalArithmeticExp(s) {
  s = s.replace(/\s/g,'').replace(/^\+/,'');
  var rePara = /\([^\(\)]*\)/;
  var exp = s.match(rePara);

  while (exp = s.match(rePara)) {
    s = s.replace(exp[0], evalExp(exp[0]));
  }
  return evalExp(s);

  function evalExp(s) {
    s = s.replace(/[\(\)]/g,'');
    var reMD = /\d+\.?\d*\s*[\*\/]\s*[+-]?\d+\.?\d*/;
    var reM = /\*/;
    var reAS = /-?\d+\.?\d*\s*[\+-]\s*[+-]?\d+\.?\d*/;
    var reA  = /\d\+/;
    var exp;

    while (exp = s.match(reMD)) {
      s = exp[0].match(reM)? s.replace(exp[0], multiply(exp[0])) : s.replace(exp[0], divide(exp[0]));
    }

    while (exp = s.match(reAS)) {
      s = exp[0].match(reA)? s.replace(exp[0], add(exp[0])) : s.replace(exp[0], subtract(exp[0]));
    }

    return '' + s;

    function multiply(s, b) {
      b = s.split('*');
      return b[0] * b[1];
    }

    function divide(s, b) {
      b = s.split('/');
      return b[0] / b[1];
    }

    function add(s, b) {
      s = s.replace(/^\+/,'').replace(/\++/,'+');
      b = s.split('+');
      return Number(b[0]) + Number(b[1]);
    }

    function subtract(s, b) {
      s = s.replace(/\+-|-\+/g,'-');

      if (s.match(/--/)) {
        return add(s.replace(/--/,'+'));
      }
      b = s.split('-');
      return b.length == 3? -1 * b[1] - b[2] : b[0] - b[1];
    }
  }
}
```



{{out|Sample Output}}

```txt
evalArithmeticExp('2+3') // 5
evalArithmeticExp('2+3/4') // 2.75
evalArithmeticExp('2*3-4') // 2
evalArithmeticExp('2*(3+4)+5/6') // 14.833333333333334
evalArithmeticExp('2 * (3 + (4 * 5 + (6 * 7) * 8) - 9) * 10') // 7000
evalArithmeticExp('2*-3--4+-0.25' // -2.25
```



## jq


This entry highlights the use of a PEG grammar expressed in jq.


###  PEG operations


```jq
def star(E): (E | star(E)) // .;
def plus(E): E | (plus(E) // . );
def optional(E): E // .;
def amp(E): . as $in | E | $in;
def neg(E): select( [E] == [] );
```




###  Helper functions


```jq
def literal($s):
  select(.remainder | startswith($s))
  | .result += [$s]
  | .remainder |= .[$s | length :] ;

def box(E):
   ((.result = null) | E) as $e
   | .remainder = $e.remainder
   | .result += [$e.result]  # the magic sauce
   ;

# Consume a regular expression rooted at the start of .remainder, or emit empty;
# on success, update .remainder and set .match but do NOT update .result
def consume($re):
  # on failure, match yields empty
  (.remainder | match("^" + $re)) as $match
  | .remainder |= .[$match.length :]
  | .match = $match.string ;

def parseNumber($re):
  consume($re)
  | .result = .result + [.match|tonumber] ;
```



###  PEG Grammar

The PEG grammar for arithmetic expressions follows the one given at the Perl 6 entry.
```jq
def Expr:

  def ws: consume(" *");

  def Number: ws | parseNumber( "-?[0-9]+([.][0-9]*)?" );

  def Sum:
    def Parenthesized: ws | consume("[(]") | ws | box(Sum) | ws | consume("[)]");
    def Factor: Parenthesized // Number;
    def Product: box(Factor | star( ws | (literal("*") // literal("/")) | Factor));
    Product | ws | star( (literal("+") // literal("-")) | Product);

  Sum;
```



###  Evaluation


```jq
# Left-to-right evaluation
def eval:
  if type == "array" then
    if length == 0 then null
    else .[-1] |= eval
    | if length == 1 then .[0]
      else (.[:-2] | eval) as $v
      | if   .[-2] == "*" then $v * .[-1]
        elif .[-2] == "/" then $v / .[-1]
        elif .[-2] == "+" then $v + .[-1]
        elif .[-2] == "-" then $v - .[-1]
        else tostring|error
	end
      end
    end
  else .
  end;

def eval(String):
  {remainder: String}
  | Expr.result
  | eval;
```



###  Example

    eval("2 * (3 -1) + 2 * 5")

produces: 14


## Jsish

From Javascript entry.


```javascript
/* Arithmetic evaluation, in Jsish */
function evalArithmeticExp(s) {
    s = s.replace(/\s/g,'').replace(/^\+/,'');
    var rePara = /\([^\(\)]*\)/;
    var exp;

    function evalExp(s) {
        s = s.replace(/[\(\)]/g,'');
        var reMD = /[0-9]+\.?[0-9]*\s*[\*\/]\s*[+-]?[0-9]+\.?[0-9]*/;
        var reM = /\*/;
        var reAS = /-?[0-9]+\.?[0-9]*\s*[\+-]\s*[+-]?[0-9]+\.?[0-9]*/;
        var reA    = /[0-9]\+/;
        var exp;

        function multiply(s, b=0) {
            b = s.split('*');
            return b[0] * b[1];
        }

        function divide(s, b=0) {
            b = s.split('/');
            return b[0] / b[1];
        }

        function add(s, b=0) {
            s = s.replace(/^\+/,'').replace(/\++/,'+');
            b = s.split('+');
            return Number(b[0]) + Number(b[1]);
        }

        function subtract(s, b=0) {
            s = s.replace(/\+-|-\+/g,'-');

            if (s.match(/--/)) {
                return add(s.replace(/--/,'+'));
            }
            b = s.split('-');
            return b.length == 3 ? -1 * b[1] - b[2] : b[0] - b[1];
        }

        while (exp = s.match(reMD)) {
            s = exp[0].match(reM) ? s.replace(exp[0], multiply(exp[0]).toString()) : s.replace(exp[0], divide(exp[0]).toString());
        }

        while (exp = s.match(reAS)) {
            s = exp[0].match(reA)? s.replace(exp[0], add(exp[0]).toString()) : s.replace(exp[0], subtract(exp[0]).toString());
        }

        return '' + s;
    }

    while (exp = s.match(rePara)) {
        s = s.replace(exp[0], evalExp(exp[0]));
    }

    return evalExp(s);
}

if (Interp.conf('unitTest')) {
;    evalArithmeticExp('2+3');
;    evalArithmeticExp('2+3/4');
;    evalArithmeticExp('2*3-4');
;    evalArithmeticExp('2*(3+4)+5/6');
;    evalArithmeticExp('2 * (3 + (4 * 5 + (6 * 7) * 8) - 9) * 10');
;    evalArithmeticExp('2*-3--4+-0.25');
}

/*
=!EXPECTSTART!=
evalArithmeticExp('2+3') ==> 5
evalArithmeticExp('2+3/4') ==> 2.75
evalArithmeticExp('2*3-4') ==> 2
evalArithmeticExp('2*(3+4)+5/6') ==> 14.8333333333333
evalArithmeticExp('2 * (3 + (4 * 5 + (6 * 7) * 8) - 9) * 10') ==> 7000
evalArithmeticExp('2*-3--4+-0.25') ==> -2.25
=!EXPECTEND!=
*/
```


{{out}}

```txt
prompt$ jsish --U arithmeticEvaluation.jsi
evalArithmeticExp('2+3') ==> 5
evalArithmeticExp('2+3/4') ==> 2.75
evalArithmeticExp('2*3-4') ==> 2
evalArithmeticExp('2*(3+4)+5/6') ==> 14.8333333333333
evalArithmeticExp('2 * (3 + (4 * 5 + (6 * 7) * 8) - 9) * 10') ==> 7000
evalArithmeticExp('2*-3--4+-0.25') ==> -2.25
```



## Julia

Julia's homoiconic nature and strong metaprogramming facilities make AST/Expression parsing and creation as accessible and programmatic as other language features

```julia>julia
 expr="2 * (3 -1) + 2 * 5"
"2 * (3 -1) + 2 * 5"

julia> parsed = parse(expr) #Julia provides low-level access to language parser for AST/Expr creation
:(+(*(2,-(3,1)),*(2,5)))

julia> t = typeof(parsed)
Expr

julia> names(t) #shows type fields
(:head,:args,:typ)

julia> parsed.args #Inspect our 'Expr' type innards
3-element Any Array:
 :+
 :(*(2,-(3,1)))
 :(*(2,5))

julia> typeof(parsed.args[2]) #'Expr' types can nest
Expr

julia> parsed.args[2].args
3-element Any Array:
  :*
 2
  :(-(3,1))

julia> parsed.args[2].args[3].args #Will nest until lowest level of AST
3-element Any Array:
  :-
 3
 1

julia> eval(parsed)
14

julia> eval(parse("1 - 5 * 2 / 20 + 1"))
1.5

julia> eval(parse("2 * (3 + ((5) / (7 - 11)))"))
3.5
```



## Kotlin

{{trans|JavaScript}}

```scala
// version 1.2.10

/* if string is empty, returns zero */
fun String.toDoubleOrZero() = this.toDoubleOrNull() ?: 0.0

fun multiply(s: String): String {
    val b = s.split('*').map { it.toDoubleOrZero() }
    return (b[0] * b[1]).toString()
}

fun divide(s: String): String {
    val b = s.split('/').map { it.toDoubleOrZero() }
    return (b[0] / b[1]).toString()
}

fun add(s: String): String {
    var t = s.replace(Regex("""^\+"""), "").replace(Regex("""\++"""), "+")
    val b = t.split('+').map { it.toDoubleOrZero() }
    return (b[0] + b[1]).toString()
}

fun subtract(s: String): String {
    var t = s.replace(Regex("""(\+-|-\+)"""), "-")
    if ("--" in t) return add(t.replace("--", "+"))
    val b = t.split('-').map { it.toDoubleOrZero() }
    return (if (b.size == 3) -b[1] - b[2] else b[0] - b[1]).toString()
}

fun evalExp(s: String): String {
    var t = s.replace(Regex("""[()]"""), "")
    val reMD = Regex("""\d+\.?\d*\s*[*/]\s*[+-]?\d+\.?\d*""")
    val reM  = Regex( """\*""")
    val reAS = Regex("""-?\d+\.?\d*\s*[+-]\s*[+-]?\d+\.?\d*""")
    val reA  = Regex("""\d\+""")

    while (true) {
        val match = reMD.find(t)
        if (match == null) break
        val exp = match.value
        val match2 = reM.find(exp)
        t = if (match2 != null)
                t.replace(exp, multiply(exp))
            else
                t.replace(exp, divide(exp))
    }

    while (true) {
        val match = reAS.find(t)
        if (match == null) break
        val exp = match.value
        val match2 = reA.find(exp)
        t = if (match2 != null)
                t.replace(exp, add(exp))
            else
                t.replace(exp, subtract(exp))
    }

    return t
}

fun evalArithmeticExp(s: String): Double {
    var t = s.replace(Regex("""\s"""), "").replace("""^\+""", "")
    val rePara = Regex("""\([^()]*\)""")
    while(true) {
        val match = rePara.find(t)
        if (match == null) break
        val exp = match.value
        t = t.replace(exp, evalExp(exp))
    }
    return evalExp(t).toDoubleOrZero()
}

fun main(arsg: Array<String>) {
    listOf(
        "2+3",
        "2+3/4",
        "2*3-4",
        "2*(3+4)+5/6",
        "2 * (3 + (4 * 5 + (6 * 7) * 8) - 9) * 10",
        "2*-3--4+-0.25",
         "-4 - 3",
         "((((2))))+ 3 * 5",
         "1 + 2 * (3 + (4 * 5 + 6 * 7 * 8) - 9) / 10",
         "1 + 2*(3 - 2*(3 - 2)*((2 - 4)*5 - 22/(7 + 2*(3 - 1)) - 1)) + 1"
    ).forEach { println("$it = ${evalArithmeticExp(it)}") }
}
```


{{out}}

```txt

2+3 = 5.0
2+3/4 = 2.75
2*3-4 = 2.0
2*(3+4)+5/6 = 14.833333333333334
2 * (3 + (4 * 5 + (6 * 7) * 8) - 9) * 10 = 7000.0
2*-3--4+-0.25 = -2.25
-4 - 3 = -7.0
((((2))))+ 3 * 5 = 17.0
1 + 2 * (3 + (4 * 5 + 6 * 7 * 8) - 9) / 10 = 71.0
1 + 2*(3 - 2*(3 - 2)*((2 - 4)*5 - 22/(7 + 2*(3 - 1)) - 1)) + 1 = 60.0

```



## Lua



```lua
require"lpeg"

P, R, C, S, V = lpeg.P, lpeg.R, lpeg.C, lpeg.S, lpeg.V

--matches arithmetic expressions and returns a syntax tree
expression = P{"expr";
ws = P" "^0,
number = C(R"09"^1) * V"ws",
lp = "(" * V"ws",
rp = ")" * V"ws",
sym = C(S"+-*/") * V"ws",
more = (V"sym" * V"expr")^0,
expr = V"number" * V"more" + V"lp" * lpeg.Ct(V"expr" * V"more") * V"rp" * V"more"}

--evaluates a tree
function eval(expr)
  --empty
  if type(expr) == "string" or type(expr) == "number" then return expr + 0 end

  --arithmetic functions
  tb = {["+"] = function(a,b) return eval(a) + eval(b) end,
		["-"] = function(a,b) return eval(a) - eval(b) end,
		["*"] = function(a,b) return eval(a) * eval(b) end,
		["/"] = function(a,b) return eval(a) / eval(b) end}

  --you could add ^ or other operators to this pretty easily
  for i, v in ipairs{"*/", "+-"} do
    for s, u in ipairs(expr) do
	  local k = type(u) == "string" and C(S(v)):match(u)
	  if k then
	    expr[s-1] = tb[k](expr[s-1],expr[s+1])
	    table.remove(expr, s)
	    table.remove(expr, s)
	  end
	end
  end
  return expr[1]
end

print(eval{expression:match(io.read())})
```



## Liberty BASIC


```lb

'[RC] Arithmetic evaluation.bas
'Buld the tree (with linked nodes, in array 'cause LB has no pointers)
'applying shunting yard algorythm.
'Then evaluate tree

global stack$   'operator/brakets stack
stack$=""

maxStack = 100
dim stack(maxStack) 'nodes stack
global SP 'stack pointer
SP = 0

'-------------------
global maxNode,curFree
global FirstOp,SecondOp,isNumber,NodeCont
global opList$
opList$ = "+-*/^"

maxNode=100
FirstOp=1   'pointers to other nodes; 0 means no pointer
SecondOp=2
isNumber=3  'like, 1 is number, 0 is operator
NodeCont=4  'number if isNumber; or mid$("+-*/^", i, 1) for 1..5 operator

dim node(NodeCont, maxNode)
'will be used from 1, 0 plays null pointer (no link)

curFree=1   'first free node
'-------------------

in$ = " 1 + 2 ^ 3 * 4 - 12 / 6 "
print "Input: "
print in$

'read tokens
token$ = "#"
while 1
    i=i+1
    token$ = word$(in$, i)
    if token$ = "" then i=i-1: exit while

    select case
    case token$ = "("
        'If the token is a left parenthesis, then push it onto the stack.
        call stack.push token$

    case token$ = ")"
        'If the token is a right parenthesis:
        'Until the token at the top of the stack is a left parenthesis, pop operators off the stack onto the output queue.
        'Pop the left parenthesis from the stack, but not onto the output queue.
        'If the stack runs out without finding a left parenthesis, then there are mismatched parentheses.
        while stack.peek$() <> "("
            'if stack is empty
            if stack$="" then print "Error: no matching '(' for token ";i: end
            'add operator node to tree
            child2=node.pop()
            child1=node.pop()
            call node.push addOpNode(child1,child2,stack.pop$())
        wend
        discard$=stack.pop$()   'discard "("

    case isOperator(token$)
        'If the token is an operator, o1, then:
        'while there is an operator token, o2, at the top of the stack, and
        'either o1 is left-associative and its precedence is equal to that of o2,
        'or o1 has precedence less than that of o2,
        '   pop o2 off the stack, onto the output queue;
        'push o1 onto the stack
        op1$=token$
        while(isOperator(stack.peek$()))
            op2$=stack.peek$()
            if (op2$<>"^" and precedence(op1$) = precedence(op2$)) _
                OR (precedence(op1$) < precedence(op2$)) then
                '"^" is the only right-associative operator
                'add operator node to tree
                child2=node.pop()
                child1=node.pop()
                call node.push addOpNode(child1,child2,stack.pop$())
            else
                exit while
            end if
        wend
        call stack.push op1$

    case else   'number
    'actually, wrohg operator could end up here, like say %
        'If the token is a number, then
        'add leaf node to tree (number)
        call node.push addNumNode(val(token$))
    end select

wend

'When there are no more tokens to read:
'While there are still operator tokens in the stack:
'   If the operator token on the top of the stack is a parenthesis, then there are mismatched parentheses.
'   Pop the operator onto the output queue.
while stack$<>""
    if stack.peek$() = "(" then print "no matching ')'": end
    'add operator node to tree
    child2=node.pop()
    child1=node.pop()
    call node.push addOpNode(child1,child2,stack.pop$())
wend

root = node.pop()
'call dumpNodes
print "Tree:"
call drawTree root, 1, 0, 3
locate 1, 10
print "Result: ";evaluate(root)

end

'------------------------------------------
function isOperator(op$)
    isOperator = instr(opList$, op$)<>0 AND len(op$)=1
end function

function precedence(op$)
    if isOperator(op$) then
        precedence = 1 _
            + (instr("+-*/^", op$)<>0) _
            + (instr("*/^", op$)<>0) _
            + (instr("^", op$)<>0)
    end if
end function

'------------------------------------------
sub stack.push s$
    stack$=s$+"|"+stack$
end sub

function stack.pop$()
    'it does return empty on empty stack or queue
    stack.pop$=word$(stack$,1,"|")
    stack$=mid$(stack$,instr(stack$,"|")+1)
end function

function stack.peek$()
    'it does return empty on empty stack or queue
    stack.peek$=word$(stack$,1,"|")
end function

'---------------------------------------
sub node.push s
    stack(SP)=s
    SP=SP+1
end sub

function node.pop()
    'it does return -999999 on empty stack
    if SP<1 then pop=-999999: exit function
    SP=SP-1
    node.pop=stack(SP)
end function

'
### =================================

sub dumpNodes
    for i = 1 to curFree-1
        print i,
        for j = 1 to 4
            print node(j, i),
        next
        print
    next
    print
end sub

function evaluate(node)
    if node=0 then exit function
    if node(isNumber, node) then
        evaluate = node(NodeCont, node)
        exit function
    end if
    'else operator
    op1 = evaluate(node(FirstOp, node))
    op2 = evaluate(node(SecondOp, node))
    select case node(NodeCont, node)    'opList$, "+-*/^"
    case 1
        evaluate = op1+op2
    case 2
        evaluate = op1-op2
    case 3
        evaluate = op1*op2
    case 4
        evaluate = op1/op2
    case 5
        evaluate = op1^op2
    end select
end function

sub drawTree node, level, leftRight, offsetY
    if node=0 then exit sub
    call drawTree node(FirstOp, node), level+1, leftRight-1/2^level, offsetY

    'print node
    'count on 80 char maiwin
    x = 40*(1+leftRight)
    y = level+offsetY
    locate x, y
    'print  x, y,">";
    if node(isNumber, node) then
        print node(NodeCont, node)
    else
        print  mid$(opList$, node(NodeCont, node),1)
    end if

    call drawTree node(SecondOp, node), level+1, leftRight+1/2^level, offsetY
end sub

function addNumNode(num)
'returns new node
    newNode=curFree
    curFree=curFree+1
    node(isNumber,newNode)=1
    node(NodeCont,newNode)=num

    addNumNode = newNode
end function

function addOpNode(firstChild, secondChild, op$)
'returns new node
'FirstOrSecond ignored if parent is 0
    newNode=curFree
    curFree=curFree+1
    node(isNumber,newNode)=0
    node(NodeCont,newNode)=instr(opList$, op$)

    node(FirstOp,newNode)=firstChild
    node(SecondOp,newNode)=secondChild

    addOpNode = newNode
end function

```


{{out}}

```txt

Input:
 1 + 2 ^ 3 * 4 - 12 / 6
Tree:
                                       -
                   +                                       /
         1                   *                   12                  6
                        ^         4
                     2    3

Result: 31

```



## M2000 Interpreter

There is a function called EVAL which has many variants, one of them is the Expression Evaluation (when we pass a string as parameter).
All visible variables can be used, and all known functions, internal and user (if they are visible at that point). Global variables and functions are always visible.


```M2000 Interpreter

y=100
Module CheckEval {
      A$="1 + 2 * (3 + (4 * 5 + 6 * 7 * 8) - 9) / 10"
      Print Eval(A$)
      x=10
      Print Eval("x+5")=x+5
      Print Eval("A$=A$")=True
      Try {
            Print Eval("y")  ' error: y is uknown here
      }
}
Call CheckEval

```


From BBC BASIC. In M2000 we can't call a user function which isn't a child function, so here we make all functions as members of same group, so now they call each other. A function as a member in a group can use other members, using a dot or This and a dot, so .Ast$() is same as This.Ast$().


```M2000 Interpreter

Module CheckAst {
      Group Eval {
                Function Ast$ (&in$) {
                        Def ast$, oper$
                        Do {
                              Ast$+=.Ast1$(&in$)
                              in$=Trim$(in$)
                              oper$=left$(in$,1)
                              if Instr("+-", oper$)>0 then {
                              ast$+=oper$
                              in$=Mid$(in$, 2)
                              } else exit
                        } until len(in$)=0
                        ="("+ast$+")"
                  }
                Function Ast1$ (&in$) {
                        Def ast$, oper$
                        Do {
                              Ast$+=.Ast2$(&in$)
                              in$=Trim$(in$)
                              oper$=left$(in$,1)
                              if Instr("*/", oper$)>0 then {
                              ast$+=oper$
                              in$=Mid$(in$, 2)
                              } else exit
                        } until len(in$)=0
                        ="("+ast$+")"
                  }
                Function Ast2$ (&in$) {
                        Def ast$, oper$
                        in$=Trim$(in$)
                        if Asc(in$)<>40 then =.Number$(&in$) : exit
                        in$=Mid$(in$, 2)
                        ast$=.Ast$(&in$)
                        in$=Mid$(in$, 2)
                        =ast$
                  }
                  Function Number$ (&in$) {
                        Def ch$, num$
                        Do {
                              ch$=left$(in$,1)
                              if instr("0123456789", ch$)>0  Then {
                                    num$+=ch$
                                    in$=Mid$(in$, 2)
                              } Else Exit
                        } until len(in$)=0
                        =num$
                  }
      }
      Expr$ = "1+2 * (3 + (4 * 5 + 6 * 7 * 8) - 9) / 10"
      Print Eval(Eval.Ast$(&Expr$))=71
}
CheckAst

```


=={{header|Mathematica}} / {{header|Wolfram Language}}==

```Mathematica
(*parsing:*)
parse[string_] :=
 Module[{e},
  StringCases[string,
     "+" | "-" | "*" | "/" | "(" | ")" |
      DigitCharacter ..] //. {a_String?DigitQ :>
      e[ToExpression@a], {x___, PatternSequence["(", a_e, ")"],
       y___} :> {x, a,
       y}, {x :
        PatternSequence[] |
         PatternSequence[___, "(" | "+" | "-" | "*" | "/"],
       PatternSequence[op : "+" | "-", a_e], y___} :> {x, e[op, a],
       y}, {x :
        PatternSequence[] | PatternSequence[___, "(" | "+" | "-"],
       PatternSequence[a_e, op : "*" | "/", b_e], y___} :> {x,
       e[op, a, b],
       y}, {x :
        PatternSequence[] | PatternSequence[___, "(" | "+" | "-"],
       PatternSequence[a_e, b_e], y___} :> {x, e["*", a, b],
       y}, {x : PatternSequence[] | PatternSequence[___, "("],
       PatternSequence[a_e, op : "+" | "-", b_e],
       y : PatternSequence[] |
         PatternSequence[")" | "+" | "-", ___]} :> {x, e[op, a, b],
       y}} //. {e -> List, {a_Integer} :> a, {a_List} :> a}]

(*evaluation*)
evaluate[a_Integer] := a;
evaluate[{"+", a_}] := evaluate[a];
evaluate[{"-", a_}] := -evaluate[a];
evaluate[{"+", a_, b_}] := evaluate[a] + evaluate[b];
evaluate[{"-", a_, b_}] := evaluate[a] - evaluate[b];
evaluate[{"*", a_, b_}] := evaluate[a]*evaluate[b];
evaluate[{"/", a_, b_}] := evaluate[a]/evaluate[b];
evaluate[string_String] := evaluate[parse[string]]
```


Example use:

```Mathematica
parse["-1+2(3+4*-5/6)"]
evaluate["-1+2(3+4*-5/6)"]
```


{{out}}

```txt
{"+", {"-", 1}, {"*", 2, {"-", 3, {"/", {"*", 4, {"-", 5}}, 6}}}}
35/3
```



## Nim


{{works with|Nim|0.19.0}}

This implementation uses a Pratt parser.


```nim
import strutils
import os

#--
# Lexer
#--

type
  TokenKind = enum
    tokNumber
    tokPlus = "+", tokMinus = "-", tokStar = "*", tokSlash = "/"
    tokLPar, tokRPar
    tokEnd
  Token = object
    case kind: TokenKind
    of tokNumber: value: float
    else: discard

proc lex(input: string): seq[Token] =
  # Here we go through the entire input string and collect all the tokens into
  # a sequence.
  var pos = 0
  while pos < input.len:
    case input[pos]
    of '0'..'9':
      # Digits consist of three parts: the integer part, the delimiting decimal
      # point, and the decimal part.
      var numStr = ""
      while pos < input.len and input[pos] in Digits:
        numStr.add(input[pos])
        inc(pos)
      if pos < input.len and input[pos] == '.':
        numStr.add('.')
        inc(pos)
        while pos < input.len and input[pos] in Digits:
          numStr.add(input[pos])
          inc(pos)
      result.add(Token(kind: tokNumber, value: numStr.parseFloat()))
    of '+': inc(pos); result.add(Token(kind: tokPlus))
    of '-': inc(pos); result.add(Token(kind: tokMinus))
    of '*': inc(pos); result.add(Token(kind: tokStar))
    of '/': inc(pos); result.add(Token(kind: tokSlash))
    of '(': inc(pos); result.add(Token(kind: tokLPar))
    of ')': inc(pos); result.add(Token(kind: tokRPar))
    of ' ': inc(pos)
    else: raise newException(ArithmeticError,
                             "Unexpected character '" & input[pos] & '\'')
  # We append an 'end' token to the end of our token sequence, to mark where the
  # sequence ends.
  result.add(Token(kind: tokEnd))

#--
# Parser
#--

type
  ExprKind = enum
    exprNumber
    exprBinary
  Expr = ref object
    case kind: ExprKind
    of exprNumber: value: float
    of exprBinary:
      left, right: Expr
      operator: TokenKind

proc `$`(ex: Expr): string =
  # This proc returns a lisp representation of the expression.
  case ex.kind
  of exprNumber: $ex.value
  of exprBinary: '(' & $ex.operator & ' ' & $ex.left & ' ' & $ex.right & ')'

var
  # The input to the program is provided via command line parameters.
  tokens = lex(commandLineParams().join(" "))
  pos = 0

# This table stores the precedence level of each infix operator. For tokens
# this does not apply to, the precedence is set to 0.
const Precedence: array[low(TokenKind)..high(TokenKind), int] = [
  tokNumber: 0,
  tokPlus: 1,
  tokMinus: 1,
  tokStar: 2,
  tokSlash: 2,
  tokLPar: 0,
  tokRPar: 0,
  tokEnd: 0
]

# We use a Pratt parser, so the two primary components are the prefix part, and
# the infix part. We start with a prefix token, and when we're done, we continue
# with an infix token.

proc parse(prec = 0): Expr

proc parseNumber(token: Token): Expr =
  result = Expr(kind: exprNumber, value: token.value)

proc parseParen(token: Token): Expr =
  result = parse()
  if tokens[pos].kind != tokRPar:
    raise newException(ArithmeticError, "Unbalanced parenthesis")
  inc(pos)

proc parseBinary(left: Expr, token: Token): Expr =
  result = Expr(kind: exprBinary, left: left, right: parse(),
                operator: token.kind)

proc parsePrefix(token: Token): Expr =
  case token.kind
  of tokNumber: result = parseNumber(token)
  of tokLPar: result = parseParen(token)
  else: discard

proc parseInfix(left: Expr, token: Token): Expr =
  case token.kind
  of tokPlus, tokMinus, tokStar, tokSlash: result = parseBinary(left, token)
  else: discard

proc parse(prec = 0): Expr =
  # This procedure is the heart of a Pratt parser, it puts the whole expression
  # together into one abstract syntax tree, properly dealing with precedence.
  var token = tokens[pos]
  inc(pos)
  result = parsePrefix(token)
  while prec < Precedence[tokens[pos].kind]:
    token = tokens[pos]
    if token.kind == tokEnd:
      # When we hit the end token, we're done.
      break
    inc(pos)
    result = parseInfix(result, token)

let ast = parse()

proc `==`(ex: Expr): float =
  # This proc recursively evaluates the given expression.
  result =
    case ex.kind
    of exprNumber: ex.value
    of exprBinary:
      case ex.operator
      of tokPlus: ==ex.left + ==ex.right
      of tokMinus: ==ex.left - ==ex.right
      of tokStar: ==ex.left * ==ex.right
      of tokSlash: ==ex.left / ==ex.right
      else: 0.0

# In the end, we print out the result.
echo ==ast
```



## OCaml



```ocaml
type expression =
  | Const of float
  | Sum  of expression * expression   (* e1 + e2 *)
  | Diff of expression * expression   (* e1 - e2 *)
  | Prod of expression * expression   (* e1 * e2 *)
  | Quot of expression * expression   (* e1 / e2 *)

let rec eval = function
  | Const c -> c
  | Sum (f, g) -> eval f +. eval g
  | Diff(f, g) -> eval f -. eval g
  | Prod(f, g) -> eval f *. eval g
  | Quot(f, g) -> eval f /. eval g

open Genlex

let lexer = make_lexer ["("; ")"; "+"; "-"; "*"; "/"]

let rec parse_expr = parser
     [< e1 = parse_mult; e = parse_more_adds e1 >] -> e
 and parse_more_adds e1 = parser
     [< 'Kwd "+"; e2 = parse_mult; e = parse_more_adds (Sum(e1, e2)) >] -> e
   | [< 'Kwd "-"; e2 = parse_mult; e = parse_more_adds (Diff(e1, e2)) >] -> e
   | [< >] -> e1
 and parse_mult = parser
     [< e1 = parse_simple; e = parse_more_mults e1 >] -> e
 and parse_more_mults e1 = parser
     [< 'Kwd "*"; e2 = parse_simple; e = parse_more_mults (Prod(e1, e2)) >] -> e
   | [< 'Kwd "/"; e2 = parse_simple; e = parse_more_mults (Quot(e1, e2)) >] -> e
   | [< >] -> e1
 and parse_simple = parser
   | [< 'Int i >] -> Const(float i)
   | [< 'Float f >] -> Const f
   | [< 'Kwd "("; e = parse_expr; 'Kwd ")" >] -> e


let parse_expression = parser [< e = parse_expr; _ = Stream.empty >] -> e

let read_expression s = parse_expression(lexer(Stream.of_string s))
```


Using the function <code>read_expression</code> in an interactive loop:


```ocaml
let () =
  while true do
    print_string "Expression: ";
    let str = read_line() in
    if str = "q" then exit 0;
    let expr = read_expression str in
    let res = eval expr in
    Printf.printf " = %g\n%!" res;
  done
```


Compile with:
 ocamlopt -pp camlp4o arith_eval.ml -o arith_eval.opt


## ooRexx


```ooRexx

expressions = .array~of("2+3", "2+3/4", "2*3-4", "2*(3+4)+5/6", "2 * (3 + (4 * 5 + (6 * 7) * 8) - 9) * 10", "2*-3--4+-.25")
loop input over expressions
    expression = createExpression(input)
    if expression \= .nil then
        say 'Expression "'input'" parses to "'expression~string'" and evaluates to "'expression~evaluate'"'
end


-- create an executable expression from the input, printing out any
-- errors if they are raised.
::routine createExpression
  use arg inputString
--  signal on syntax
  return .ExpressionParser~parseExpression(inputString)

syntax:
   condition = condition('o')
   say condition~errorText
   say condition~message
   return .nil


-- a base class for tree nodes in the tree
-- all nodes return some sort of value.  This can be constant,
-- or the result of additional evaluations
::class evaluatornode
-- all evaluation is done here
::method evaluate abstract

-- node for numeric values in the tree
::class constant
::method init
  expose value
  use arg value

::method evaluate
  expose value
  return value

::method string
  expose value
  return value

-- node for a parenthetical group on the tree
::class parens
::method init
  expose subexpression
  use arg subexpression

::method evaluate
  expose subexpression
  return subexpression~evaluate

::method string
  expose subexpression
  return "("subexpression~string")"

-- base class for binary operators
::class binaryoperator
::method init
  expose left right
  -- the left and right sides are set after the left and right sides have
  -- been resolved.
  left = .nil
  right = .nil

-- base operation
::method evaluate
  expose left right
  return self~operation(left~evaluate, right~evaluate)

-- the actual operation of the node
::method operation abstract
::method symbol abstract
::method precedence abstract

-- display an operator as a string value
::method string
  expose left right
  return '('left~string self~symbol right~string')'

::attribute left
::attribute right

::class addoperator subclass binaryoperator
::method operation
  use arg left, right
  return left + right

::method symbol
  return "+"

::method precedence
  return 1

::class subtractoperator subclass binaryoperator
::method operation
  use arg left, right
  return left - right

::method symbol
  return "-"

::method precedence
  return 1

::class multiplyoperator subclass binaryoperator
::method operation
  use arg left, right
  return left * right

::method symbol
  return "*"

::method precedence
  return 2

::class divideoperator subclass binaryoperator
::method operation
  use arg left, right
  return left / right

::method symbol
  return "/"

::method precedence
  return 2

-- a class to parse the expression and build an evaluation tree
::class expressionParser
-- create a resolved operand from an operator instance and the top
-- two entries on the operand stack.
::method createNewOperand class
  use strict arg operator, operands
  -- the operands are a stack, so they are in inverse order current
  operator~right = operands~pull
  operator~left = operands~pull
  -- this goes on the top of the stack now
  operands~push(operator)

::method parseExpression class
  use strict arg inputString
  -- stacks for managing the operands and pending operators
  operands = .queue~new
  operators = .queue~new
  -- this flags what sort of item we expect to find at the current
  -- location
  afterOperand = .false

  loop currentIndex = 1 to inputString~length
      char = inputString~subChar(currentIndex)
      -- skip over whitespace
      if char == ' ' then iterate currentIndex
      -- If the last thing we parsed was an operand, then
      -- we expect to see either a closing paren or an
      -- operator to appear here
      if afterOperand then do
          if char == ')' then do
              loop while \operators~isempty
                  operator = operators~pull
                  -- if we find the opening paren, replace the
                  -- top operand with a paren group wrapper
                  -- and stop popping items
                  if operator == '(' then do
                     operands~push(.parens~new(operands~pull))
                     leave
                  end
                  -- collapse the operator stack a bit
                  self~createNewOperand(operator, operands)
              end
              -- done with this character
              iterate currentIndex
          end
          afterOperand = .false
          operator = .nil
          if char == "+" then operator = .addoperator~new
          else if char == "-" then operator = .subtractoperator~new
          else if char == "*" then operator = .multiplyoperator~new
          else if char == "/" then operator = .divideoperator~new
          if operator \= .nil then do
              loop while \operators~isEmpty
                  top = operators~peek
                  -- start of a paren group stops the popping
                  if top == '(' then leave
                  -- or the top operator has a lower precedence
                  if top~precedence < operator~precedence then leave
                  -- process this pending one
                  self~createNewOperand(operators~pull, operands)
              end
              -- this new operator is now top of the stack
              operators~push(operator)
              -- and back to the top
              iterate currentIndex
          end
          raise syntax 98.900 array("Invalid expression character" char)
      end
      -- if we've hit an open paren, add this to the operator stack
      -- as a phony operator
      if char == '(' then do
          operators~push('(')
          iterate currentIndex
      end
      -- not an operator, so we have an operand of some type
      afterOperand = .true
      startindex = currentIndex
      -- allow a leading minus sign on this
      if inputString~subchar(currentIndex) == '-' then
          currentIndex += 1
      -- now scan for the end of numbers
      loop while currentIndex <= inputString~length
          -- exit for any non-numeric value
          if \inputString~matchChar(currentIndex, "0123456789.") then leave
          currentIndex += 1
      end
      -- extract the string value
      operand = inputString~substr(startIndex, currentIndex - startIndex)
      if \operand~datatype('Number') then
          raise syntax 98.900 array("Invalid numeric operand '"operand"'")
      -- back this up to the last valid character
      currentIndex -= 1
      -- add this to the operand stack as a tree element that returns a constant
      operands~push(.constant~new(operand))
  end

  loop while \operators~isEmpty
      operator = operators~pull
      if operator == '(' then
          raise syntax 98.900 array("Missing closing ')' in expression")
      self~createNewOperand(operator, operands)
  end
  -- our entire expression should be the top of the expression tree
  expression = operands~pull
  if \operands~isEmpty then
      raise syntax 98.900 array("Invalid expression")
  return expression

```

{{out}}

```txt

Expression "2+3" parses to "(2 + 3)" and evaluates to "5"
Expression "2+3/4" parses to "(2 + (3 / 4))" and evaluates to "2.75"
Expression "2*3-4" parses to "((2 * 3) - 4)" and evaluates to "2"
Expression "2*(3+4)+5/6" parses to "((2 * ((3 + 4))) + (5 / 6))" and evaluates to "14.8333333"
Expression "2 * (3 + (4 * 5 + (6 * 7) * 8) - 9) * 10" parses to "((2 * (((3 + (((4 * 5) + (((6 * 7)) * 8)))) - 9))) * 10)" and evaluates to 7000"
Expression "2*-3--4+-.25" parses to "(((2 * -3) - -4) + -.25)" and evaluates to "-2.25"

```


## Oz

We can create a simple, but slow parser using logic programming.
Every procedure reads the input characters from <code>X0</code> and returns the remaining characters in <code>X</code>. The AST is returned as the regular return value.

The <code>Do</code> procedure automatically threads the input state through a sequence of procedure calls.


```oz
declare

  fun {Expr X0 ?X}
     choice
        [L _ R] = {Do [Term &+ Expr] X0 ?X} in add(L R)
     [] [L _ R] = {Do [Term &- Expr] X0 ?X} in sub(L R)
     [] {Term X0 X}
     end
  end

  fun {Term X0 ?X}
     choice
        [L _ R] = {Do [Factor &* Term] X0 ?X} in mul(L R)
     [] [L _ R] = {Do [Factor &/ Term] X0 ?X} in 'div'(L R)
     [] {Factor X0 X}
     end
  end

  fun {Factor X0 ?X}
     choice {Parens Expr X0 X}
     [] {Number X0 X}
     end
  end

  fun {Number X0 X}
     Ds = {Many1 Digit X0 X}
  in
     num(Ds)
  end

  fun {Digit X0 ?X}
     D|!X = X0
  in
     D = choice &0 [] &1 [] &2 [] &3 [] &4 [] &5 [] &6 [] &7 [] &8 [] &9 end
  end

  fun {Many1 Rule X0 ?X}
     choice [{Rule X0 X}]
     [] X1 in {Rule X0 X1}|{Many1 Rule X1 X}
     end
  end

  fun {Parens Rule X0 ?X}
     [_ R _] = {Do [&( Rule &)] X0 X}
  in
     R
  end

  fun {Do Rules X0 ?X}
     Res#Xn = {FoldL Rules
               fun {$ Res#Xi Rule}
                  if {Char.is Rule} then
                     !Rule|X2 = Xi
                  in
                     (Rule|Res) # X2
                  elseif {Procedure.is Rule} then
                     X2 in
                     ({Rule Xi X2}|Res) # X2
                  end
               end
               nil#X0}
  in
     X = Xn
     {Reverse Res}
  end

  %% Returns a singleton list if an AST was found or nil otherwise.
  fun {Parse S}
     {SearchOne fun {$} {Expr S nil} end}
  end

  fun {Eval X}
     case X of
        num(Ds)    then {String.toInt Ds}
     [] add(L R)   then {Eval L} + {Eval R}
     [] sub(L R)   then {Eval L} - {Eval R}
     [] mul(L R)   then {Eval L} * {Eval R}
     [] 'div'(L R) then {Eval L} div {Eval R}
     end
  end

  [AST] = {Parse "((11+15)*15)*2-(3)*4*1"}

in

  {Inspector.configure widgetShowStrings true}
  {Inspect AST}
  {Inspect {Eval AST}}
```


To improve performance, the number of choice points should be limited, for example by reading numbers deterministically instead.
For real parsing with possible large input, it is however recommended to use [http://www.mozart-oz.org/home/doc/gump/node5.html Gump], Mozart's parser generator.


## Pascal

See [[Arithmetic Evaluator/Pascal]].


## Perl


```perl
sub ev
# Evaluates an arithmetic expression like "(1+3)*7" and returns
# its value.
 {my $exp = shift;
  # Delete all meaningless characters. (Scientific notation,
  # infinity, and not-a-number aren't supported.)
  $exp =~ tr {0-9.+-/*()} {}cd;
  return ev_ast(astize($exp));}

 {my $balanced_paren_regex;
  $balanced_paren_regex = qr
     {\( ( [^()]+ | (??{$balanced_paren_regex}) )+ \)}x;
  # ??{ ... } interpolates lazily (only when necessary),
  # permitting recursion to arbitrary depths.

  sub astize
  # Constructs an abstract syntax tree by recursively
  # transforming textual arithmetic expressions into array
  # references of the form [operator, left oprand, right oprand].
   {my $exp = shift;
    # If $exp is just a number, return it as-is.
    $exp =~ /[^0-9.]/ or return $exp;
    # If parentheses surround the entire expression, get rid of
    # them.
    $exp = substr($exp, 1, -1)
        while $exp =~ /\A($balanced_paren_regex)\z/;
    # Replace stuff in parentheses with placeholders.
    my @paren_contents;
    $exp =~ s {($balanced_paren_regex)}
              {push(@paren_contents, $1);
               "[p$#paren_contents]"}eg;
    # Scan for operators in order of increasing precedence,
    # preferring the rightmost.
    $exp =~ m{(.+) ([+-]) (.+)}x or
        $exp =~ m{(.+) ([*/]) (.+)}x or
        # The expression must've been malformed somehow.
        # (Note that unary minus isn't supported.)
        die "Eh?: [$exp]\n";
    my ($op, $lo, $ro) = ($2, $1, $3);
    # Restore the parenthetical expressions.
    s {\[p(\d+)\]} {($paren_contents[$1])}eg
        foreach $lo, $ro;
    # And recurse.
    return [$op, astize($lo), astize($ro)];}}

 {my %ops =
     ('+' => sub {$_[0] + $_[1]},
      '-' => sub {$_[0] - $_[1]},
      '*' => sub {$_[0] * $_[1]},
      '/' => sub {$_[0] / $_[1]});

  sub ev_ast
  # Evaluates an abstract syntax tree of the form returned by
  # &astize.
   {my $ast = shift;
    # If $ast is just a number, return it as-is.
    ref $ast or return $ast;
    # Otherwise, recurse.
    my ($op, @operands) = @$ast;
    $_ = ev_ast($_) foreach @operands;
    return $ops{$op}->(@operands);}}
```



## Perl 6

{{Works with|rakudo|2018.03}}


```perl6
sub ev (Str $s --> Numeric) {

    grammar expr {
        token TOP { ^ <sum> $ }
        token sum { <product> (('+' || '-') <product>)* }
        token product { <factor> (('*' || '/') <factor>)* }
        token factor { <unary_minus>? [ <parens> || <literal> ] }
        token unary_minus { '-' }
        token parens { '(' <sum> ')' }
        token literal { \d+ ['.' \d+]? || '.' \d+ }
    }

    my sub minus ($b) { $b ?? -1 !! +1 }

    my sub sum ($x) {
        [+] flat product($x<product>), map
            { minus($^y[0] eq '-') * product $^y<product> },
            |($x[0] or [])
    }

    my sub product ($x) {
        [*] flat factor($x<factor>), map
            { factor($^y<factor>) ** minus($^y[0] eq '/') },
            |($x[0] or [])
    }

    my sub factor ($x) {
        minus($x<unary_minus>) * ($x<parens>
          ?? sum $x<parens><sum>
          !! $x<literal>)
    }

    expr.parse([~] split /\s+/, $s);
    $/ or fail 'No parse.';
    sum $/<sum>;

}

# Testing:

say ev '5';                                    #   5
say ev '1 + 2 - 3 * 4 / 5';                    #   0.6
say ev '1 + 5*3.4 - .5  -4 / -2 * (3+4) -6';   #  25.5
say ev '((11+15)*15)* 2 + (3) * -4 *1';        # 768
```



## Phix

This is really just a simplification of the one in the heart of Phix,
which of course by now is thousands of lines spread over several files,
plus this as asked for has an AST, whereas Phix uses cross-linked flat IL.

```Phix
sequence opstack = {}       -- atom elements are literals,
                            -- sequence elements are subexpressions
                            -- on completion length(opstack) should be 1
object token

constant op_p_p = 0         --  1: expressions stored as op,p1,p2
    --   p_op_p             --  0: expressions stored as p1,op,p2
    --   p_p_op             -- -1: expressions stored as p1,p2,op

object op = 0   -- 0 if none, else "+", "-", "*", "/", "^", "%", or "u-"

string s        -- the expression being parsed
integer ch
integer sidx

procedure err(string msg)
    printf(1,"%s\n%s^ %s\n\nPressEnter...",{s,repeat(' ',sidx-1),msg})
    {} = wait_key()
    abort(0)
end procedure

procedure nxtch(object msg="eof")
    sidx += 1
    if sidx>length(s) then
        if string(msg) then err(msg) end if
        ch = -1
    else
        ch = s[sidx]
    end if
end procedure

procedure skipspaces()
    while find(ch," \t\r\n")!=0 do nxtch(0) end while
end procedure

procedure get_token()
atom n, fraction
integer dec
    skipspaces()
    if ch=-1 then token = "eof" return end if
    if ch>='0' and ch<='9' then
        n = ch-'0'
        while 1 do
            nxtch(0)
            if ch<'0' or ch>'9' then exit end if
            n = n*10+ch-'0'
        end while
        if ch='.' then
            dec = 1
            fraction = 0
            while 1 do
                nxtch(0)
                if ch<'0' or ch>'9' then exit end if
                fraction = fraction*10 + ch-'0'
                dec *= 10
            end while
            n += fraction/dec
        end if
--      if find(ch,"eE") then   -- you get the idea
--      end if
        token = n
        return
    end if
    if find(ch,"+-/*()^%")=0 then err("syntax error") end if
    token = s[sidx..sidx]
    nxtch(0)
    return
end procedure

procedure Match(string t)
    if token!=t then err(t&" expected") end if
    get_token()
end procedure

procedure PopFactor()
object p2 = opstack[$]
    if op="u-" then
        if op_p_p=1 then                        -- op_p_p
            opstack[$] = {op,0,p2}
        elsif op_p_p=0 then                     -- p_op_p
            opstack[$] = {0,op,p2}
        else -- -1                              -- p_p_op
            opstack[$] = {0,p2,op}
        end if
    else
        opstack = opstack[1..$-1]
        if op_p_p=1 then                        -- op_p_p
            opstack[$] = {op,opstack[$],p2}
        elsif op_p_p=0 then                     -- p_op_p
            opstack[$] = {opstack[$],op,p2}
        else -- -1                              -- p_p_op
            opstack[$] = {opstack[$],p2,op}
        end if
    end if
    op = 0
end procedure

procedure PushFactor(atom t)
    if op!=0 then PopFactor() end if
    opstack = append(opstack,t)
end procedure

procedure PushOp(string t)
    if op!=0 then PopFactor() end if
    op = t
end procedure

procedure Factor()
    if atom(token) then
        PushFactor(token)
        if ch!=-1 then
            get_token()
        end if
    elsif token="-" then
        get_token()
--      Factor()
        Expr(3) -- makes "-3^2" yield -9 (ie -(3^2)) not 9 (ie (-3)^2).
        if op!=0 then PopFactor() end if
        if integer(opstack[$]) then
            opstack[$] = -opstack[$]
        else
            PushOp("u-")
        end if
    elsif token="(" then
        get_token()
        Expr(0)
        Match(")")
    elsif token="+" then -- (ignore)
        nxtch()
        Factor()
    else
        err("syntax error")
    end if
end procedure

constant {operators,
          precedence,
          associativity} = columnize({{"^",3,0},
                                      {"%",2,1},
                                      {"*",2,1},
                                      {"/",2,1},
                                      {"+",1,1},
                                      {"-",1,1},
                                      $})

procedure Expr(integer p)
--
-- Parse an expression, using precedence climbing.
--
-- p is the precedence level we should parse to, eg/ie
--      4: Factor only (may as well just call Factor)
--      3: "" and ^
--      2: "" and *,/,%
--      1: "" and +,-
--      0: full expression (effectively the same as 1)
--  obviously, parentheses override any setting of p.
--
integer k, thisp
    Factor()
    while 1 do
        k = find(token,operators) -- *,/,+,-
        if k=0 then exit end if
        thisp = precedence[k]
        if thisp<p then exit end if
        get_token()
        Expr(thisp+associativity[k])
        PushOp(operators[k])
    end while
end procedure

function eval(object s)
object lhs, rhs
string op
    if atom(s) then
        return s
    end if
    if op_p_p=1 then            -- op_p_p
        {op,lhs,rhs} = s
    elsif op_p_p=0 then         -- p_op_p
        {lhs,op,rhs} = s
    else -- -1                  -- p_p_op
        {lhs,rhs,op} = s
    end if
    if sequence(lhs) then lhs = eval(lhs) end if
    if sequence(rhs) then rhs = eval(rhs) end if
    if op="+" then
        return lhs+rhs
    elsif op="-" then
        return lhs-rhs
    elsif op="*" then
        return lhs*rhs
    elsif op="/" then
        return lhs/rhs
    elsif op="^" then
        return power(lhs,rhs)
    elsif op="%" then
        return remainder(lhs,rhs)
    elsif op="u-" then
        return -rhs
    else
        ?9/0
    end if
end function

s = "3+4+5+6*7/1*5^2^3"
sidx = 0
nxtch()
get_token()
Expr(0)
if op!=0 then PopFactor() end if
if length(opstack)!=1 then err("some error") end if
puts(1,"AST (flat): ")
?opstack[1]
puts(1,"AST (tree):\n")
ppEx(opstack[1],{pp_Nest,6})
puts(1,"result: ")
?eval(opstack[1])
{} = wait_key()
```

I added a flag (for this task) to store the ast nodes as op_p_p, p_op_p, or p_p_op, whichever you prefer.
{{out}}

```txt

with op_p_p:
AST (flat): {"+",{"+",{"+",3,4},5},{"*",{"/",{"*",6,7},1},{"^",5,{"^",2,3}}}}
AST (tree):
{"+",
 {"+",
  {"+",
   3,
   4},
  5},
 {"*",
  {"/",
   {"*",
    6,
    7},
   1},
  {"^",
   5,
   {"^",
    2,
    3}}}}
result: 16406262

with p_op_p:
AST (flat): {{{3,"+",4},"+",5},"+",{{{6,"*",7},"/",1},"*",{5,"^",{2,"^",3}}}}
AST (tree):
{{{3,
   "+",
   4},
  "+",
  5},
 "+",
 {{{6,
    "*",
    7},
   "/",
   1},
  "*",
  {5,
   "^",
   {2,
    "^",
    3}}}}
result: 16406262

and lastly with p_p_op:
16406262
AST (flat): {{{3,4,"+"},5,"+"},{{{6,7,"*"},1,"/"},{5,{2,3,"^"},"^"},"*"},"+"}
AST (tree):
{{{3,
   4,
   "+"},
  5,
  "+"},
 {{{6,
    7,
    "*"},
   1,
   "/"},
  {5,
   {2,
    3,
    "^"},
   "^"},
  "*"},
 "+"}
result: 16406262

```



## PicoLisp

The built-in function 'str' splits a string into a list of lexical tokens
(numbers and transient symbols). From that, a recursive descendent parser can
build an expression tree, resulting in directly executable Lisp code.

```PicoLisp
(de ast (Str)
   (let *L (str Str "")
      (aggregate) ) )

(de aggregate ()
   (let X (product)
      (while (member (car *L) '("+" "-"))
         (setq X (list (intern (pop '*L)) X (product))) )
      X ) )

(de product ()
   (let X (term)
      (while (member (car *L) '("*" "/"))
         (setq X (list (intern (pop '*L)) X (term))) )
      X ) )

(de term ()
   (let X (pop '*L)
      (cond
         ((num? X) X)
         ((= "+" X) (term))
         ((= "-" X) (list '- (term)))
         ((= "(" X) (prog1 (aggregate) (pop '*L)))) ) )
```

{{out}}

```PicoLisp
: (ast "1+2+3*-4/(1+2)")
-> (+ (+ 1 2) (/ (* 3 (- 4)) (+ 1 2)))

: (ast "(1+2+3)*-4/(1+2)")
-> (/ (* (+ (+ 1 2) 3) (- 4)) (+ 1 2))
```



## Pop11



```pop11
/* Scanner routines */
/* Uncomment the following to parse data from standard input

vars itemrep;
incharitem(charin) -> itemrep;

*/

;;; Current symbol
vars sym;

define get_sym();
    itemrep() -> sym;
enddefine;

define expect(x);
    lvars x;
    if x /= sym then
        printf(x, 'Error, expected %p\n');
        mishap(sym, 1, 'Example parser error');
    endif;
    get_sym();
enddefine;

lconstant res_list = [( ) + * ];

lconstant reserved = newproperty(
  maplist(res_list, procedure(x); [^x ^(true)]; endprocedure),
    20, false, "perm");

/*
  Parser for arithmetic expressions
*/
/*
expr: term
   | expr "+" term
   | expr "-" term
   ;
*/

define do_expr() -> result;
    lvars result = do_term(), op;
    while sym = "+" or sym = "-" do
        sym -> op;
        get_sym();
        [^op ^result ^(do_term())] -> result;
    endwhile;
enddefine;

/*
term: factor
   | term "*" factor
   | term "/" factor
   ;
*/

define do_term() -> result;
    lvars result = do_factor(), op;
    while sym = "*" or sym = "/" do
        sym -> op;
        get_sym();
        [^op ^result ^(do_factor())] -> result;
    endwhile;
enddefine;

/*
factor: word
   | constant
   | "(" expr ")"
   ;
*/

define do_factor() -> result;
    if sym = "(" then
        get_sym();
        do_expr() -> result;
        expect(")");
    elseif isinteger(sym) or isbiginteger(sym) then
        sym -> result;
        get_sym();
    else
        if reserved(sym) then
            printf(sym, 'unexpected symbol %p\n');
            mishap(sym, 1, 'Example parser syntax error');
        endif;
        sym -> result;
        get_sym();
    endif;
enddefine;

/* Expression evaluator, returns false on error (currently only
   division by 0 */

define arith_eval(expr);
    lvars op, arg1, arg2;
    if not(expr) then
        return(expr);
    endif;
    if isinteger(expr) or isbiginteger(expr) then
        return(expr);
    endif;
    expr(1) -> op;
    arith_eval(expr(2)) -> arg1;
    arith_eval(expr(3)) -> arg2;
    if not(arg1) or not(arg2) then
        return(false);
    endif;
    if op = "+" then
        return(arg1 + arg2);
    elseif op = "-" then
        return(arg1 - arg2);
    elseif op = "*" then
        return(arg1 * arg2);
    elseif op = "/" then
        if arg2 = 0 then
            return(false);
        else
            return(arg1 div arg2);
        endif;
    else
        printf('Internal error\n');
        return(false);
    endif;
enddefine;

/* Given list, create item repeater.  Input list is stored in a
   closure are traversed when new item is requested. */

define listitemrep(lst);
    procedure();
        lvars item;
        if lst = [] then
            termin;
        else
            front(lst) -> item;
            back(lst) -> lst;
            item;
         endif;
     endprocedure;
enddefine;

/* Initialise scanner */

listitemrep([(3 + 50) * 7 - 100 / 10]) -> itemrep;

get_sym();

;;; Test it
arith_eval(do_expr()) =>
```



## Prolog

{{works with|SWI Prolog}}

```prolog
% Lexer
 numeric(X) :- 48 =< X, X =< 57.
 not_numeric(X) :- 48 > X ; X > 57.

 lex1([], []).
 lex1([40|Xs], ['('|Ys]) :- lex1(Xs, Ys).
 lex1([41|Xs], [')'|Ys]) :- lex1(Xs, Ys).
 lex1([43|Xs], ['+'|Ys]) :- lex1(Xs, Ys).
 lex1([45|Xs], ['-'|Ys]) :- lex1(Xs, Ys).
 lex1([42|Xs], ['*'|Ys]) :- lex1(Xs, Ys).
 lex1([47|Xs], ['/'|Ys]) :- lex1(Xs, Ys).
 lex1([X|Xs], [N|Ys]) :- numeric(X), N is X - 48, lex1(Xs, Ys).

 lex2([], []).
 lex2([X], [X]).
 lex2([Xa,Xb|Xs], [Xa|Ys]) :- atom(Xa), lex2([Xb|Xs], Ys).
 lex2([Xa,Xb|Xs], [Xa|Ys]) :- number(Xa), atom(Xb), lex2([Xb|Xs], Ys).
 lex2([Xa,Xb|Xs], [Y|Ys]) :- number(Xa), number(Xb), N is Xa * 10 + Xb, lex2([N|Xs], [Y|Ys]).

 % Parser
 oper(1, *, X, Y, X * Y). oper(1, /, X, Y, X / Y).
 oper(2, +, X, Y, X + Y). oper(2, -, X, Y, X - Y).

 num(D) --> [D], {number(D)}.

 expr(0, Z) --> num(Z).
 expr(0, Z) --> {Z = (X)}, ['('], expr(2, X), [')'].

 expr(N, Z) --> {succ(N0, N)}, {oper(N, Op, X, Y, Z)}, expr(N0, X), [Op], expr(N, Y).
 expr(N, Z) --> {succ(N0, N)}, expr(N0, Z).

 parse(Tokens, Expr) :- expr(2, Expr, Tokens, []).


 % Evaluator
 evaluate(E, E) :- number(E).
 evaluate(A + B, E) :- evaluate(A, Ae), evaluate(B, Be), E is Ae + Be.
 evaluate(A - B, E) :- evaluate(A, Ae), evaluate(B, Be), E is Ae - Be.
 evaluate(A * B, E) :- evaluate(A, Ae), evaluate(B, Be), E is Ae * Be.
 evaluate(A / B, E) :- evaluate(A, Ae), evaluate(B, Be), E is Ae / Be.

 % Solution
 calculator(String, Value) :-
    lex1(String, Tokens1),
    lex2(Tokens1, Tokens2),
    parse(Tokens2, Expression),
    evaluate(Expression, Value).

 % Example use
 % calculator("(3+50)*7-9", X).
```



## Python

There are python modules, such as Ply, which facilitate the implementation of parsers.  This example, however, uses only standard Python with the parser having two stacks, one for operators, one for operands.

A subsequent example uses Pythons' ast module to generate the abstract syntax tree.


```python
import operator

class AstNode(object):
   def __init__( self, opr, left, right ):
      self.opr = opr
      self.l = left
      self.r = right

   def eval(self):
      return self.opr(self.l.eval(), self.r.eval())

class LeafNode(object):
   def __init__( self, valStrg ):
      self.v = int(valStrg)

   def eval(self):
      return self.v

class Yaccer(object):
   def __init__(self):
      self.operstak = []
      self.nodestak =[]
      self.__dict__.update(self.state1)

   def v1( self, valStrg ):
      # Value String
      self.nodestak.append( LeafNode(valStrg))
      self.__dict__.update(self.state2)
      #print 'push', valStrg

   def o2( self, operchar ):
      # Operator character or open paren in state1
      def openParen(a,b):
         return 0		# function should not be called

      opDict= { '+': ( operator.add, 2, 2 ),
         '-': (operator.sub, 2, 2 ),
         '*': (operator.mul, 3, 3 ),
         '/': (operator.div, 3, 3 ),
         '^': ( pow,         4, 5 ),  # right associative exponentiation for grins
         '(': ( openParen,   0, 8 )
         }
      operPrecidence = opDict[operchar][2]
      self.redeuce(operPrecidence)

      self.operstak.append(opDict[operchar])
      self.__dict__.update(self.state1)
      # print 'pushop', operchar

   def syntaxErr(self, char ):
      # Open Parenthesis
      print 'parse error - near operator "%s"' %char

   def pc2( self,operchar ):
      # Close Parenthesis
      # reduce node until matching open paren found
      self.redeuce( 1 )
      if len(self.operstak)>0:
         self.operstak.pop()		# pop off open parenthesis
      else:
         print 'Error - no open parenthesis matches close parens.'
      self.__dict__.update(self.state2)

   def end(self):
      self.redeuce(0)
      return self.nodestak.pop()

   def redeuce(self, precidence):
      while len(self.operstak)>0:
         tailOper = self.operstak[-1]
         if tailOper[1] < precidence: break

         tailOper = self.operstak.pop()
         vrgt = self.nodestak.pop()
         vlft= self.nodestak.pop()
         self.nodestak.append( AstNode(tailOper[0], vlft, vrgt))
         # print 'reduce'

   state1 = { 'v': v1, 'o':syntaxErr, 'po':o2, 'pc':syntaxErr }
   state2 = { 'v': syntaxErr, 'o':o2, 'po':syntaxErr, 'pc':pc2 }


def Lex( exprssn, p ):
   bgn = None
   cp = -1
   for c in exprssn:
      cp += 1
      if c in '+-/*^()':         # throw in exponentiation (^)for grins
         if bgn is not None:
            p.v(p, exprssn[bgn:cp])
            bgn = None
         if c=='(': p.po(p, c)
         elif c==')':p.pc(p, c)
         else: p.o(p, c)
      elif c in ' \t':
         if bgn is not None:
            p.v(p, exprssn[bgn:cp])
            bgn = None
      elif c in '0123456789':
         if bgn is None:
            bgn = cp
      else:
         print 'Invalid character in expression'
         if bgn is not None:
            p.v(p, exprssn[bgn:cp])
            bgn = None

   if bgn is not None:
      p.v(p, exprssn[bgn:cp+1])
      bgn = None
   return p.end()


expr = raw_input("Expression:")
astTree = Lex( expr, Yaccer())
print expr, '=',astTree.eval()
```



### ast standard library module

Python comes with its own [http://docs.python.org/3.1/library/ast.html#module-ast ast] module as part of its standard libraries. The module compiles Python source into an AST tree that can in turn be compiled to bytecode then executed.

```python>>>
 import ast
>>>
>>> expr="2 * (3 -1) + 2 * 5"
>>> node = ast.parse(expr, mode='eval')
>>> print(ast.dump(node).replace(',', ',\n'))
Expression(body=BinOp(left=BinOp(left=Num(n=2),
 op=Mult(),
 right=BinOp(left=Num(n=3),
 op=Sub(),
 right=Num(n=1))),
 op=Add(),
 right=BinOp(left=Num(n=2),
 op=Mult(),
 right=Num(n=5))))
>>> code_object = compile(node, filename='<string>', mode='eval')
>>> eval(code_object)
14
>>> # lets modify the AST by changing the 5 to a 6
>>> node.body.right.right.n
5
>>> node.body.right.right.n = 6
>>> code_object = compile(node, filename='<string>', mode='eval')
>>> eval(code_object)
16
```



## Racket



```racket
#lang racket

(require parser-tools/yacc
         parser-tools/lex
         (prefix-in ~ parser-tools/lex-sre))

(define-tokens value-tokens (NUM))
(define-empty-tokens op-tokens (OPEN CLOSE + - * / EOF NEG))

(define lex
  (lexer [(eof) 'EOF]
         [whitespace (lex input-port)]
         [(~or "+" "-" "*" "/") (string->symbol lexeme)]
         ["(" 'OPEN]
         [")" 'CLOSE]
         [(~: (~+ numeric) (~? (~: #\. (~* numeric))))
          (token-NUM (string->number lexeme))]))

(define parse
  (parser [start E] [end EOF]
          [tokens value-tokens op-tokens]
          [error void]
          [precs (left - +) (left * /) (left NEG)]
          [grammar (E [(NUM) $1]
                      [(E + E) (+ $1 $3)]
                      [(E - E) (- $1 $3)]
                      [(E * E) (* $1 $3)]
                      [(E / E) (/ $1 $3)]
                      [(- E) (prec NEG) (- $2)]
                      [(OPEN E CLOSE) $2])]))

(define (calc str)
  (define i (open-input-string str))
  (displayln (parse (λ () (lex i)))))

(calc "(1 + 2 * 3) - (1+2)*-3")
```



## REXX

Several additional operators are supported as well as several forms of exponentiated numbers:
:::*   '''^'''       exponentiation,   as well as   '''**'''
:::*   '''//'''       remainder division
:::*   '''%'''     integer division
:::*   '''<big>÷</big>'''       in addition to   '''<big>/</big>'''
:::*   '''&amp;'''     for logical   '''AND'''
:::*   '''|'''       for logical   '''OR'''
:::*   '''&amp;&amp;'''   for logical   '''XOR'''
:::*   '''||'''       for concatenation
:::*   '''[   ]     {   }'''     as grouping symbols,   as well as   '''(   )'''
:::*   12.3e+44       ("single" precision)
:::*   12.3E+44       ("single" precision)
:::*   12.3D+44       ("double" precision)
:::*   12.3Q+44       ("extended" or "quad" precision)

```rexx
/*REXX program  evaluates an  infix─type arithmetic expression  and displays the result.*/
nchars = '0123456789.eEdDqQ'                     /*possible parts of a number,  sans  ± */
e='***error***';    $=" ";     doubleOps= '&|*/';      z=       /*handy─dandy variables.*/
parse arg x 1 ox1;    if x=''  then call serr "no input was specified."
x=space(x);   L=length(x);     x=translate(x, '()()', "[]{}")
j=0
     do forever;    j=j+1;     if j>L  then leave;    _=substr(x, j, 1);   _2=getX()
     newT=pos(_,' ()[]{}^÷')\==0;  if newT  then do;  z=z _ $;  iterate;   end
     possDouble=pos(_,doubleOps)\==0             /*is    _   a possible double operator?*/
     if possDouble  then do                      /* "  this  "     "       "       "    */
                         if _2==_  then do       /*yupper, it's one of a double operator*/
                                        _=_ || _ /*create and use a double char operator*/
                                        x=overlay($, x, Nj)      /*blank out 2nd symbol.*/
                                        end
                         z=z _ $;  iterate
                         end
     if _=='+' | _=="-"  then do;  p_=word(z, max(1,words(z)))   /*last  Z  token.      */
                                   if p_=='('   then z=z 0       /*handle a unary ±     */
                                   z=z _ $;     iterate
                              end
     lets=0;  sigs=0;  #=_

            do j=j+1  to L;   _=substr(x,j,1)                    /*build a valid number.*/
            if lets==1 & sigs==0 then if _=='+' | _=="-"  then do;  sigs=1
                                                                    #=# || _
                                                                    iterate
                                                               end
            if pos(_,nchars)==0  then leave
            lets=lets+datatype(_,'M')            /*keep track of the number of exponents*/
            #=# || translate(_,'EEEEE', "eDdQq") /*keep building the number.            */
            end   /*j*/
     j=j-1
     if \datatype(#,'N')  then call  serr  "invalid number: "     #
     z=z # $
     end   /*forever*/

_=word(z,1);      if _=='+' | _=="-"  then z=0 z /*handle the unary cases.              */
x='(' space(z) ")";    tokens=words(x)           /*force stacking for the expression.   */
  do i=1  for tokens;  @.i=word(x,i);  end /*i*/ /*assign input tokens.                 */
L=max(20,length(x))                              /*use 20 for the minimum display width.*/
op= ')(-+/*^';    Rop=substr(op,3);     p.=;     s.=;     n=length(op);    epr=;    stack=

  do i=1  for n;  _=substr(op,i,1);     s._=(i+1)%2;     p._=s._ + (i==n);      end  /*i*/
                                                 /* [↑]  assign the operator priorities.*/
  do #=1  for tokens;   ?=@.#                    /*process each token from the  @. list.*/
  if ?=='**'      then ?="^"                     /*convert to REXX-type exponentiation. */
     select                                      /*@.#  is:   (   operator   )   operand*/
     when ?=='('  then stack="(" stack
     when isOp(?) then do                        /*is the token an operator ?           */
                       !=word(stack,1)           /*get token from stack.*/
                         do  while !\==')' & s.!>=p.?;  epr=epr !            /*addition.*/
                         stack=subword(stack, 2)                  /*del token from stack*/
                         !=       word(stack, 1)                  /*get token from stack*/
                         end   /*while*/
                       stack=? stack                              /*add token  to  stack*/
                       end
     when ?==')' then do;   !=word(stack, 1)                      /*get token from stack*/
                         do  while !\=='(';             epr=epr ! /*append to expression*/
                         stack=subword(stack, 2)                  /*del token from stack*/
                         !=       word(stack, 1)                  /*get token from stack*/
                        end   /*while*/
                      stack=subword(stack, 2)                     /*del token from stack*/
                      end
    otherwise  epr=epr ?                                          /*add operand to  epr.*/
    end   /*select*/
  end     /*#*/

epr=space(epr stack);     tokens=words(epr);     x=epr;     z=;     stack=
  do i=1  for tokens; @.i=word(epr,i);  end /*i*/                 /*assign input tokens.*/
Dop='/ // % ÷';           Bop="& | &&"           /*division   operands; binary operands.*/
Aop='- + * ^ **' Dop Bop; Lop=Aop "||"           /*arithmetic operands; legal  operands.*/

  do #=1  for tokens;   ?=@.#;   ??=?            /*process each token from   @.  list.  */
  w=words(stack);  b=word(stack, max(1, w  ) )   /*stack count;  the last entry.        */
                   a=word(stack, max(1, w-1) )   /*stack's  "first"  operand.           */
  division  =wordpos(?, Dop)\==0                 /*flag:  doing a division operation.   */
  arith     =wordpos(?, Aop)\==0                 /*flag:  doing arithmetic operation.   */
  bitOp     =wordpos(?, Bop)\==0                 /*flag:  doing binary mathematics.     */
  if datatype(?, 'N')  then do; stack=stack ?;                iterate; end
  if wordpos(?,Lop)==0 then do;  z=e  "illegal operator:" ?;        leave; end
  if w<2               then do;  z=e  "illegal epr expression.";    leave; end
  if ?=='^'            then ??="**"              /*REXXify   ^ ──► **   (make it legal).*/
  if ?=='÷'            then ??="/"               /*REXXify   ÷ ──► /    (make it legal).*/
  if division  &  b=0  then do;  z=e  "division by zero"        b;  leave; end
  if bitOp & \isBit(a) then do;  z=e  "token isn't logical: "   a;  leave; end
  if bitOp & \isBit(b) then do;  z=e  "token isn't logical: "   b;  leave; end
             select                              /*perform an arithmetic operation.     */
             when ??=='+'             then y = a +  b
             when ??=='-'             then y = a -  b
             when ??=='*'             then y = a *  b
             when ??=='/' | ??=="÷"   then y = a /  b
             when ??=='//'            then y = a // b
             when ??=='%'             then y = a %  b
             when ??=='^' | ??=="**"  then y = a ** b
             when ??=='||'            then y = a || b
             otherwise              z=e 'invalid operator:' ?;         leave
             end   /*select*/
  if datatype(y, 'W')   then y=y/1               /*normalize the number with  ÷  by  1. */
  _=subword(stack, 1, w-2);  stack=_ y           /*rebuild the stack with the answer.   */
  end   /*#*/

if word(z, 1)==e  then stack=                    /*handle the special case of errors.   */
z=space(z stack)                                 /*append any residual entries.         */
say 'answer──►'   z                              /*display the answer  (result).        */
parse source upper . how .                       /*invoked via  C.L.  or REXX program ? */
if how=='COMMAND' | \datatype(z, 'W')  then exit /*stick a fork in it,  we're all done. */
return z                                         /*return  Z ──► invoker  (the RESULT). */
/*──────────────────────────────────────────────────────────────────────────────────────*/
isBit: return arg(1)==0 | arg(1) == 1            /*returns  1  if 1st argument is binary*/
isOp:  return pos(arg(1), rOp) \== 0             /*is argument 1 a  "real"  operator?   */
serr:  say;   say e arg(1);   say;   exit 13     /*issue an error message with some text*/
/*──────────────────────────────────────────────────────────────────────────────────────*/
getX:            do Nj=j+1  to length(x);   _n=substr(x, Nj, 1);    if _n==$  then iterate
                 return  substr(x, Nj, 1)        /* [↑]  ignore any blanks in expression*/
                 end   /*Nj*/
       return $                                  /*reached end-of-tokens,  return $.    */
```

To view a version of the above REXX program, see this version which has much more whitespace:   ──►   [[Arithmetic_evaluation/REXX]].




'''output'''   when using the input of:   <tt> + 1+2.0-003e-00*[4/6] </tt>

```txt

answer──► 1

```



## Ruby

Function to convert infix arithmetic expression to binary tree. The resulting tree knows how to print and evaluate itself. Assumes expression is well-formed (matched parens, all operators have 2 operands). Algorithm: http://www.seas.gwu.edu/~csci131/fall96/exp_to_tree.html

```ruby
$op_priority = {"+" => 0, "-" => 0, "*" => 1, "/" => 1}

class TreeNode
  OP_FUNCTION = {
    "+" => lambda {|x, y| x + y},
    "-" => lambda {|x, y| x - y},
    "*" => lambda {|x, y| x * y},
    "/" => lambda {|x, y| x / y}}
  attr_accessor :info, :left, :right

  def initialize(info)
    @info = info
  end

  def leaf?
    @left.nil? and @right.nil?
  end

  def to_s(order)
    if leaf?
      @info
    else
      left_s, right_s = @left.to_s(order), @right.to_s(order)

      strs = case order
             when :prefix  then [@info, left_s, right_s]
             when :infix   then [left_s, @info, right_s]
             when :postfix then [left_s, right_s, @info]
             else               []
             end

      "(" + strs.join(" ") + ")"
    end
  end

  def eval
    if !leaf? and operator?(@info)
      OP_FUNCTION[@info].call(@left.eval, @right.eval)
    else
      @info.to_f
    end
  end
end

def tokenize(exp)
  exp
    .gsub('(', ' ( ')
    .gsub(')', ' ) ')
    .gsub('+', ' + ')
    .gsub('-', ' - ')
    .gsub('*', ' * ')
    .gsub('/', ' / ')
    .split(' ')
end

def operator?(token)
  $op_priority.has_key?(token)
end

def pop_connect_push(op_stack, node_stack)
  temp = op_stack.pop
  temp.right = node_stack.pop
  temp.left = node_stack.pop
  node_stack.push(temp)
end

def infix_exp_to_tree(exp)
  tokens = tokenize(exp)
  op_stack, node_stack = [], []

  tokens.each do |token|
    if operator?(token)
      # clear stack of higher priority operators
      until (op_stack.empty? or
             op_stack.last.info == "(" or
             $op_priority[op_stack.last.info] < $op_priority[token])
        pop_connect_push(op_stack, node_stack)
      end

      op_stack.push(TreeNode.new(token))
    elsif token == "("
      op_stack.push(TreeNode.new(token))
    elsif token == ")"
      while op_stack.last.info != "("
        pop_connect_push(op_stack, node_stack)
      end

      # throw away the '('
      op_stack.pop
    else
      node_stack.push(TreeNode.new(token))
    end
  end

  until op_stack.empty?
    pop_connect_push(op_stack, node_stack)
  end

  node_stack.last
end
```

Testing:

```ruby
exp = "1 + 2 - 3 * (4 / 6)"
puts("Original: " + exp)

tree = infix_exp_to_tree(exp)
puts("Prefix: " + tree.to_s(:prefix))
puts("Infix: " + tree.to_s(:infix))
puts("Postfix: " + tree.to_s(:postfix))
puts("Result: " + tree.eval.to_s)
```

{{out}}

```txt
Original: 1 + 2 - 3 * (4 / 6)
Prefix: (- (+ 1 2) (* 3 (/ 4 6)))
Infix: ((1 + 2) - (3 * (4 / 6)))
Postfix: ((1 2 +) (3 (4 6 /) *) -)
Result: 1.0
```



## Rust


```rust
//! Simple calculator parser and evaluator


/// Binary operator
#[derive(Debug)]
pub enum Operator {
    Add,
    Substract,
    Multiply,
    Divide
}

/// A node in the tree
#[derive(Debug)]
pub enum Node {
    Value(f64),
    SubNode(Box<Node>),
    Binary(Operator, Box<Node>,Box<Node>),
}

/// parse a string into a node
pub fn parse(txt :&str) -> Option<Node> {
    let chars = txt.chars().filter(|c| *c != ' ').collect();
    parse_expression(&chars, 0).map(|(_,n)| n)
}

/// parse an expression into a node, keeping track of the position in the character vector
fn parse_expression(chars: &Vec<char>, pos: usize) -> Option<(usize,Node)> {
    match parse_start(chars, pos) {
        Some((new_pos, first)) => {
            match parse_operator(chars, new_pos) {
                Some((new_pos2,op)) => {
                    if let Some((new_pos3, second)) = parse_expression(chars, new_pos2) {
                        Some((new_pos3, combine(op, first, second)))
                    } else {
                        None
                    }
                },
                None => Some((new_pos,first)),
            }
        },
        None => None,
    }
}

/// combine nodes to respect associativity rules
fn combine(op: Operator, first: Node, second: Node) -> Node {
    match second {
        Node::Binary(op2,v21,v22) => if precedence(&op)>=precedence(&op2) {
            Node::Binary(op2,Box::new(combine(op,first,*v21)),v22)
        } else {
            Node::Binary(op,Box::new(first),Box::new(Node::Binary(op2,v21,v22)))
        },
        _ => Node::Binary(op,Box::new(first),Box::new(second)),
    }
}

/// a precedence rank for operators
fn precedence(op: &Operator) -> usize {
    match op{
        Operator::Multiply | Operator::Divide => 2,
        _ => 1
    }
}

/// try to parse from the start of an expression (either a parenthesis or a value)
fn parse_start(chars: &Vec<char>, pos: usize) -> Option<(usize,Node)> {
    match start_parenthesis(chars, pos){
        Some (new_pos) => {
            let r = parse_expression(chars, new_pos);
            end_parenthesis(chars, r)
        },
        None => parse_value(chars, pos),
    }
}

/// match a starting parentheseis
fn start_parenthesis(chars: &Vec<char>, pos: usize) -> Option<usize>{
    if pos<chars.len() && chars[pos] == '(' {
        Some(pos+1)
    } else {
        None
    }
}

/// match an end parenthesis, if successful will create a sub node contained the wrapped expression
fn end_parenthesis(chars: &Vec<char>, wrapped :Option<(usize,Node)>) -> Option<(usize,Node)>{
    match wrapped {
        Some((pos, node)) => if pos<chars.len() && chars[pos] == ')' {
                Some((pos+1,Node::SubNode(Box::new(node))))
            } else {
                None
            },
        None => None,
    }
}

/// parse a value: an decimal with an optional minus sign
fn parse_value(chars: &Vec<char>, pos: usize) -> Option<(usize,Node)>{
    let mut new_pos = pos;
    if new_pos<chars.len() && chars[new_pos] == '-' {
        new_pos = new_pos+1;
    }
    while new_pos<chars.len() && (chars[new_pos]=='.' || (chars[new_pos] >= '0' && chars[new_pos] <= '9')) {
        new_pos = new_pos+1;
    }
    if new_pos>pos {
        if let Ok(v) = dbg!(chars[pos..new_pos].iter().collect::<String>()).parse() {
            Some((new_pos,Node::Value(v)))
        } else {
            None
        }
    } else {
        None
    }

}

/// parse an operator
fn parse_operator(chars: &Vec<char>, pos: usize) -> Option<(usize,Operator)> {
    if pos<chars.len() {
        let ops_with_char = vec!(('+',Operator::Add),('-',Operator::Substract),('*',Operator::Multiply),('/',Operator::Divide));
        for (ch,op) in ops_with_char {
            if chars[pos] == ch {
                return Some((pos+1, op));
            }
        }
    }
    None
}

/// eval a string
pub fn eval(txt :&str) -> f64 {
    match parse(txt) {
        Some(t) => eval_term(&t),
        None => panic!("Cannot parse {}",txt),
    }

}

/// eval a term, recursively
fn eval_term(t: &Node) -> f64 {
    match t {
        Node::Value(v) => *v,
        Node::SubNode(t) => eval_term(t),
        Node::Binary(Operator::Add,t1,t2) => eval_term(t1) + eval_term(t2),
        Node::Binary(Operator::Substract,t1,t2) => eval_term(t1) - eval_term(t2),
        Node::Binary(Operator::Multiply,t1,t2) => eval_term(t1) * eval_term(t2),
        Node::Binary(Operator::Divide,t1,t2) => eval_term(t1) / eval_term(t2),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_eval(){
        assert_eq!(2.0,eval("2"));
        assert_eq!(4.0,eval("2+2"));
        assert_eq!(11.0/4.0, eval("2+3/4"));
        assert_eq!(2.0, eval("2*3-4"));
        assert_eq!(3.0, eval("1+2*3-4"));
        assert_eq!(89.0/6.0, eval("2*(3+4)+5/6"));
        assert_eq!(14.0, eval("2 * (3 -1) + 2 * 5"));
        assert_eq!(7000.0, eval("2 * (3 + (4 * 5 + (6 * 7) * 8) - 9) * 10"));
        assert_eq!(-9.0/4.0, eval("2*-3--4+-.25"));
        assert_eq!(1.5, eval("1 - 5 * 2 / 20 + 1"));
        assert_eq!(3.5, eval("2 * (3 + ((5) / (7 - 11)))"));

    }
}


```



## Scala

This code shows a bit of Scala's parser classes. The error handling of parser errors
is practically non-existent, to avoid obscuring the code.


```scala

package org.rosetta.arithmetic_evaluator.scala

object ArithmeticParser extends scala.util.parsing.combinator.RegexParsers {

  def readExpression(input: String) : Option[()=>Int] = {
    parseAll(expr, input) match {
      case Success(result, _) =>
        Some(result)
      case other =>
        println(other)
        None
    }
  }

  private def expr : Parser[()=>Int] = {
    (term<~"+")~expr ^^ { case l~r => () => l() + r() } |
    (term<~"-")~expr ^^ { case l~r => () => l() - r() } |
    term
  }

  private def term : Parser[()=>Int] = {
    (factor<~"*")~term ^^ { case l~r => () => l() * r() } |
    (factor<~"/")~term ^^ { case l~r => () => l() / r() } |
    factor
  }

  private def factor : Parser[()=>Int] = {
    "("~>expr<~")" |
    "\\d+".r ^^ { x => () => x.toInt } |
    failure("Expected a value")
  }
}

object Main {
  def main(args: Array[String]) {
    println("""Please input the expressions. Type "q" to quit.""")
    var input: String = ""

    do {
      input = readLine("> ")
      if (input != "q") {
        ArithmeticParser.readExpression(input).foreach(f => println(f()))
      }
    } while (input != "q")
  }
}

```


Example:


```txt

C:\Workset>scala org.rosetta.arithmetic_evaluator.scala.ArithmeticEvaluator
Please input the expressions. Type "q" to quit.
> 2+3*2
8
> (1+3)*7
28
> 1+a
[1.3] failure: Expected a number

1+a
  ^
> 2 + 2
4
> q

```


This example was made rather more complex by the requirement of generating an AST tree. With a Scala distribution there are many examples of arithmetic parsers, as small as half a dozen lines.


## Scheme


This works in three stages: string->tokens turns the input string into a list of tokens, parse converts this into an AST, which is eventually evaluated into a number result.  Only positive integers are read, though output can be a rational, positive or negative.

The parse function uses a recursive-descent parser to follow the precedence rules.


```scheme

(import (scheme base)
        (scheme char)
        (scheme cxr)
        (scheme write)
        (srfi 1 lists))

;; convert a string into a list of tokens
(define (string->tokens str)
  (define (next-token chars)
    (cond ((member (car chars) (list #\+ #\- #\* #\/) char=?)
           (values (cdr chars)
                   (cdr (assq (car chars) ; convert char for op into op procedure, using a look up list
                              (list (cons #\+ +) (cons #\- -) (cons #\* *) (cons #\/ /))))))
          ((member (car chars) (list #\( #\)) char=?)
           (values (cdr chars)
                   (if (char=? (car chars) #\()
                     'open
                     'close)))
          (else ; read a multi-digit positive integer
            (let loop ((rem chars)
                       (res 0))
              (if (and (not (null? rem))
                       (char-numeric? (car rem)))
                (loop (cdr rem)
                      (+ (* res 10)
                         (- (char->integer (car rem))
                            (char->integer #\0))))
                (values rem
                        res))))))
  ;
  (let loop ((chars (remove char-whitespace? (string->list str)))
             (tokens '()))
    (if (null? chars)
      (reverse tokens)
      (let-values (((remaining-chars token) (next-token chars)))
                  (loop remaining-chars
                        (cons token tokens))))))

;; turn list of tokens into an AST
;; -- using recursive descent parsing to obey laws of precedence
(define (parse tokens)
  (define (parse-factor tokens)
    (if (number? (car tokens))
      (values (car tokens) (cdr tokens))
      (let-values (((expr rem) (parse-expr (cdr tokens))))
                  (values expr (cdr rem)))))
  (define (parse-term tokens)
    (let-values (((left-expr rem) (parse-factor tokens)))
                (if (and (not (null? rem))
                         (member (car rem) (list * /)))
                  (let-values (((right-expr remr) (parse-term (cdr rem))))
                              (values (list (car rem) left-expr right-expr)
                                      remr))
                  (values left-expr rem))))
  (define (parse-part tokens)
    (let-values (((left-expr rem) (parse-term tokens)))
                (if (and (not (null? rem))
                         (member (car rem) (list + -)))
                  (let-values (((right-expr remr) (parse-part (cdr rem))))
                              (values (list (car rem) left-expr right-expr)
                                      remr))
                  (values left-expr rem))))
  (define (parse-expr tokens)
    (let-values (((expr rem) (parse-part tokens)))
                (values expr rem)))
  ;
  (let-values (((expr rem) (parse-expr tokens)))
                (if (null? rem)
                  expr
                  (error "Misformed expression"))))

;; evaluate the AST, returning a number
(define (eval-expression ast)
  (cond ((number? ast)
         ast)
        ((member (car ast) (list + - * /))
         ((car ast)
          (eval-expression (cadr ast))
          (eval-expression (caddr ast))))
        (else
          (error "Misformed expression"))))

;; parse and evaluate the given string
(define (interpret str)
  (eval-expression (parse (string->tokens str))))

;; running some examples
(for-each
  (lambda (str)
    (display
      (string-append str
                     " => "
                     (number->string (interpret str))))
    (newline))
  '("1 + 2" "20+4*5" "1/2+5*(6-3)" "(1+3)/4-1" "(1 - 5) * 2 / (20 + 1)"))

```


{{out}}


```txt

1 + 2 => 3
20+4*5 => 40
1/2+5*(6-3) => 31/2
(1+3)/4-1 => 0
(1 - 5) * 2 / (20 + 1) => -8/21

```



## Sidef

{{trans|JavaScript}}

```ruby
func evalArithmeticExp(s) {

    func evalExp(s) {

        func operate(s, op) {
           s.split(op).map{|c| Number(c) }.reduce(op)
        }

        func add(s) {
            operate(s.sub(/^\+/,'').sub(/\++/,'+'), '+')
        }

        func subtract(s) {
            s.gsub!(/(\+-|-\+)/,'-')

            if (s ~~ /--/) {
                return(add(s.sub(/--/,'+')))
            }

            var b = s.split('-')
            b.len == 3 ? (-1*Number(b[1]) - Number(b[2]))
                       : operate(s, '-')
        }

        s.gsub!(/[()]/,'').gsub!(/-\+/, '-')

        var reM  = /\*/
        var reMD = %r"(\d+\.?\d*\s*[*/]\s*[+-]?\d+\.?\d*)"

        var reA  = /\d\+/
        var reAS = /(-?\d+\.?\d*\s*[+-]\s*[+-]?\d+\.?\d*)/

        while (var match = reMD.match(s)) {
            match[0] ~~ reM
                ? s.sub!(reMD, operate(match[0], '*').to_s)
                : s.sub!(reMD, operate(match[0], '/').to_s)
        }

        while (var match = reAS.match(s)) {
            match[0] ~~ reA
                ? s.sub!(reAS,      add(match[0]).to_s)
                : s.sub!(reAS, subtract(match[0]).to_s)
        }

        return s
    }

    var rePara = /(\([^\(\)]*\))/
    s.split!.join!('').sub!(/^\+/,'')

    while (var match = s.match(rePara)) {
        s.sub!(rePara, evalExp(match[0]))
    }

    return Number(evalExp(s))
}
```


Testing the function:

```ruby
for expr,res in [
     ['2+3'                                      =>        5],
     ['-4-3'                                     =>       -7],
     ['-+2+3/4'                                  =>    -1.25],
     ['2*3-4'                                    =>        2],
     ['2*(3+4)+2/4'                              => 2/4 + 14],
     ['2*-3--4+-0.25'                            =>    -2.25],
     ['2 * (3 + (4 * 5 + (6 * 7) * 8) - 9) * 10' =>     7000],
] { 
    var num = evalArithmeticExp(expr)
    assert_eq(num, res)
    "%-45s == %10g\n".printf(expr, num)
}
```



## Standard ML

This implementation uses a [https://en.wikipedia.org/wiki/Recursive_descent_parser recursive descent parser]. It first lexes the input. The parser builds a Abstract Syntax Tree (AST) and the evaluator evaluates it. The parser uses sub categories.
The parsing is a little bit tricky because the grammar is left recursive.

```sml
(* AST *)
datatype expression =
	  Con of int				(* constant *)
	| Add of expression * expression	(* addition *)
	| Mul of expression * expression	(* multiplication *)
	| Sub of expression * expression	(* subtraction *)
	| Div of expression * expression	(* division *)

(* Evaluator *)
fun eval (Con x)      = x
  | eval (Add (x, y)) = (eval x)  +  (eval y)
  | eval (Mul (x, y)) = (eval x)  *  (eval y)
  | eval (Sub (x, y)) = (eval x)  -  (eval y)
  | eval (Div (x, y)) = (eval x) div (eval y)

(* Lexer *)
datatype token =
	  CON of int
	| ADD
	| MUL
	| SUB
	| DIV
	| LPAR
	| RPAR

fun lex nil = nil
  | lex (#"+" :: cs) = ADD :: lex cs
  | lex (#"*" :: cs) = MUL :: lex cs
  | lex (#"-" :: cs) = SUB :: lex cs
  | lex (#"/" :: cs) = DIV :: lex cs
  | lex (#"(" :: cs) = LPAR :: lex cs
  | lex (#")" :: cs) = RPAR :: lex cs
  | lex (#"~" :: cs) = if null cs orelse not (Char.isDigit (hd cs)) then raise Domain
                       else lexDigit (0, cs, ~1)
  | lex (c    :: cs) = if Char.isDigit c then lexDigit (0, c :: cs, 1)
                       else raise Domain

and lexDigit (a, cs, s) = if null cs orelse not (Char.isDigit (hd cs)) then CON (a*s) :: lex cs
                          else lexDigit (a * 10 + (ord (hd cs))- (ord #"0") , tl cs, s)

(* Parser *)
exception Error of string

fun match (a,ts) t = if null ts orelse hd ts <> t
                     then raise Error "match"
		     else (a, tl ts)

fun extend (a,ts) p f = let val (a',tr) = p ts in (f(a,a'), tr) end

fun parseE  ts             = parseE' (parseM ts)
and parseE' (e, ADD :: ts) = parseE' (extend (e, ts) parseM Add)
  | parseE' (e, SUB :: ts) = parseE' (extend (e, ts) parseM Sub)
  | parseE' s = s

and parseM  ts             = parseM' (parseP ts)
and parseM' (e, MUL :: ts) = parseM' (extend (e, ts) parseP Mul)
  | parseM' (e, DIV :: ts) = parseM' (extend (e, ts) parseP Div)
  | parseM' s = s

and parseP (CON c :: ts) = (Con c, ts)
  | parseP (LPAR  :: ts) = match (parseE ts) RPAR
  | parseP _ = raise Error "parseP"


(* Test *)
fun lex_parse_eval (str:string) =
	case parseE (lex (explode str)) of
	   (exp, nil) => eval exp
	 | _          => raise Error "not parseable stuff at the end"
```



## Tcl

{{works with|Tcl|8.5}}
The code below delivers the AST for an expression
in a form that it can be immediately eval-led,
using Tcl's prefix operators.

```Tcl
namespace import tcl::mathop::*

proc ast str {
    # produce abstract syntax tree for an expression
    regsub -all {[-+*/()]} $str { & } str ;# "tokenizer"
    s $str
}
proc s {args} {
    # parse "(a + b) * c + d" to "+ [* [+ a b] c] d"
    if {[llength $args] == 1} {set args [lindex $args 0]}
    if [regexp {[()]} $args] {
        eval s [string map {( "\[s " ) \]} $args]
    } elseif {"*" in $args} {
	s [s_group $args *]
    } elseif {"/" in $args} {
	s [s_group $args /]
    } elseif {"+" in $args} {
        s [s_group $args +]
    } elseif {"-" in $args} {
        s [s_group $args -]
    } else {
        string map {\{ \[ \} \]} [join $args]
    }
}
proc s_group {list op} {
    # turn ".. a op b .." to ".. {op a b} .."
    set pos [lsearch -exact $list $op]
    set p_1 [- $pos 1]
    set p1  [+ $pos 1]
    lreplace $list $p_1 $p1 \
                  [list $op [lindex $list $p_1] [lindex $list $p1]]
}
#-- Test suite
foreach test [split {
    ast 2-2
    ast 1-2-3
    ast (1-2)-3
    ast 1-(2-3)
    ast (1+2)*3
    ast (1+2)/3-4*5
    ast ((1+2)/3-4)*5
} \n] {
    puts "$test ..... [eval $test] ..... [eval [eval $test]]"
}
```

{{out}}

```txt

    ast 2-2 ..... - 2 2 ..... 0
    ast 1-2-3 ..... - [- 1 2] 3 ..... -4
    ast (1-2)-3 ..... - [- 1 2] 3 ..... -4
    ast 1-(2-3) ..... - 1 [- 2 3] ..... 2
    ast (1+2)*3 ..... * [+ 1 2] 3 ..... 9
    ast (1+2)/3-4*5 ..... - [/ [+ 1 2] 3] [* 4 5] ..... -19
    ast ((1+2)/3-4)*5 ..... * [- [/ [+ 1 2] 3] 4] 5 ..... -15

```



## TXR


Use TXR text pattern matching to parse expression to a Lisp AST, then evaluate with <code>eval</code>:


```txr
@(next :args)
@(define space)@/ */@(end)
@(define mulop (nod))@\
   @(local op)@\
   @(space)@\
   @(cases)@\
     @{op /[*]/}@(bind nod @(intern op *user-package*))@\
   @(or)@\
     @{op /\//}@(bind (nod) @(list 'trunc))@\
   @(end)@\
   @(space)@\
@(end)
@(define addop (nod))@\
   @(local op)@(space)@{op /[+\-]/}@(space)@\
   @(bind nod @(intern op *user-package*))@\
@(end)
@(define number (nod))@\
  @(local n)@(space)@{n /[0-9]+/}@(space)@\
  @(bind nod @(int-str n 10))@\
@(end)
@(define factor (nod))@(cases)(@(expr nod))@(or)@(number nod)@(end)@(end)
@(define term (nod))@\
  @(local op nod1 nod2)@\
  @(cases)@\
    @(factor nod1)@\
    @(cases)@(mulop op)@(term nod2)@(bind nod (op nod1 nod2))@\
    @(or)@(bind nod nod1)@\
    @(end)@\
  @(or)@\
    @(addop op)@(factor nod1)@\
    @(bind nod (op nod1))@\
  @(end)@\
@(end)
@(define expr (nod))@\
  @(local op nod1 nod2)@\
  @(term nod1)@\
  @(cases)@(addop op)@(expr nod2)@(bind nod (op nod1 nod2))@\
  @(or)@(bind nod nod1)@\
  @(end)@\
@(end)
@(cases)
@  {source (expr e)}
@  (output)
source: @source
AST:    @(format nil "~s" e)
value:  @(eval e nil)
@  (end)
@(or)
@  (maybe)@(expr e)@(end)@bad
@  (output)
erroneous suffix "@bad"
@  (end)
@(end)
```


Run:


```txt
$  txr expr-ast.txr '3 + 3/4 * (2 + 2) + (4*4)'
source: 3 + 3/4 * (2 + 2) + (4*4)
AST:    (+ 3 (+ (trunc 3 (* 4 (+ 2 2))) (* 4 4)))
value:  19
```



{{omit from|gnuplot}}


## Ursala

with no error checking other than removal of spaces

```Ursala
#import std
#import nat
#import flo

lex = ~=' '*~F+ rlc both -=digits    # separate into tokens

parse = # build a tree

--<';'>; @iNX ~&l->rh ^/~&lt cases~&lhh\~&lhPNVrC {
   '*/': ^|C/~&hNV associate '*/',
   '+-': ^|C/~&hNV associate '*/+-',
   ');': @r ~&htitBPC+ associate '*/+-'}

associate "ops" = ~&tihdh2B-="ops"-> ~&thd2tth2hNCCVttt2C

traverse = *^ ~&v?\%ep ^H\~&vhthPX '+-*/'-$<plus,minus,times,div>@dh

evaluate = traverse+ parse+ lex
```


test program:

```Ursala
#cast %eL

test = evaluate*t

-[
1+1
4/5
2-1
3*7
3+4+5
9-2-4
7/3/2
4+2*3
5*2-1
5-3*2
(1+1)*(2+3)
(2-4)/(3+5*(8-1))]-
```

{{out}}

```txt

<
   2.000000e+00,
   8.000000e-01,
   1.000000e+00,
   2.100000e+01,
   1.200000e+01,
   3.000000e+00,
   1.166667e+00,
   1.000000e+01,
   9.000000e+00,
   -1.000000e+00,
   1.000000e+01,
   -5.263158e-02>
```



## zkl

In zkl, the compiler stack is part of the language and is written in zkl so ...

```zkl
Compiler.Parser.parseText("(1+3)*7").dump();
Compiler.Parser.parseText("1+3*7").dump();
```

The ASTs look like
{{out}}

```txt

class RootClass#    Input source: "<text>"
Attributes:  static createReturnsSelf
   ...
{ Block(Class)   <Line 1>
   Exp(
      (,1,+,3,),*,7
   )
}

class RootClass#    Input source: "<text>"
...
{ Block(Class)   <Line 1>
   Exp(
      1,+,3,*,7
   )
}

```

Evaluating them is just moving up the stack:

```zkl
Compiler.Compiler.compileText("(1+3)*7").__constructor(); vm.regX;
Compiler.Compiler.compileText("1+3*7").__constructor(); vm.regX;
```

{{out}}

```txt

28
22

```



## ZX Spectrum Basic


```zxbasic
10 PRINT "Use integer numbers and signs"'"+ - * / ( )"''
20 LET s$="": REM last symbol
30 LET pc=0: REM parenthesis counter
40 LET i$="1+2*(3+(4*5+6*7*8)-9)/10"
50 PRINT "Input = ";i$
60 FOR n=1 TO LEN i$
70 LET c$=i$(n)
80 IF c$>="0" AND c$<="9" THEN GO SUB 170: GO TO 130
90 IF c$="+" OR c$="-" THEN GO SUB 200: GO TO 130
100 IF c$="*" OR c$="/" THEN GO SUB 200: GO TO 130
110 IF c$="(" OR c$=")" THEN GO SUB 230: GO TO 130
120 GO TO 300
130 NEXT n
140 IF pc>0 THEN PRINT FLASH 1;"Parentheses not paired.": BEEP 1,-25: STOP
150 PRINT "Result = ";VAL i$
160 STOP
170 IF s$=")" THEN GO TO 300
180 LET s$=c$
190 RETURN
200 IF (NOT (s$>="0" AND s$<="9")) AND s$<>")" THEN GO TO 300
210 LET s$=c$
220 RETURN
230 IF c$="(" AND ((s$>="0" AND s$<="9") OR s$=")") THEN GO TO 300
240 IF c$=")" AND ((NOT (s$>="0" AND s$<="9")) OR s$="(") THEN GO TO 300
250 LET s$=c$
260 IF c$="(" THEN LET pc=pc+1: RETURN
270 LET pc=pc-1
280 IF pc<0 THEN GO TO 300
290 RETURN
300 PRINT FLASH 1;"Invalid symbol ";c$;" detected in pos ";n: BEEP 1,-25
310 STOP

```

