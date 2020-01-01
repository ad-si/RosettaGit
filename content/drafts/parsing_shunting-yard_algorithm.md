+++
title = "Parsing/Shunting-yard algorithm"
description = ""
date = 2019-04-29T23:48:33Z
aliases = []
[extra]
id = 10991
[taxonomies]
categories = []
tags = []
+++

{{task}}

;Task:
Given the operator characteristics and input from the [[wp:Shunting-yard_algorithm|Shunting-yard algorithm]] page and tables, use the algorithm to show the changes in the operator stack and RPN output
as each individual token is processed.

* Assume an input of a correct, space separated, string of tokens representing an infix expression
* Generate a space separated output string representing the RPN
* Test with the input string:
:::: <big><big><code> 3 + 4 * 2 / ( 1 - 5 ) ^ 2 ^ 3 </code></big></big>
* print and display the output here.
* Operator precedence is given in this table:
:{| class="wikitable"

! operator !! [[wp:Order_of_operations|precedence]] !! [[wp:Operator_associativity|associativity]] !! operation
|- || align="center"
|                    <big><big> ^ </big></big>  ||  4  ||  right  ||  exponentiation
|- || align="center"
|                    <big><big> * </big></big>  ||  3  ||  left   ||  multiplication
|- || align="center"
|                    <big><big> / </big></big>  ||  3  ||  left   ||  division
|- || align="center"
|                    <big><big> + </big></big>  ||  2  ||  left   ||  addition
|- || align="center"
|                    <big><big> - </big></big>  ||  2  ||  left   ||  subtraction
|}



;Extra credit
Add extra text explaining the actions and an optional comment for the action on receipt of each token.


;Note
The handling of functions and arguments is not required.


;See also:
* [[Parsing/RPN calculator algorithm]] for a method of calculating a final value from this output RPN expression.
* [[Parsing/RPN to infix conversion]].





## 8th


```forth
\ Convert infix expression to postfix, using 'shunting-yard' algorithm
\ https://en.wikipedia.org/wiki/Shunting-yard_algorithm


\ precedence of infix tokens.  negative means 'right-associative', otherwise left:
with: n
{
  "+" : 2,
  "-" : 2,
  "/" : 3,
  "*" : 3,
  "^" : -4,
  "(" : 1,
  ")" : -1
} var, tokens

: precedence \ s -- s n
  tokens @ over m:@ nip
  null? if drop 0 then ;

var ops
var out

: >out \ x --
  out @ swap
  a:push drop ;

: >ops \ op prec --
  2 a:close
  ops @ swap
  a:push drop ;

: a:peek -1 a:@ ;

\ Check the array for items with greater or equal precedence,
\ and move them to the out queue:
: pop-ops \ op prec ops -- op prec ops
  \ empty array, do nothing:
  a:len not if ;; then

  \ Look at top of ops stack:
  a:peek a:open            \ op p ops[] op2 p2

  \ if the 'p2' is not less p (meaning item on top of stack is greater or equal
  \ in precedence), then pop the item from the ops stack and push onto the out:
  3 pick \ p2 p
  < not if
    \ op p ops[] op2
    >out a:pop drop recurse ;;
  then
  drop ;


: right-paren
  "RIGHTPAREN" . cr
  2drop
  \ move non-left-paren from ops and move to out:
  ops @
  repeat
    a:len not if
      break
    else
      a:pop a:open
      1 = if
        2drop ;;
      else
        >out
      then
    then
  again drop ;

: .state \ n --
drop \  "Token: %s\n" s:strfmt .
  "Out:   " .
    out @  ( . space drop ) a:each drop cr
  "ops:   " .  ops @ ( 0 a:@ . space 2drop ) a:each drop cr cr ;

: handle-number \ s n --
  "NUMBER " . over . cr
  drop >out ;

: left-paren \ s n --
  "LEFTPAREN" .  cr
  >ops ;

: handle-op \ s n --
    "OPERATOR " . over . cr
    \ op precedence
    \ Is the current op left-associative?
    dup sgn 1 = if
      \ it is, so check the ops array for items with greater or equal precedence,
      \ and move them to the out queue:
      ops @ pop-ops drop
    then
    \ push the operator
    >ops ;

: handle-token \ s --
  precedence dup not if
    \ it's a number:
    handle-number
  else
    dup 1 = if        left-paren
    else dup -1 = if  right-paren
    else              handle-op
    then then
  then ;

: infix>postfix \ s -- s
  /\s+/ s:/           \ split to indiviual whitespace-delimited tokens

  \ Initialize our data structures
  a:new ops !   a:new out !

  (
    nip dup >r
    handle-token
    r> .state
  ) a:each drop
  \ remove all remaining ops and put on output:
  out @
  ops @ a:rev
  ( nip a:open drop a:push ) a:each drop
  \ final string:
  " " a:join ;

"3 + 4 * 2 / ( 1 - 5 ) ^ 2 ^ 3" infix>postfix . cr

"Expected: \n" . "3 4 2 * 1 5 - 2 3 ^ ^ / +" .  cr
bye

```

{{out}}

```txt
NUMBER 3
Out:   3
ops:

OPERATOR +
Out:   3
ops:   +

NUMBER 4
Out:   3 4
ops:   +

OPERATOR *
Out:   3 4
ops:   + *

NUMBER 2
Out:   3 4 2
ops:   + *

OPERATOR /
Out:   3 4 2 *
ops:   + /

LEFTPAREN
Out:   3 4 2 *
ops:   + / (

NUMBER 1
Out:   3 4 2 * 1
ops:   + / (

OPERATOR -
Out:   3 4 2 * 1
ops:   + / ( -

NUMBER 5
Out:   3 4 2 * 1 5
ops:   + / ( -

RIGHTPAREN
Out:   3 4 2 * 1 5 -
ops:   + /

OPERATOR ^
Out:   3 4 2 * 1 5 -
ops:   + / ^

NUMBER 2
Out:   3 4 2 * 1 5 - 2
ops:   + / ^

OPERATOR ^
Out:   3 4 2 * 1 5 - 2
ops:   + / ^ ^

NUMBER 3
Out:   3 4 2 * 1 5 - 2 3
ops:   + / ^ ^

3 4 2 * 1 5 - 2 3 ^ ^ / +
Expected:
3 4 2 * 1 5 - 2 3 ^ ^ / +

```



## ALGOL 68

{{works with|ALGOL 68G|Any - tested with release 2.8.3.win32}}

```algol68
BEGIN
    # parses s and returns an RPN expression using Dijkstra's "Shunting Yard" algorithm #
    # s is expected to contain a valid infix expression containing single-digit numbers and single-character operators #
    PROC parse = ( STRING s )STRING:
    BEGIN
        # add to the output #
        PROC output element = ( CHAR c )VOID:
        BEGIN
            output[ output pos +:= 1 ] := c;
            output pos +:= 1
        END # output element # ;
        PROC stack op = ( CHAR c )VOID: stack[ stack pos +:= 1 ] := c;
        # unstacks and returns the top operator on the stack - stops the program if the stack is empty #
        PROC unstack = CHAR:
            IF stack pos < 1 THEN
                # empty stack #
                print( ( "Stack underflow", newline ) );
                stop
            ELSE
                # still something on the stack to unstack #
                CHAR    result = stack[ stack pos ];
                stack[ stack pos ] := " ";
                stack pos         -:=  1;
                result
            FI # unstack # ;
        # returns the priority of the operator o - which must be one of "(", "^", "*", "/", "+" or "-" #
        PROC priority of = ( CHAR o )INT: IF o = "(" THEN -1 ELIF o = "^" THEN 4 ELIF o = "*" OR o = "/" THEN 3 ELSE 2 FI;
        # returns TRUE if o is a right-associative operator, FALSE otherwise #
        PROC right = ( CHAR c )BOOL: c = "^";
        PROC lower or equal priority = ( CHAR c )BOOL:
            IF stack pos < 1 THEN FALSE # empty stack #
            ELSE priority of( c ) <= priority of( stack[ stack pos ] )
            FI # lower or equal priority # ;
        PROC lower priority = ( CHAR c )BOOL:
            IF stack pos < 1 THEN FALSE # empty stack #
            ELSE priority of( c ) < priority of( stack[ stack pos ] )
            FI # lower priority # ;
        # max stack size and output size #
        INT    max stack = 32;
        # stack and output queue #
        [ 1 : max stack ]CHAR stack;
        [ 1 : max stack ]CHAR output;
        FOR c pos TO max stack DO stack[ c pos ] := output[ c pos ] := " " OD;
        # stack pointer and output queue pointer #
        INT    stack pos  := 0;
        INT    output pos := 0;
        print( ( "Parsing: ", s, newline ) );
        print( ( "token   output                               stack", newline ) );
        FOR s pos FROM LWB s TO UPB s DO
            CHAR c = s[ s pos ];
            IF c /= " " THEN
                IF c >= "0" AND c <= "9" THEN output element( c )
                ELIF c = "(" THEN stack op( c )
                ELIF c = ")" THEN
                    # close bracket - unstack to the matching "(" and unstack the "(" #
                    WHILE CHAR op char = unstack;
                          op char /= "("
                    DO
                        output element( op char )
                    OD
                ELIF right( c ) THEN
                    # right associative operator #
                    WHILE lower priority( c )          DO output element( unstack ) OD;
                    stack op( c )
                ELSE
                    # must be left associative #
                    WHILE lower or equal priority( c ) DO output element( unstack ) OD;
                    stack op( c )
                FI;
                print( ( c, "       ", output, "     ", stack, newline ) )
            FI
        OD;
        WHILE stack pos >= 1 DO output element( unstack ) OD;
        output[ 1 : output pos ]
    END # parse # ;

    print( ( "result: ", parse( "3 + 4 * 2 / ( 1 - 5 ) ^ 2 ^ 3" ), newline ) )
END
```

{{out}}

```txt

Parsing: 3 + 4 * 2 / ( 1 - 5 ) ^ 2 ^ 3
token   output                               stack
3       3
+       3                                    +
4       3 4                                  +
*       3 4                                  +*
2       3 4 2                                +*
/       3 4 2 *                              +/
(       3 4 2 *                              +/(
1       3 4 2 * 1                            +/(
-       3 4 2 * 1                            +/(-
5       3 4 2 * 1 5                          +/(-
)       3 4 2 * 1 5 -                        +/
^       3 4 2 * 1 5 -                        +/^
2       3 4 2 * 1 5 - 2                      +/^
^       3 4 2 * 1 5 - 2                      +/^^
3       3 4 2 * 1 5 - 2 3                    +/^^
result: 3 4 2 * 1 5 - 2 3 ^ ^ / +
```



## AutoHotkey

{{works with|AutoHotkey_L}}

```AHK
SetBatchLines -1
#NoEnv

expr := "3 + 4 * 2 / ( 1 - 5 ) ^ 2 ^ 3"

output := "Testing string '" expr "'`r`n`r`nToken`tOutput Queue"
           . Space(StrLen(expr)-StrLen("Output Queue")+2) "OP Stack"

; define a stack with semantic .push() and .pop() funcs
stack := {push: func("ObjInsert"), pop: func("ObjRemove"), peek: func("Peek")}

Loop Parse, expr, %A_Space%
{
       token := A_LoopField
       if token is number
               Q .= token A_Space
       if isOp(token){
               o1 := token
               while   isOp(o2 := stack.peek())
                       and ((isLeft(o1)  and Precedence(o1) <= Precedence(o2))
                       or  (isRight(o1) and Precedence(o1) <  Precedence(o2)))
                   Q .= stack.pop() A_Space
               stack.push(o1)
       }
       If ( token = "(" )
               stack.push(token)
       If ( token = ")" )
       {
               While ((t := stack.pop()) != "(") && (t != "")
                       Q .= t A_Space
               if (t = "")
                       throw Exception("Unmatched parenthesis. "
                          . "Character number " A_Index)
       }
       output .= "`r`n" token Space(7) Q Space(StrLen(expr)+2-StrLen(Q))
               . Disp(stack)
}
output .= "`r`n(empty stack to output)"
While (t := stack.pop()) != ""
       if InStr("()", t)
               throw Exception("Unmatched parenthesis.")
       else    Q .= t A_Space, output .= "`r`n" Space(8) Q
                       . Space(StrLen(expr)+2-StrLen(Q)) Disp(stack)
output .= "`r`n`r`nFinal string: '" Q "'"
clipboard := output

isOp(t){
       return (!!InStr("+-*/^", t) && t)
}
Peek(this){
       r := this.Remove(), this.Insert(r)
       return r
}
IsLeft(o){
       return !!InStr("*/+-", o)
}
IsRight(o){
       return o = "^"
}
Precedence(o){
       return (InStr("+-/*^", o)+3)//2
}
Disp(obj){
       for each, val in obj
               o := val . o
       return  o
}
Space(n){
       return n>0 ? A_Space Space(n-1) : ""
}
```

;Output
<pre style="height:30ex;overflow:scroll;">Testing string '3 + 4 * 2 / ( 1 - 5 ) ^ 2 ^ 3'

Token	Output Queue                   OP Stack
3       3
+       3                              +
4       3 4                            +
*       3 4                            *+
2       3 4 2                          *+
/       3 4 2 *                        /+
(       3 4 2 *                        (/+
1       3 4 2 * 1                      (/+
-       3 4 2 * 1                      -(/+
5       3 4 2 * 1 5                    -(/+
)       3 4 2 * 1 5 -                  /+
^       3 4 2 * 1 5 -                  ^/+
2       3 4 2 * 1 5 - 2                ^/+
^       3 4 2 * 1 5 - 2                ^^/+
3       3 4 2 * 1 5 - 2 3              ^^/+
(empty stack to output)
        3 4 2 * 1 5 - 2 3 ^            ^/+
        3 4 2 * 1 5 - 2 3 ^ ^          /+
        3 4 2 * 1 5 - 2 3 ^ ^ /        +
        3 4 2 * 1 5 - 2 3 ^ ^ / +

Final string: '3 4 2 * 1 5 - 2 3 ^ ^ / + '
```



## C

Requires a functioning ANSI terminal and Enter key.

```c
#include <sys/types.h>
#include <regex.h>
#include <stdio.h>

typedef struct {
	const char *s;
	int len, prec, assoc;
} str_tok_t;

typedef struct {
	const char * str;
	int assoc, prec;
	regex_t re;
} pat_t;

enum assoc { A_NONE, A_L, A_R };
pat_t pat_eos = {"", A_NONE, 0};

pat_t pat_ops[] = {
	{"^\\)",	A_NONE, -1},
	{"^\\*\\*",	A_R, 3},
	{"^\\^",	A_R, 3},
	{"^\\*",	A_L, 2},
	{"^/",		A_L, 2},
	{"^\\+",	A_L, 1},
	{"^-",		A_L, 1},
	{0}
};

pat_t pat_arg[] = {
	{"^[-+]?[0-9]*\\.?[0-9]+([eE][-+]?[0-9]+)?"},
	{"^[a-zA-Z_][a-zA-Z_0-9]*"},
	{"^\\(", A_L, -1},
	{0}
};

str_tok_t stack[256]; /* assume these are big enough */
str_tok_t queue[256];
int l_queue, l_stack;
#define qpush(x) queue[l_queue++] = x
#define spush(x) stack[l_stack++] = x
#define spop()   stack[--l_stack]

void display(const char *s)
{
	int i;
	printf("\033[1;1H\033[JText | %s", s);
	printf("\nStack| ");
	for (i = 0; i < l_stack; i++)
		printf("%.*s ", stack[i].len, stack[i].s); // uses C99 format strings
	printf("\nQueue| ");
	for (i = 0; i < l_queue; i++)
		printf("%.*s ", queue[i].len, queue[i].s);
	puts("\n\n<press enter>");
	getchar();
}

int prec_booster;

#define fail(s1, s2) {fprintf(stderr, "[Error %s] %s\n", s1, s2); return 0;}

int init(void)
{
	int i;
	pat_t *p;

	for (i = 0, p = pat_ops; p[i].str; i++)
		if (regcomp(&(p[i].re), p[i].str, REG_NEWLINE|REG_EXTENDED))
			fail("comp", p[i].str);

	for (i = 0, p = pat_arg; p[i].str; i++)
		if (regcomp(&(p[i].re), p[i].str, REG_NEWLINE|REG_EXTENDED))
			fail("comp", p[i].str);

	return 1;
}

pat_t* match(const char *s, pat_t *p, str_tok_t * t, const char **e)
{
	int i;
	regmatch_t m;

	while (*s == ' ') s++;
	*e = s;

	if (!*s) return &pat_eos;

	for (i = 0; p[i].str; i++) {
		if (regexec(&(p[i].re), s, 1, &m, REG_NOTEOL))
			continue;
		t->s = s;
		*e = s + (t->len = m.rm_eo - m.rm_so);
		return p + i;
	}
	return 0;
}

int parse(const char *s) {
	pat_t *p;
	str_tok_t *t, tok;

	prec_booster = l_queue = l_stack = 0;
	display(s);
	while (*s) {
		p = match(s, pat_arg, &tok, &s);
		if (!p || p == &pat_eos) fail("parse arg", s);

		/* Odd logic here. Don't actually stack the parens: don't need to. */
		if (p->prec == -1) {
			prec_booster += 100;
			continue;
		}
		qpush(tok);
		display(s);

re_op:		p = match(s, pat_ops, &tok, &s);
		if (!p) fail("parse op", s);

		tok.assoc = p->assoc;
		tok.prec = p->prec;

		if (p->prec > 0)
			tok.prec = p->prec + prec_booster;
		else if (p->prec == -1) {
			if (prec_booster < 100)
				fail("unmatched )", s);
			tok.prec = prec_booster;
		}

		while (l_stack) {
			t = stack + l_stack - 1;
			if (!(t->prec == tok.prec && t->assoc == A_L)
					&& t->prec <= tok.prec)
				break;
			qpush(spop());
			display(s);
		}

		if (p->prec == -1) {
			prec_booster -= 100;
			goto re_op;
		}

		if (!p->prec) {
			display(s);
			if (prec_booster)
				fail("unmatched (", s);
			return 1;
		}

		spush(tok);
		display(s);
	}

	if (p->prec > 0)
		fail("unexpected eol", s);

	return 1;
}

int main()
{
	int i;
	const char *tests[] = {
		"3 + 4 * 2 / ( 1 - 5 ) ^ 2 ^ 3",	/* RC mandated: OK */
		"123",					/* OK */
		"3+4 * 2 / ( 1 - 5 ) ^ 2 ^ 3.14",	/* OK */
		"(((((((1+2+3**(4 + 5))))))",		/* bad parens */
		"a^(b + c/d * .1e5)!",			/* unknown op */
		"(1**2)**3",				/* OK */
		"2 + 2 *",				/* unexpected eol */
		0
	};

	if (!init()) return 1;
	for (i = 0; tests[i]; i++) {
		printf("Testing string `%s'   <enter>\n", tests[i]);
		getchar();

		printf("string `%s': %s\n\n", tests[i],
			parse(tests[i]) ? "Ok" : "Error");
	}

	return 0;
}
```


;Output:
Note: This cannot give a flavour of the true interactive output where tokens are shuffled around every time the enter key is pressed.

```txt
Testing string `3 + 4 * 2 / ( 1 - 5 ) ^ 2 ^ 3'   <enter>

Text | 3 + 4 * 2 / ( 1 - 5 ) ^ 2 ^ 3
Stack|
Queue|

<press enter>

Text |  + 4 * 2 / ( 1 - 5 ) ^ 2 ^ 3
Stack|
Queue| 3

<press enter>

Text |  4 * 2 / ( 1 - 5 ) ^ 2 ^ 3
Stack| +
Queue| 3

<press enter>

Text |  * 2 / ( 1 - 5 ) ^ 2 ^ 3
Stack| +
Queue| 3 4

<press enter>

Text |  2 / ( 1 - 5 ) ^ 2 ^ 3
Stack| + *
Queue| 3 4

<press enter>

Text |  / ( 1 - 5 ) ^ 2 ^ 3
Stack| + *
Queue| 3 4 2

<press enter>

Text |  ( 1 - 5 ) ^ 2 ^ 3
Stack| +
Queue| 3 4 2 *

<press enter>

Text |  ( 1 - 5 ) ^ 2 ^ 3
Stack| + /
Queue| 3 4 2 *

<press enter>

Text |  - 5 ) ^ 2 ^ 3
Stack| + /
Queue| 3 4 2 * 1

<press enter>

Text |  5 ) ^ 2 ^ 3
Stack| + / -
Queue| 3 4 2 * 1

<press enter>

Text |  ) ^ 2 ^ 3
Stack| + / -
Queue| 3 4 2 * 1 5

<press enter>

Text |  ^ 2 ^ 3
Stack| + /
Queue| 3 4 2 * 1 5 -

<press enter>

Text |  2 ^ 3
Stack| + / ^
Queue| 3 4 2 * 1 5 -

<press enter>

Text |  ^ 3
Stack| + / ^
Queue| 3 4 2 * 1 5 - 2

<press enter>

Text |  3
Stack| + / ^ ^
Queue| 3 4 2 * 1 5 - 2

<press enter>

Text |
Stack| + / ^ ^
Queue| 3 4 2 * 1 5 - 2 3

<press enter>

Text |
Stack| + / ^
Queue| 3 4 2 * 1 5 - 2 3 ^

<press enter>

Text |
Stack| + /
Queue| 3 4 2 * 1 5 - 2 3 ^ ^

<press enter>

Text |
Stack| +
Queue| 3 4 2 * 1 5 - 2 3 ^ ^ /

<press enter>

Text |
Stack|
Queue| 3 4 2 * 1 5 - 2 3 ^ ^ / +

<press enter>

Text |
Stack|
Queue| 3 4 2 * 1 5 - 2 3 ^ ^ / +

<press enter>

string `3 + 4 * 2 / ( 1 - 5 ) ^ 2 ^ 3': Ok

Testing string `123'   <enter>
^C
```



## C++

{{trans|Java}}

```cpp
#include <iostream>
#include <sstream>
#include <stack>

std::string infixToPostfix(const std::string& infix) {
    const std::string ops = "-+/*^";
    std::stringstream ss;
    std::stack<int> s;

    std::stringstream input(infix);
    std::string token;
    while (std::getline(input, token, ' ')) {
        if (token.empty()) {
            continue;
        }

        char c = token[0];
        size_t idx = ops.find(c);

        // check for operator
        if (idx != std::string::npos) {
            while (!s.empty()) {
                int prec2 = s.top() / 2;
                int prec1 = idx / 2;
                if (prec2 > prec1 || (prec2 == prec1 && c != '^')) {
                    ss << ops[s.top()] << ' ';
                    s.pop();
                } else break;
            }
            s.push(idx);
        } else if (c == '(') {
            s.push(-2); // -2 stands for '('
        } else if (c == ')') {
            // until '(' on stack, pop operators.
            while (s.top() != -2) {
                ss << ops[s.top()] << ' ';
                s.pop();
            }
            s.pop();
        } else {
            ss << token << ' ';
        }
    }

    while (!s.empty()) {
        ss << ops[s.top()] << ' ';
        s.pop();
    }

    return ss.str();
}

int main() {
    std::string infix = "3 + 4 * 2 / ( 1 - 5 ) ^ 2 ^ 3";
    std::cout << "infix:   " << infix << '\n';
    std::cout << "postfix: " << infixToPostfix(infix) << '\n';

    return 0;
}
```

{{out}}

```txt
infix:   3 + 4 * 2 / ( 1 - 5 ) ^ 2 ^ 3
postfix: 3 4 2 * 1 5 - 2 3 ^ ^ / +
```



## C sharp

{{works with|C sharp|7.0}}

```csharp
using System;
using System.Collections.Generic;
using System.Linq;

public class Program
{
    public static void Main() {
        string infix = "3 + 4 * 2 / ( 1 - 5 ) ^ 2 ^ 3";
        Console.WriteLine(infix.ToPostfix());
    }
}

public static class ShuntingYard
{
    private static readonly Dictionary<string, (string symbol, int precedence, bool rightAssociative)> operators
        = new (string symbol, int precedence, bool rightAssociative) [] {
            ("^", 4, true),
            ("*", 3, false),
            ("/", 3, false),
            ("+", 2, false),
            ("-", 2, false)
    }.ToDictionary(op => op.symbol);

    public static string ToPostfix(this string infix) {
        string[] tokens = infix.Split(' ');
        var stack = new Stack<string>();
        var output = new List<string>();
        foreach (string token in tokens) {
            if (int.TryParse(token, out _)) {
                output.Add(token);
                Print(token);
            } else if (operators.TryGetValue(token, out var op1)) {
                while (stack.Count > 0 && operators.TryGetValue(stack.Peek(), out var op2)) {
                    int c = op1.precedence.CompareTo(op2.precedence);
                    if (c < 0 || !op1.rightAssociative && c <= 0) {
                        output.Add(stack.Pop());
                    } else {
                        break;
                    }
                }
                stack.Push(token);
                Print(token);
            } else if (token == "(") {
                stack.Push(token);
                Print(token);
            } else if (token == ")") {
                string top = "";
                while (stack.Count > 0 && (top = stack.Pop()) != "(") {
                    output.Add(top);
                }
                if (top != "(") throw new ArgumentException("No matching left parenthesis.");
                Print(token);
            }
        }
        while (stack.Count > 0) {
            var top = stack.Pop();
            if (!operators.ContainsKey(top)) throw new ArgumentException("No matching right parenthesis.");
            output.Add(top);
        }
        Print("pop");
        return string.Join(" ", output);

        //Yikes!
        void Print(string action) => Console.WriteLine($"{action + ":",-4} {$"stack[ {string.Join(" ", stack.Reverse())} ]",-18} {$"out[ {string.Join(" ", output)} ]"}");
        //A little more readable?
        void Print(string action) => Console.WriteLine("{0,-4} {1,-18} {2}", action + ":", $"stack[ {string.Join(" ", stack.Reverse())} ]", $"out[ {string.Join(" ", output)} ]");
    }
}
```

{{out}}

```txt

3:   stack[  ]          out[ 3 ]
+:   stack[ + ]         out[ 3 ]
4:   stack[ + ]         out[ 3 4 ]
*:   stack[ + * ]       out[ 3 4 ]
2:   stack[ + * ]       out[ 3 4 2 ]
/:   stack[ + / ]       out[ 3 4 2 * ]
(:   stack[ + / ( ]     out[ 3 4 2 * ]
1:   stack[ + / ( ]     out[ 3 4 2 * 1 ]
-:   stack[ + / ( - ]   out[ 3 4 2 * 1 ]
5:   stack[ + / ( - ]   out[ 3 4 2 * 1 5 ]
):   stack[ + / ]       out[ 3 4 2 * 1 5 - ]
^:   stack[ + / ^ ]     out[ 3 4 2 * 1 5 - ]
2:   stack[ + / ^ ]     out[ 3 4 2 * 1 5 - 2 ]
^:   stack[ + / ^ ^ ]   out[ 3 4 2 * 1 5 - 2 ]
3:   stack[ + / ^ ^ ]   out[ 3 4 2 * 1 5 - 2 3 ]
pop: stack[  ]          out[ 3 4 2 * 1 5 - 2 3 ^ ^ / + ]
3 4 2 * 1 5 - 2 3 ^ ^ / +
```



## Ceylon


```ceylon
import ceylon.collection {

	ArrayList
}

abstract class Token(String|Integer data) of IntegerLiteral | Operator | leftParen | rightParen {
	string => data.string;
}
class IntegerLiteral(shared Integer integer) extends Token(integer) {}
class Operator  extends Token {

	shared Integer precedence;
	shared Boolean rightAssoc;

	shared new plus extends Token("+") {
		precedence = 2;
		rightAssoc = false;
	}
	shared new minus extends Token("-") {
		precedence = 2;
		rightAssoc = false;
	}
	shared new times extends Token("*") {
		precedence = 3;
		rightAssoc = false;
	}
	shared new divides extends Token("/") {
		precedence = 3;
		rightAssoc = false;
	}
	shared new power extends Token("^") {
		precedence = 4;
		rightAssoc = true;
	}

	shared Boolean below(Operator other) =>
			!rightAssoc && precedence <= other.precedence || rightAssoc && precedence < other.precedence;
}
object leftParen extends Token("(") {}
object rightParen extends Token(")") {}


shared void run() {

	function shunt(String input) {

		function tokenize(String input) =>
				input.split().map((String element) =>
					switch(element.trimmed)
					case("(") leftParen
					case(")") rightParen
					case("+") Operator.plus
					case("-") Operator.minus
					case("*") Operator.times
					case("/") Operator.divides
					case("^") Operator.power
					else IntegerLiteral(parseInteger(element) else 0)); // no error handling

		value outputQueue = ArrayList<Token>();
		value operatorStack = ArrayList<Token>();

		void report(String action) {
			print("``action.padTrailing(22)`` | ``" ".join(outputQueue).padTrailing(25)`` | ``" ".join(operatorStack).padTrailing(10)``");
		}

		print("input is ``input``\n");
		print("Action                 | Output Queue              | Operators' Stack
		       -----------------------|---------------------------|-----------------");

		for(token in tokenize(input)) {
			switch(token)
			case(is IntegerLiteral) {
				outputQueue.offer(token);
				report("``token`` from input to queue");
			}
			case(leftParen) {
				operatorStack.push(token);
				report("``token`` from input to stack");
			}
			case(rightParen){
				while(exists top = operatorStack.pop(), top != leftParen) {
					outputQueue.offer(top);
					report("``top`` from stack to queue");
				}
			}
			case(is Operator) {
				while(exists top = operatorStack.top, is Operator top, token.below(top)) {
					operatorStack.pop();
					outputQueue.offer(top);
					report("``top`` from stack to queue");
				}
				operatorStack.push(token);
				report("``token`` from input to stack");
			}
		}
		while(exists top = operatorStack.pop()) {
			outputQueue.offer(top);
			report("``top`` from stack to queue");
		}
		return " ".join(outputQueue);
	}

	value rpn = shunt("3 + 4 * 2 / ( 1 - 5 ) ^ 2 ^ 3");
	assert(rpn == "3 4 2 * 1 5 - 2 3 ^ ^ / +");
	print("\nthe result is ``rpn``");
}
```

{{out}}

```txt
input is 3 + 4 * 2 / ( 1 - 5 ) ^ 2 ^ 3

Action                 | Output Queue              | Operators' Stack
-----------------------|---------------------------|-----------------
3 from input to queue  | 3                         |
+ from input to stack  | 3                         | +
4 from input to queue  | 3 4                       | +
* from input to stack  | 3 4                       | + *
2 from input to queue  | 3 4 2                     | + *
* from stack to queue  | 3 4 2 *                   | +
/ from input to stack  | 3 4 2 *                   | + /
( from input to stack  | 3 4 2 *                   | + / (
1 from input to queue  | 3 4 2 * 1                 | + / (
- from input to stack  | 3 4 2 * 1                 | + / ( -
5 from input to queue  | 3 4 2 * 1 5               | + / ( -
- from stack to queue  | 3 4 2 * 1 5 -             | + / (
^ from input to stack  | 3 4 2 * 1 5 -             | + / ^
2 from input to queue  | 3 4 2 * 1 5 - 2           | + / ^
^ from input to stack  | 3 4 2 * 1 5 - 2           | + / ^ ^
3 from input to queue  | 3 4 2 * 1 5 - 2 3         | + / ^ ^
^ from stack to queue  | 3 4 2 * 1 5 - 2 3 ^       | + / ^
^ from stack to queue  | 3 4 2 * 1 5 - 2 3 ^ ^     | + /
/ from stack to queue  | 3 4 2 * 1 5 - 2 3 ^ ^ /   | +
+ from stack to queue  | 3 4 2 * 1 5 - 2 3 ^ ^ / + |

the result is 3 4 2 * 1 5 - 2 3 ^ ^ / +
```



## Common Lisp

Implemented as a state machine. The current state is the top of both the input queue and the operator stack. A signal function receives the current state and does a lookup to determine the signal to output. Based on the signal, the state (input queue and/or operator stack) is changed. The process iterates until both queue and stack are empty.

```lisp
;;;; Parsing/infix to RPN conversion
(defconstant operators "^*/+-")
(defconstant precedence '(-4 3 3 2 2))

(defun operator-p (op)
  "string->integer|nil: Returns operator precedence index or nil if not operator."
  (and (= (length op) 1) (position (char op 0) operators)))

(defun has-priority (op2 op1)
  "(string,string)->boolean: True if op2 has output priority over op1."
  (defun prec (op) (nth (operator-p op) precedence))
  (or (and (plusp (prec op1))  (<= (prec op1) (abs (prec op2))))
      (and (minusp (prec op1)) (< (- (prec op1)) (abs (prec op2))))))

(defun string-split (expr)
  "string->list: Tokenize a space separated string."
  (let* ((p (position #\Space expr))
         (tok (if p (subseq expr 0 p) expr)))
    (if p (append (list tok) (string-split (subseq expr (1+ p)))) (list tok))))

(defun classify (tok)
  "nil|string->symbol: Classify a token."
  (cond
   ((null tok) 'NOL)
   ((operator-p tok) 'OPR)
   ((string= tok "(") 'LPR)
   ((string= tok ")") 'RPR)
   (t 'LIT)))

;;; transitions when op2 is dont care
(defconstant trans1D '((LIT GO) (LPR ENTER)))
;;; transitions when we check op2 also
(defconstant trans2D
  '((OPR ((NOL ENTER)
          (LPR ENTER)
          (OPR (lambda (op1 op2) (if (has-priority op2 op1) 'LEAVE 'ENTER)))))
    (RPR ((NOL "mismatched parentheses")
          (LPR CLEAR)
          (OPR LEAVE)))
    (NOL ((NOL nil)
          (LPR "mismatched parentheses")
          (OPR LEAVE)))))

(defun do-signal (op1 op2)
  "(nil|string,nil|string)->symbol|string|nil: Emit a signal based on state of inputq and opstack.
   A nil return is a successful lookup (on nil,nil) because all input combinations are specified."
  (let ((sig (or (cadr (assoc (classify op1) trans1D))
                 (cadr (assoc (classify op2) (cadr (assoc (classify op1) trans2D)))))))
    (if (or (null sig) (symbolp sig) (stringp sig)) sig
        (funcall (coerce sig 'function) op1 op2))))

(defun rpn (expr)
  "string->string: Parse infix expression into rpn."
  (format t "TOKEN  TOS    SIGNAL     OPSTACK       OUTPUTQ~%")

  ;; iterate until both stacks empty
  (do* ((input (string-split expr)) (opstack nil) (outputq "")
        (sig (do-signal (first input) (first opstack)) (do-signal (first input) (first opstack))))
       ((null sig) ; until
        ;; print last closing frame
        (format t "~A~7,T~A~14,T~A~25,T~A~38,T~A~%" nil nil nil opstack outputq)
        (subseq outputq 1)) ; return final infix expression

    ;; print opening frame
    (format t "~A~7,T~A~14,T" (first input) (first opstack))
    (format t (if (stringp sig) "\"~A\"" "~A") sig)

    ;; switch state
    (let ((output (case sig
                    (GO     (pop input))
                    (ENTER  (push (pop input) opstack) nil)
                    (LEAVE  (pop opstack))
                    (CLEAR  (pop input) (pop opstack) nil)
                    (otherwise (pop input) (pop opstack)
                               (if (stringp sig) sig "unknown signal")))))
      (when output (setf outputq (concatenate 'string outputq " " output))))

    ;; print closing frame
    (format t "~25,T~A~38,T~A~%" opstack outputq))) ; end-do

(defun main (&optional (xtra nil))
  "nil->[printed rpn expressions]: Main function."
  (let ((expressions '("3 + 4 * 2 / ( 1 - 5 ) ^ 2 ^ 3"
                       "( ( 1 + 2 ) ^ ( 3 + 4 ) ) ^ ( 5 + 6 )"
                       "( ( 3 ^ 4 ) ^ 2 ^ 9 ) ^ 2 ^ 5"
                       "3 + 4 * ( 5 - 6 ) ) 4 * 9")))
    (dolist (expr (if xtra expressions (list (car expressions))))
      (format t "~%INFIX:\"~A\"~%" expr)
      (format t "RPN:\"~A\"~%" (rpn expr)))))

```

{{out}}

```txt
CL-USER(2): (main)

INFIX:"3 + 4 * 2 / ( 1 - 5 ) ^ 2 ^ 3"
TOKEN  TOS    SIGNAL     OPSTACK       OUTPUTQ
3      NIL    GO         NIL           3
+      NIL    ENTER      (+)           3
4      +      GO         (+)           3 4
*      +      ENTER      (* +)         3 4
2      *      GO         (* +)         3 4 2
/      *      LEAVE      (+)           3 4 2 *
/      +      ENTER      (/ +)         3 4 2 *
(      /      ENTER      (( / +)       3 4 2 *
1      (      GO         (( / +)       3 4 2 * 1
-      (      ENTER      (- ( / +)     3 4 2 * 1
5      -      GO         (- ( / +)     3 4 2 * 1 5
)      -      LEAVE      (( / +)       3 4 2 * 1 5 -
)      (      CLEAR      (/ +)         3 4 2 * 1 5 -
^      /      ENTER      (^ / +)       3 4 2 * 1 5 -
2      ^      GO         (^ / +)       3 4 2 * 1 5 - 2
^      ^      ENTER      (^ ^ / +)     3 4 2 * 1 5 - 2
3      ^      GO         (^ ^ / +)     3 4 2 * 1 5 - 2 3
NIL    ^      LEAVE      (^ / +)       3 4 2 * 1 5 - 2 3 ^
NIL    ^      LEAVE      (/ +)         3 4 2 * 1 5 - 2 3 ^ ^
NIL    /      LEAVE      (+)           3 4 2 * 1 5 - 2 3 ^ ^ /
NIL    +      LEAVE      NIL           3 4 2 * 1 5 - 2 3 ^ ^ / +
NIL    NIL    NIL        NIL           3 4 2 * 1 5 - 2 3 ^ ^ / +
RPN:"3 4 2 * 1 5 - 2 3 ^ ^ / +"
NIL
```



## D

{{trans|Java}}

```D
import std.container;
import std.stdio;

void main() {
    string infix = "3 + 4 * 2 / ( 1 - 5 ) ^ 2 ^ 3";
    writeln("infix:   ", infix);
    writeln("postfix: ", infixToPostfix(infix));
}

string infixToPostfix(string infix) {
    import std.array;

    /* To find out the precedence, we take the index of the
       token in the ops string and divide by 2 (rounding down).
       This will give us: 0, 0, 1, 1, 2 */
    immutable ops = ["+", "-", "/", "*", "^"];

    auto sb = appender!string;
    SList!int stack;

    // split the input on whitespace
    foreach (token; infix.split) {
        if (token.empty) {
            continue;
        }

        int idx = ops.indexOf(token);

        // check for operator
        if (idx != -1) {
            while (!stack.empty) {
                int prec2 = stack.peek / 2;
                int prec1 = idx / 2;
                if (prec2 > prec1 || (prec2 == prec1 && token != "^")) {
                    sb.put(ops[stack.pop]);
                    sb.put(' ');
                } else {
                    break;
                }
            }
            stack.push(idx);
        } else if (token == "(") {
            stack.push(-2); // -2 stands for '('
        } else if (token == ")") {
            // until '(' on stack, pop operators.
            while (stack.peek != -2) {
                sb.put(ops[stack.pop]);
                sb.put(' ');
            }
            stack.pop();
        } else {
            sb.put(token);
            sb.put(' ');
        }
    }

    while (!stack.empty) {
        sb.put(ops[stack.pop]);
        sb.put(' ');
    }

    return sb.data;
}

// Find the first index of the specified value, or -1 if not found.
int indexOf(T)(const T[] a, const T v) {
    foreach(i,e; a) {
        if (e == v) {
            return i;
        }
    }
    return -1;
}

// Convienience for adding a new element
void push(T)(ref SList!T s, T v) {
    s.insertFront(v);
}

// Convienience for accessing the top element
auto peek(T)(SList!T s) {
    return s.front;
}

// Convienience for removing and returning the top element
auto pop(T)(ref SList!T s) {
    auto v = s.front;
    s.removeFront;
    return v;
}
```


{{out}}

```txt
infix:   3 + 4 * 2 / ( 1 - 5 ) ^ 2 ^ 3
postfix: 3 4 2 * 1 5 - 2 3 ^ ^ / +
```



## EchoLisp


```scheme

(require 'hash)
(require 'tree)

(define OPS (make-hash))
(hash-set OPS "^" '(  4 #f)) ;; right assoc
(hash-set OPS "*" '(  3 #t)) ;; left assoc
(hash-set OPS "/" '(  3 #t))
(hash-set OPS "+" '(  2 #t))
(hash-set OPS "-" '(  2 #t))

;; helpers
(define (is-right-par? token) (string=? token ")"))
(define (is-left-par? token)  (string=? token "("))
(define (is-num? op)   (not (hash-ref OPS op))) ;; crude
(define (is-op? op)    (hash-ref OPS op))
(define (is-left? op)  (second (hash-ref OPS op)))
(define (is-right? op) (not (is-left? op)))
(define (op-prec op)   (first (hash-ref OPS op)))

;; Wikipedia algorithm, translated as it is

(define (shunt tokens S Q)
    (for ((token tokens))
     (writeln "S: " (stack->list S) "Q: " (queue->list Q) "token: "token)
    (cond
    [(is-left-par? token) (push S token) ]
    [(is-right-par? token)
        (while (and (stack-top S) (not (is-left-par? (stack-top S))))
               (q-push Q ( pop S)))
        (when (stack-empty? S) (error 'misplaced-parenthesis "()" ))
        (pop S)] ; // left par

    [(is-op? token)
            (while (and
                (is-op? (stack-top S))
                (or
                  (and (is-left? token) (<= (op-prec token) (op-prec (stack-top S))))
                  (and (is-right? token) (< (op-prec token) (op-prec (stack-top S))))))
                (q-push Q (pop S)))
        (push S token)]

    [(is-num? token) (q-push Q token)]
    [else (error 'bad-token token)])) ; for
    (while (stack-top S) (q-push Q (pop S))))

(string-delimiter "")
(define (task infix)
    (define S (stack 'S))
    (define Q (queue 'Q))
        (shunt (text-parse infix) S Q)
        (writeln 'infix infix)
        (writeln 'RPN (queue->list Q)))

```

{{out}}

```txt

(task  "3 + 4 * 2 / ( 1 - 5 ) ^ 2 ^ 3")

S:      null     Q:      null     token:      3
S:      null     Q:      (3)     token:      +
S:      (+)     Q:      (3)     token:      4
S:      (+)     Q:      (3 4)     token:      *
S:      (+ *)     Q:      (3 4)     token:      2
S:      (+ *)     Q:      (3 4 2)     token:      /
S:      (+ /)     Q:      (3 4 2 *)     token:      (
S:      (+ / ()     Q:      (3 4 2 *)     token:      1
S:      (+ / ()     Q:      (3 4 2 * 1)     token:      -
S:      (+ / (-)     Q:      (3 4 2 * 1)     token:      5
S:      (+ / (-)     Q:      (3 4 2 * 1 5)     token:      )
S:      (+ /)       Q:      (3 4 2 * 1 5 -)     token:      ^
S:      (+ / ^)     Q:      (3 4 2 * 1 5 -)     token:      2
S:      (+ / ^)     Q:      (3 4 2 * 1 5 - 2)     token:      ^
S:      (+ / ^ ^)     Q:      (3 4 2 * 1 5 - 2)     token:      3

infix     3 + 4 * 2 / ( 1 - 5 ) ^ 2 ^ 3
RPN     (3 4 2 * 1 5 - 2 3 ^ ^ / +)

```


=={{header|F_Sharp|F#}}==

```fsharp
open System

type action = Shift | ReduceStack | ReduceInput

type State (input : string list, stack : string list, output : string list) =
    member x.Input with get() = input
    member x.Stack with get() = stack
    member x.Output with get() = output

    member x.report act =
        let markTop = function | [] -> [] | x::xs -> ("["+x+"]")::xs
        let (s, text) =
            if x.Stack = [] && x.Input = [] then x, ""
            else
                match act with
                | Shift -> State((markTop x.Input), x.Stack, x.Output), "shift"
                | ReduceStack -> State(x.Input, (markTop x.Stack), x.Output), "reduce"
                | ReduceInput -> State((markTop x.Input), x.Stack, x.Output), "reduce"

        let lstr (x :string list) = String.Join(" ", (List.toArray x))
        printfn "%25s    %-9s %6s %s" (lstr (List.rev s.Output)) (lstr (List.rev s.Stack)) text (lstr s.Input)

    member x.shift =
        x.report Shift
        State(x.Input.Tail, (x.Input.Head)::x.Stack, x.Output)

    member x.reduce =
        x.report ReduceStack
        State (x.Input, (x.Stack.Tail), (x.Stack.Head)::x.Output)

    member x.reduceNumber =
        x.report ReduceInput
        State(x.Input.Tail, x.Stack, (x.Input.Head)::x.Output)

let prec = function
| "^" -> 4 | "*" | "/" -> 3 | "+" | "-" -> 2 | "(" -> 1
| x -> failwith ("No precedence! Not an operator: " + x)

type assocKind = | Left | Right
let assoc = function | "^" -> Right | _ -> Left

let (|Number|Open|Close|Operator|) x =
    if (Double.TryParse >> fst) x then Number
    elif x = "(" then Open
    elif x = ")" then Close
    else Operator

let rec shunting_yard (s : State) =

    let rec reduce_to_Open (s : State) =
        match s.Stack with
        | [] -> failwith "mismatched parentheses!"
        | "("::xs -> State(s.Input.Tail, xs, s.Output)
        | _ ->
            reduce_to_Open s.reduce

    let reduce_by_prec_and_shift x s =
        let (xPrec, xAssoc) = (prec x, assoc x)
        let rec loop (s : State) =
            match s.Stack with
            | [] -> s
            | x::xs ->
                let topPrec = prec x
                if xAssoc = Left && xPrec <= topPrec || xAssoc = Right && xPrec < topPrec then
                    loop s.reduce
                else
                    s
        (loop s).shift

    let rec reduce_rest (s : State) =
        match s.Stack with
        | [] -> s
        | "("::_ -> failwith "mismatched parentheses!"
        | x::_ ->
            reduce_rest s.reduce

    match s.Input with
    | x::inputRest ->
        match x with
        | Number ->
            shunting_yard s.reduceNumber
        | Open ->
            shunting_yard s.shift
        | Close ->
            shunting_yard (reduce_to_Open s)
        | Operator ->
            shunting_yard (reduce_by_prec_and_shift x s)
    | [] -> reduce_rest s

[<EntryPoint>]
let main argv =
    let input = if argv.Length = 0 then "3 + 4 * 2 / ( 1 - 5 ) ^ 2 ^ 3".Split() else argv
    shunting_yard (State((Array.toList input), [], []))
    |> (fun s -> s.report ReduceStack)
    0
```

{{out}}

```txt
                                       reduce [3] + 4 * 2 / ( 1 - 5 ) ^ 2 ^ 3
                        3               shift [+] 4 * 2 / ( 1 - 5 ) ^ 2 ^ 3
                        3    +         reduce [4] * 2 / ( 1 - 5 ) ^ 2 ^ 3
                      3 4    +          shift [*] 2 / ( 1 - 5 ) ^ 2 ^ 3
                      3 4    + *       reduce [2] / ( 1 - 5 ) ^ 2 ^ 3
                    3 4 2    + [*]     reduce / ( 1 - 5 ) ^ 2 ^ 3
                  3 4 2 *    +          shift [/] ( 1 - 5 ) ^ 2 ^ 3
                  3 4 2 *    + /        shift [(] 1 - 5 ) ^ 2 ^ 3
                  3 4 2 *    + / (     reduce [1] - 5 ) ^ 2 ^ 3
                3 4 2 * 1    + / (      shift [-] 5 ) ^ 2 ^ 3
                3 4 2 * 1    + / ( -   reduce [5] ) ^ 2 ^ 3
              3 4 2 * 1 5    + / ( [-] reduce ) ^ 2 ^ 3
            3 4 2 * 1 5 -    + /        shift [^] 2 ^ 3
            3 4 2 * 1 5 -    + / ^     reduce [2] ^ 3
          3 4 2 * 1 5 - 2    + / ^      shift [^] 3
          3 4 2 * 1 5 - 2    + / ^ ^   reduce [3]
        3 4 2 * 1 5 - 2 3    + / ^ [^] reduce
      3 4 2 * 1 5 - 2 3 ^    + / [^]   reduce
    3 4 2 * 1 5 - 2 3 ^ ^    + [/]     reduce
  3 4 2 * 1 5 - 2 3 ^ ^ /    [+]       reduce
3 4 2 * 1 5 - 2 3 ^ ^ / +
```



## Fortran


### The plan

The basic idea comes from noting that operands and operators alternate, so the plan is to flip-flop between two states. Similarly, the "shunting" aspect comes from shunting incoming operators to a stack providing that higher-precedence operators already in the stack are passed on first. Notice that stacked operators are sent forth if their precedence is greater than ''or equal to'' the incoming operator. This means that a sequence such as 1 + 2 + 3 will be handled as (1 + 2) + 3 rather than first stacking up all the adds. In other words, this is the tie-breaker rule of left-to-right for sequences of equal precedence. Similarly, when the end of the expression is reached, a space is treated as if it were an operator but one with very low precedence so that all stacked operators are rolled forth. Otherwise, the scan would have to be followed by code to flush the stack, an annoying duplication.

To handle the special behaviour for exponentiation whereby 1^2^3 is evaluated right-to-left as 1^(2^3) - contrary to the tiebreaker rule - when the ^ operator is stacked its stacked precedence is made one lower than its precedence when encountered. Thus, if a second ^ is encountered it will not cause its predecessor to be sent forth by the "greater than ''or equal to''" scheme; only later, lesser operators will do that. Unfortunately this means that the stack of deferred operators can grow without limit, as for the case of 1^2^3^4^... since the left-to-right rule will not be keeping it down. Otherwise, the height of the stack would be limited by the number of different precedences involved - were it not for bracketing: a sequence such as {[(...)]} will cause escalation too. A different approach to evaluation, and one often followed by humans (especially by those who have used a HP calculator) is to dive into the deepest part of the expression first then back out. For the example 3 + 4*2/(1 - 5)^2^3 that would mean starting with the (1 - 5) and not having the early + waiting on a stack - and, when actually evaluating, the 3 being in the data stack with a long wait for its use. This can be useful on computers calculating with a hardware stack where say the top two elements are in registers (as with the B6700) because less up&down means that more can be done via the registers without the delays of memory access. In doing this it is helpful to have divide-reverse and subtract-reverse operations, or an operation to exchange the top two stack elements. But the resulting sequence of actions would not be RPN style.

A similar trick with the precedences attends the stacking of open brackets as a pseudo-operator with a lower stacked precedence so that when later a closing bracket is found all subsequent operators on the stack (genuine ones, such as +-*/^) will be rolled, and then BALANCEBRA can compare the closing bracket to the possibly long-ago opening bracket. If all is well, the stacked operator will be vanished and the incoming closing bracket not stacked, nor will EMIT be invoked. Further, neither the opening nor closing bracket (encountered when expecting an operand), change the state of expecting an operand next: there is to be no alternation for them.

The task specification is that each token in the text is bounded by spaces. This means that a sequence such as -6 cannot appear so there is no confusion over unary minus versus dyadic minus (as in a - b) and whether or not the hyphen used for both should be interpreted as a part of the number or as an operator applied to a number. Thus, -6^2 might be (-6)^2 or -(6^2) instead. Following the source style of Fortran, this scanner disregards all spaces nor does it require spaces between tokens.

The example expression uses only single-digit numbers, so there was a temptation to employ a DO-loop stepping through the text, but that would require a special flush stage after reaching the end of the text. This could be avoided by having the loop run <code>DO L = 1,LEN(TEXT) + 1</code> for the extra step, but that would require annoying checks within the loop for L > LEN(TEXT) to prevent out-of-bounds accessing. So, abandon the DO-loop approach and instead allow for surges forward such as for multi-digit numbers - or for multi-character operators (such as <=) or even, for names of variables. However, only integers are allowed for here. The syntax of allowable floating-point numbers is quite complex and a logical function EATREAL(V) requires over a hundred lines, which would swamp the source more directly devoted to the project. It would of course be smaller if no errors were checked for nor complaints made, but testing the backwards logic of the algorithm is quite tricky when it is not working correctly because of mistakes or omissions, so statements of affront were helpful, and would belong in a "production" version anyway.

Having abandoned the idea of a scan-in-place via the introduction of FORASIGN to span the next token, having two calls is not troublesome so the token processing can be done in two successive stages: one for operands, then after the second FORASIGN, another for operators. This in turn allows a GO TO to retry the appropriate stage should bracketing be encountered. Alternatively, the GO TO could be avoided by having a state WANTOPERAND plus a test to select which of the two states is required, followed by a state flip. And for brackets, the state would have to be flipped so that the subsequent flip would not change it. This is too much to pay for the approval of those disliking GO TO statements.

F90 style has been used, in part because of the convenience of shared data and data aggregates such as SYMBOL however, these could be replaced by suitable COMMON statements and data structures can be replaced by a collection of separate variables whose names are structured. The ability to place a subroutine inside another subroutine so as to share local context is likewise a mere convenience.


### Source


```Fortran
      MODULE COMPILER	!At least of arithmetic expressions.
       INTEGER KBD,MSG		!I/O units.

       INTEGER ENUFF		!How long s a piece of string?
       PARAMETER (ENUFF = 66)	!This long.
       CHARACTER*(ENUFF) RP	!Holds the Reverse Polish Notation.
       INTEGER LR		!And this is its length.

       INTEGER		OPSYMBOLS		!Recognised operator symbols.
       PARAMETER	(OPSYMBOLS = 11)	!There are also some associates.
       TYPE SYMB		!To recognise symbols and carry associated information.
        CHARACTER*1	IS		!Its text. Careful with the trailing space and comparisons.
        INTEGER*1	PRECEDENCE	!Controls the order of evaluation.
        CHARACTER*48	USAGE		!Description.
       END TYPE SYMB		!The cross-linkage of precedences is tricky.
       TYPE(SYMB) SYMBOL(0:OPSYMBOLS)	!Righto, I'll have some.
       PARAMETER (SYMBOL =(/	!Note that "*" is not to be seen as a match to "**".
     o  SYMB(" ", 0,"Not recognised as an operator's symbol."),
     1  SYMB(" ", 1,"separates symbols and aids legibility."),
     2  SYMB(")", 4,"opened with ( to bracket a sub-expression."),
     3  SYMB("]", 4,"opened with [ to bracket a sub-expression."),
     4  SYMB("}", 4,"opened with { to bracket a sub-expression."),
     5  SYMB("+",11,"addition, and unary + to no effect."),
     6  SYMB("-",11,"subtraction, and unary - for neg. numbers."),
     7  SYMB("*",12,"multiplication."),
     8  SYMB("×",12,"multiplication, if you can find this."),
     9  SYMB("/",12,"division."),
     o  SYMB("÷",12,"division for those with a fancy keyboard."),
C                13 is used so that stacked ^ will have lower priority than incoming ^, thus delivering right-to-left evaluation.
     1  SYMB("^",14,"raise to power. Not recognised is **.")/))
       CHARACTER*3	BRAOPEN,BRACLOSE	!Three types are allowed.
       PARAMETER	(BRAOPEN = "([{", BRACLOSE = ")]}")	!These.
       INTEGER		BRALEVEL		!In and out, in and out. That's the game.
       INTEGER		PRBRA,PRPOW		!Special double values.
       PARAMETER (PRBRA = SYMBOL( 3).PRECEDENCE)	!Bracketing
       PARAMETER (PRPOW = SYMBOL(11).PRECEDENCE)	!And powers refer leftwards.

       CHARACTER*10 DIGIT		!Numberish is a bit more complex.
       PARAMETER (DIGIT = "0123456789")	!But this will do for integers.

       INTEGER STACKLIMIT		!How high is a stack?
       PARAMETER (STACKLIMIT = 66)	!This should suffice.
       TYPE DEFERRED			!I need a siding for lower-precedence operations.
        CHARACTER*1	OPC		!The operation code.
        INTEGER*1	PRECEDENCE	!Its precedence in the siding may differ.
       END TYPE DEFERRED		!Anyway, that's enough.
       TYPE(DEFERRED) OPSTACK(0:STACKLIMIT)	!One siding, please.
       INTEGER OSP			!The operation stack pointer.

       INTEGER INCOMING,TOKENTYPE,NOTHING,ANUMBER,OPENBRA,HUH		!Some mnemonics.
       PARAMETER (NOTHING = 0, ANUMBER = -1, OPENBRA = -2, HUH = -3)	!The ordering is not arbitrary.
       CONTAINS	!Now to mess about.
        SUBROUTINE EMIT(STUFF)	!The objective is to produce some RPN text.
         CHARACTER*(*) STUFF	!The term of the moment.
         INTEGER L		!A length.
          WRITE (MSG,1) STUFF	!Announce.
    1     FORMAT ("Emit  ",A)	!Whatever it is.
          IF (STUFF.EQ."") RETURN	!Ha ha.
          L = LEN(STUFF)	!So, how much is there to append?
          IF (LR + L.GE.ENUFF) STOP "Too much RPN for RP!"	!Perhaps too much.
          IF (LR.GT.0) THEN	!Is there existing stuff?
            LR = LR + 1			!Yes. Advance one,
            RP(LR:LR) = " "		!And place a space.
          END IF		!So much for separators.
          RP(LR + 1:LR + L) = STUFF	!Place the stuff.
          LR = LR + L			!Count it in.
        END SUBROUTINE EMIT	!Simple enough, if a bit finicky.

        SUBROUTINE STACKOP(C,P)	!Push an item into the siding.
         CHARACTER*1 C	!The operation code.
         INTEGER P	!Its precedence.
          OSP = OSP + 1		!Stacking up...
          IF (OSP.GT.STACKLIMIT) STOP "OSP overtopped!"	!Perhaps not.
          OPSTACK(OSP).OPC = C		!Righto,
          OPSTACK(OSP).PRECEDENCE = P	!The deed is simple.
          WRITE (MSG,1) C,OPSTACK(1:OSP)	!Announce.
    1     FORMAT ("Stack ",A1,9X,",OpStk=",33(A1,I2:","))
        END SUBROUTINE STACKOP	!So this is more for mnemonic ease.

        LOGICAL FUNCTION COMPILE(TEXT)	!A compiler confronts a compiler!
         CHARACTER*(*) TEXT	!To be inspected.
         INTEGER L1,L2		!Fingers for the scan.
         CHARACTER*1 C		!Character of the moment.
         INTEGER HAPPY		!Ah, shades of mood.
          LR = 0		!No output yet.
          OSP = 0		!Nothing stacked.
          OPSTACK(0).OPC = ""		!Prepare a bouncer.
          OPSTACK(0).PRECEDENCE = 0	!So that loops won't go past.
          BRALEVEL = 0		!None seen.
          HAPPY = +1		!Nor any problems.
          L2 = 1		!Syncopation: one past the end of the previous token.
Chew into an operand, possibly obstructed by an open bracket.
  100     CALL FORASIGN		!Find something to inspect.
          IF (TOKENTYPE.EQ.NOTHING) THEN	!Run off the end?
            IF (OSP.GT.0) CALL GRUMP("Another operand or one of "	!E.g. "1 +".
     1       //BRAOPEN//" is expected.")				!Give a hint, because stacked stuff awaits.
          ELSE IF (TOKENTYPE.EQ.ANUMBER) THEN	!If a number,
            CALL EMIT(TEXT(L1:L2 - 1))			!Roll all its digits.
          ELSE IF (TOKENTYPE.EQ.OPENBRA) THEN	!Starting a sub-expression?
            CALL STACKOP(C,PRBRA - 1)			!Thus ( has less precedence than ).
            GO TO 100					!And I still want an operand.
C         ELSE IF (TOKENTYPE.EQ.ANAME) THEN	!Name of something?
C           CALL EMIT(TEXT(L1:L2 - 1))			!Roll it.
          ELSE					!No further options.
            CALL GRUMP("Huh? Unexpected "//C)		!Probably something like "1 + +"
          END IF				!Righto, finished with operands.
Chase after an operator, possibly interrupted by a close bracket,.
  200     CALL FORASIGN		!Find something to inspect.
          IF (TOKENTYPE.LT.0) THEN	!But, have I an operand-like token instead?
            CALL GRUMP("Operator expected, not "//C)	!It seems so.
           ELSE			!Normally, an operator is to hand. Possibly a NOTHING, though.
            WRITE (MSG,201) C,INCOMING,OPSTACK(1:OSP)	!Document it.
  201       FORMAT ("Oprn=>",A1,"< Prec=",I2,		!Try to align with other output.
     1       ",OpStk=",33(A1,I2:","))			!So as not to clutter the display.
            DO WHILE(OPSTACK(OSP).PRECEDENCE .GE. INCOMING)	!Shunt higher-precedence stuff out.
              IF (OPSTACK(OSP).PRECEDENCE .EQ. PRBRA - 1)		!Only opening brackets have this  precedence.
     1         CALL GRUMP("Unbalanced "//OPSTACK(OSP).OPC)		!And they vanish only when meeting their closing bracket.
              CALL EMIT(OPSTACK(OSP).OPC)				!Otherwise we have an operator.
              OSP = OSP - 1						!It has gone forth.
            END DO						!On to the next.
            IF (TOKENTYPE.GT.NOTHING) THEN	!Now, only lower-precedence items are still in the stack.
              IF (INCOMING.EQ.PRBRA) THEN		!And this is a special arrival.
                CALL BALANCEBRA(C)			!It should match an earlier entry.
                BRALEVEL = BRALEVEL - 1			!Count it out.
                GO TO 200				!And I still haven't got an operator.
               ELSE				!All others are normal operators.
                IF (C.EQ."^") INCOMING = PRPOW - 1	!Special trick to cause leftwards association of x^2^3.
                CALL STACKOP(C,INCOMING)		!Shunt aside, to await the next arrival.
              END IF			!So much for that operator.
            END IF			!Providing it was not just an end-of-input flusher.
          END IF		!And not a misplaced operand.
Carry on?
          IF (HAPPY .GT. 0) GO TO 100 	!No problems, and not a nothing from the end of the text.
Completed.
          COMPILE = HAPPY.GE.0	!One hopes so.
         CONTAINS	!Now for some assistants.
          SUBROUTINE GRUMP(GROWL)	!There might be a problem.
           CHARACTER*(*) GROWL	!The fault.
            WRITE (MSG,1) GROWL	!Say it.
            IF (L1.GT. 1)        WRITE (MSG,1) "Tasty:",TEXT( 1:L1 - 1)	!Now explain the context.
            IF (L2.GT.L1)        WRITE (MSG,1) "Nasty:",TEXT(L1:L2 - 1)	!This is the token when trouble was found.
            IF (L2.LE.LEN(TEXT)) WRITE (MSG,1) "Misty:",TEXT(L2:)	!And this remains to be seen.
    1       FORMAT (4X,A,1X,A)	!A simple layout works nicely for reasonable-length texts.
            HAPPY = -1	.	!Just so.
          END SUBROUTINE GRUMP	!Enuogh said.

          SUBROUTINE BALANCEBRA(B)	!Perhaps a happy meeting.
           CHARACTER*1 B	!The closer.
           CHARACTER*1 O	!The putative opener.
           INTEGER IT,L		!Fingers.
           CHARACTER*88 GROWL	!A scratchpad.
            O = OPSTACK(OSP).OPC	!This should match B.
            WRITE (MSG,1) O,B		!Perhaps.
    1       FORMAT ("Match ",2A)	!Show what I've got, anyway.
            IT = INDEX(BRAOPEN,O)	!So, what sort did I save?
            IF (IT .EQ. INDEX(BRACLOSE,B)) THEN	!A friend?
              OSP = OSP - 1			!Yes. They vanish together.
             ELSE	!Otherwise, something is out of place.
              GROWL = "Unbalanced {[(...)]} bracketing! The closing "	!Alas.
     1         //B//" is unmatched."				!So, a mess.
              IF (IT.GT.0) GROWL(62:) =  "A "//BRACLOSE(IT:IT)	!Perhaps there had been no opening bracket.
     1         //" would be better."		!But if there had, this would be its friend.
              CALL GRUMP(GROWL)		!Take that!
            END IF			!So much for discrepancies.
          END SUBROUTINE BALANCEBRA	!But, hopefully, amity prevails.

          SUBROUTINE FORASIGN	!See what comes next.
           INTEGER I	!A stepper.
            L1 = L2		!Pick up where the previous scan left off.
   10       IF (L1.GT.LEN(TEXT)) THEN	!Are we off the end yet?
              C = ""			!Yes. Scrub the marker.
              L2 = L1			!TEXT(L1:L2 - 1) will be null.
              TOKENTYPE = NOTHING	!But this is to be checked first.
              INCOMING = SYMBOL(1).PRECEDENCE	!For flushing sidetracked operators.
              HAPPY = 0			!Fading away.
             ELSE	!Otherwise, there is grist.
Check for spaces and move past them.
              C = TEXT(L1:L1)	!Grab the first character of the prospective token.
              IF (C.LE." ") THEN	!Boring?
                L1 = L1 + 1		!Yes. Step past it.
                GO TO 10		!And look afresh.
              END IF		!Otherwise, L1 now fingers the start.
Caught something to inspect.
              L2 = L1 + 1		!This is one beyond. Just for digit strings.
              IF (INDEX(DIGIT,C).GT.0) THEN	!So, has one started?
                TOKENTYPE = ANUMBER 		!Yep.
   20           IF (L2.LE.LEN(TEXT)) THEN	!Probe ahead.
                  IF (INDEX(DIGIT,TEXT(L2:L2)).GT.0) THEN	!Another digit?
                    L2 = L2 + 1			!Yes. Leaving L1 fingering its start,
                    GO TO 20			!Chase its end.
                  END IF			!So much for another digit.
                END IF			!And checking against the end.
C             ELSE IF (INDEX(LETTERS,C).GT.0) THEN	!Some sort of name?
C               advance L2 while in NAMEISH.
              ELSE IF (INDEX(BRAOPEN,C).GT.0) THEN	!An open bracket?
                TOKENTYPE = OPENBRA			!Yep.
              ELSE			!Otherwise, anything else.
                DO I = OPSYMBOLS,1,-1	!Scan backwards, to find ** before *, if present.
                  IF (SYMBOL(I).IS .EQ. C) EXIT	!Found?
                END DO			!On to the next. A linear search will do.
                IF (I.LE.0) THEN	!Is it identified?
                  TOKENTYPE = HUH		!No.
                  INCOMING = SYMBOL(0).PRECEDENCE	!And this might provoke a flush.
                 ELSE			!If it is identified,
                  TOKENTYPE = I			!Then this is a positive number.
                  INCOMING = SYMBOL(I).PRECEDENCE	!And this is of interest.
                END IF			!Righto, anything has been identified, possibly as HUH.
              END IF		!So much for classification.
            END IF	!If there is something to see.
            WRITE (MSG,30) C,INCOMING,TOKENTYPE		!Announce.
   30       FORMAT ("Next=>",A1,"< Prec=",I2,",Ttype=",I2)	!C might be blank.
          END SUBROUTINE FORASIGN	!I call for a sign, and I see what?
        END FUNCTION COMPILE	!That's the main activity.
      END MODULE COMPILER	!So, enough of this.

      PROGRAM POKE
      USE COMPILER
      CHARACTER*66 TEXT
      LOGICAL HIC
      MSG = 6
      KBD = 5
      WRITE (MSG,1)
    1 FORMAT ("Produce RPN from infix...",/)

   10 WRITE (MSG,11)
   11 FORMAT("Infix: ",$)
      READ(KBD,12) TEXT
   12 FORMAT (A)
      IF (TEXT.EQ."") STOP "Enough."
      HIC = COMPILE(TEXT)
      WRITE (MSG,13) HIC,RP(1:LR)
   13 FORMAT (L6," RPN: >",A,"<")
      GO TO 10
      END
```



### Results

So that spaces can be seen, texts are marked off via >...< The operator stack is shown as a list of elements upwards, each element being the operator followed by its precedence. Notably, the ( has precedence 3, while ) has 4, while ^ in-the-text has precedence 14 but once on the stack it has precedence 13...

```txt

Produce RPN from infix...

Infix: 3 + 4 * 2 / ( 1 - 5 ) ^ 2 ^ 3
Next=>3< Prec= 0,Ttype=-1
Emit  3
Next=>+< Prec=11,Ttype= 5
Oprn=>+< Prec=11,OpStk=
Stack +         ,OpStk=+11
Next=>4< Prec=11,Ttype=-1
Emit  4
Next=>*< Prec=12,Ttype= 7
Oprn=>*< Prec=12,OpStk=+11
Stack *         ,OpStk=+11,*12
Next=>2< Prec=12,Ttype=-1
Emit  2
Next=>/< Prec=12,Ttype= 9
Oprn=>/< Prec=12,OpStk=+11,*12
Emit  *
Stack /         ,OpStk=+11,/12
Next=>(< Prec=12,Ttype=-2
Stack (         ,OpStk=+11,/12,( 3
Next=>1< Prec=12,Ttype=-1
Emit  1
Next=>-< Prec=11,Ttype= 6
Oprn=>-< Prec=11,OpStk=+11,/12,( 3
Stack -         ,OpStk=+11,/12,( 3,-11
Next=>5< Prec=11,Ttype=-1
Emit  5
Next=>)< Prec= 4,Ttype= 2
Oprn=>)< Prec= 4,OpStk=+11,/12,( 3,-11
Emit  -
Match ()
Next=>^< Prec=14,Ttype=11
Oprn=>^< Prec=14,OpStk=+11,/12
Stack ^         ,OpStk=+11,/12,^13
Next=>2< Prec=13,Ttype=-1
Emit  2
Next=>^< Prec=14,Ttype=11
Oprn=>^< Prec=14,OpStk=+11,/12,^13
Stack ^         ,OpStk=+11,/12,^13,^13
Next=>3< Prec=13,Ttype=-1
Emit  3
Next=> < Prec= 1,Ttype= 0
Oprn=> < Prec= 1,OpStk=+11,/12,^13,^13
Emit  ^
Emit  ^
Emit  /
Emit  +
     T RPN: >3 4 2 * 1 5 - 2 3 ^ ^ / +<
Infix:
Enough.

```



### A fuller symbol table

The odd values for the precedences of the operators is driven by the model source being for a compiler able to handle much more complex arithmetic statements involving logical operations, variables, functions (some, like Max(a,b,c,...) with an arbitrary number of parameters), assignment within an expression, and conditionals such as <code>IF ''condition'' THEN ''exp1'' ELSE ''exp2'' OWISE ''exp3'' FI</code> - a three-value logic is employed. Similarly, ? stands for a "not a number" and ! for "Infinity". The fuller symbol table is...

```Fortran
Caution! The apparent gaps in the sequence of precedence values in this table are *not* unused!
Cunning ploys with precedence allow parameter evaluation, and right-to-left order as in x**y**z.
       INTEGER		OPSYMBOLS		!Recognised operator symbols.
       PARAMETER	(OPSYMBOLS = 25)	!There are also some associates.
       TYPE SYMB		!To recognise symbols and carry associated information.
        CHARACTER*2	IS		!Its text. Careful with the trailing space and comparisons.
        INTEGER*1	PRECEDENCE	!Controls the order of evaluation.
        INTEGER*1	POPCOUNT	!Stack activity: a+b means + requires two in.
        CHARACTER*48	USAGE		!Description.
       END TYPE SYMB		!The cross-linkage of precedences is tricky.
       CHARACTER*5	IFPARTS(0:4)	!These appear when an operator would otherwise be expected.
       PARAMETER       (IFPARTS = (/"IF","THEN","ELSE","OWISE","FI"/))	!So, bend the usage of "operator".
       TYPE(SYMB) SYMBOL(-4:OPSYMBOLS)	!Righto, I'll have some.
       PARAMETER (SYMBOL =(/	!Note that "*" is not to be seen as a match to "**".
     4  SYMB("FI", 2,0,"the FI that ends an IF-statement."),	!These negative entries are not for name matching
     3  SYMB("Ow", 3,0,"the OWISE part of an IF-statement."),	!Which is instead done via IFPARTS
     2  SYMB("El", 3,0,"the ELSE part of an IF-statement."),	!But are here to take advantage of the structure in place.
     1  SYMB("Th", 3,0,"the THEN part of an IF-statement."),	!The IF is recognised separately, when expecting an operand.
     o  SYMB("  ", 0,0,"Not recognised as an operator's symbol."),
     1  SYMB("  ", 1,0,"separates symbols and aids legibility."),
C                  2 and 3 are used for the parts of an IF-statement. See PRIF.
C                  3 These precedences ensure the desired order of evaluation.
     2  SYMB(") ", 4,0,"opened with ( to bracket a sub-expression."),
     3  SYMB("] ", 4,0,"opened with [ to bracket a sub-expression."),
     4  SYMB("} ", 4,0,"opened with { to bracket a sub-expression."),
     5  SYMB(", ", 5,0,"continues a list of parameters to a function."),
C       SYMB(":=", 6,0,"marks an on-the-fly assignment of a result"), Identified differently... see PRREF.
     6  SYMB("| ", 7,2,"logical OR,  similar to addition."),
     7  SYMB("& ", 8,2,"logical AND, similar to multiplication."),
     8  SYMB("¬ ", 9,0,"logical NOT, similar to negation."),
     9  SYMB("= ",10,2,"tests for equality (beware decimal fractions)"),
     o  SYMB("< ",10,2,"tests strictly less than."),
     1  SYMB("> ",10,2,"tests strictly greater than."),
     2  SYMB("<>",10,2,"tests not equal (there is no 'not' key!)"),
     3  SYMB("¬=",10,2,"tests not equal if you can find a ¬ !"),
     4  SYMB("<=",10,2,"tests less than or equal."),
     5  SYMB(">=",10,2,"tests greater than or equal."),
     6  SYMB("+ ",11,2,"addition, and unary + to no effect."),
     7  SYMB("- ",11,2,"subtraction, and unary - for neg. numbers."),
     8  SYMB("* ",12,2,"multiplication."),
     9  SYMB("× ",12,2,"multiplication, if you can find this."),
     o  SYMB("/ ",12,2,"division."),
     1  SYMB("÷ ",12,2,"division for those with a fancy keyboard."),
     2  SYMB("\ ",12,2,"remainder a\b = a - truncate(a/b)*b; 11\3 = 2"),
C                 13 is used so that stacked ** will have lower priority than incoming **, thus delivering right-to-left evaluation.
     3  SYMB("^ ",14,2,"raise to power: also recognised is **."),	!Uses the previous precedence level also!
     4  SYMB("**",14,2,"raise to power: also recognised is ^."),
     5  SYMB("! ",15,1,"factorial, sortof, just for fun.")/))
```

The USAGE field is for when there is a request for help, and the response uses the scanner's actual symbol table entries to formulate its assistance, rather than roll forth a separately-prepared wad of text.


## Go

No error checking.  No extra credit output, but there are some comments in the code.

```go
package main

import (
    "fmt"
    "strings"
)

var input = "3 + 4 * 2 / ( 1 - 5 ) ^ 2 ^ 3"

var opa = map[string]struct {
    prec   int
    rAssoc bool
}{
    "^": {4, true},
    "*": {3, false},
    "/": {3, false},
    "+": {2, false},
    "-": {2, false},
}

func main() {
    fmt.Println("infix:  ", input)
    fmt.Println("postfix:", parseInfix(input))
}

func parseInfix(e string) (rpn string) {
    var stack []string // holds operators and left parenthesis
    for _, tok := range strings.Fields(e) {
        switch tok {
        case "(":
            stack = append(stack, tok) // push "(" to stack
        case ")":
            var op string
            for {
                // pop item ("(" or operator) from stack
                op, stack = stack[len(stack)-1], stack[:len(stack)-1]
                if op == "(" {
                    break // discard "("
                }
                rpn += " " + op // add operator to result
            }
        default:
            if o1, isOp := opa[tok]; isOp {
                // token is an operator
                for len(stack) > 0 {
                    // consider top item on stack
                    op := stack[len(stack)-1]
                    if o2, isOp := opa[op]; !isOp || o1.prec > o2.prec ||
                        o1.prec == o2.prec && o1.rAssoc {
                        break
                    }
                    // top item is an operator that needs to come off
                    stack = stack[:len(stack)-1] // pop it
                    rpn += " " + op              // add it to result
                }
                // push operator (the new one) to stack
                stack = append(stack, tok)
            } else { // token is an operand
                if rpn > "" {
                    rpn += " "
                }
                rpn += tok // add operand to result
            }
        }
    }
    // drain stack to result
    for len(stack) > 0 {
        rpn += " " + stack[len(stack)-1]
        stack = stack[:len(stack)-1]
    }
    return
}
```

Output:

```txt

infix:   3 + 4 * 2 / ( 1 - 5 ) ^ 2 ^ 3
postfix: 3 4 2 * 1 5 - 2 3 ^ ^ / +

```



## Haskell


Simple with zero error handling; some extra credit.


```Haskell
import Text.Printf

prec "^" = 4
prec "*" = 3
prec "/" = 3
prec "+" = 2
prec "-" = 2

leftAssoc "^" = False
leftAssoc _ = True

isOp (t:[]) = t `elem` "-+/*^"
isOp _      = False

simSYA xs = final ++ [lastStep]
  where final = scanl f ([],[],"") xs
        lastStep = (\(x,y,_) -> (reverse y ++ x, [], "")) $ last final
        f (out,st,_) t | isOp t    =
                         (reverse (takeWhile testOp st) ++ out
                         , (t:) $ (dropWhile testOp st), t)
                       | t == "("  = (out, "(":st, t)
                       | t == ")"  = (reverse (takeWhile (/="(") st) ++ out,
                                     tail $ dropWhile (/="(") st, t)
                       | otherwise = (t:out, st, t)
          where testOp x = isOp x && (leftAssoc t && prec t == prec x
                                      || prec t < prec x)

main = do
    a <- getLine
    printf "%30s%20s%7s" "Output" "Stack" "Token"
    mapM_ (\(x,y,z) -> printf "%30s%20s%7s\n"
            (unwords $ reverse x) (unwords y) z) $ simSYA $ words a
```


Output:

```txt
>main
3 + 4 * 2 / ( 1 - 5 ) ^ 2 ^ 3
                        Output               Stack  Token
                             3                          3
                             3                   +      +
                           3 4                   +      4
                           3 4                 * +      *
                         3 4 2                 * +      2
                       3 4 2 *                 / +      /
                       3 4 2 *               ( / +      (
                     3 4 2 * 1               ( / +      1
                     3 4 2 * 1             - ( / +      -
                   3 4 2 * 1 5             - ( / +      5
                 3 4 2 * 1 5 -                 / +      )
                 3 4 2 * 1 5 -               ^ / +      ^
               3 4 2 * 1 5 - 2               ^ / +      2
               3 4 2 * 1 5 - 2             ^ ^ / +      ^
             3 4 2 * 1 5 - 2 3             ^ ^ / +      3
     3 4 2 * 1 5 - 2 3 ^ ^ / +

```


A more complete version with typed input, output and stack; StateT + Control.Lens for stateful actions; Either for both invalid tokens on parsing and unmatched parens when converting; readLine support.


```Haskell
{-# LANGUAGE LambdaCase #-}
import Control.Applicative
import Control.Lens
import Control.Monad
import Control.Monad.Error
import Control.Monad.State
import System.Console.Readline

data InToken = InOp Op | InVal Int | LParen | RParen deriving (Show)
data OutToken = OutOp Op | OutVal Int
data StackElem = StOp Op | Paren deriving (Show)
data Op = Pow | Mul | Div | Add | Sub deriving (Show)
data Assoc = L | R deriving (Eq)

type Env = ([OutToken], [StackElem])
type RPNComp = StateT Env (Either String)

instance Show OutToken where
    show (OutOp x) = snd $ opInfo x
    show (OutVal v) = show v

opInfo = \case
    Pow -> (4, "^")
    Mul -> (3, "*")
    Div -> (3, "/")
    Add -> (2, "+")
    Sub -> (2, "-")

prec = fst . opInfo
leftAssoc Pow = False
leftAssoc _   = True

--Stateful actions
processToken :: InToken -> RPNComp ()
processToken = \case
    (InVal z) -> pushVal z
    (InOp op) -> pushOp op
    LParen    -> pushParen
    RParen    -> pushTillParen

pushTillParen :: RPNComp ()
pushTillParen = use _2 >>= \case
    []     -> throwError "Unmatched right parenthesis"
    (s:st) -> case s of
         StOp o -> _1 %= (OutOp o:) >> _2 %= tail >> pushTillParen
         Paren  -> _2 %= tail

pushOp :: Op -> RPNComp ()
pushOp o = use _2 >>= \case
    [] -> _2 .= [StOp o]
    (s:st) -> case s of
        (StOp o2) -> if leftAssoc o && prec o == prec o2
                     || prec o < prec o2
                     then _1 %= (OutOp o2:) >> _2 %= tail >> pushOp o
                     else _2 %= (StOp o:)
        Paren     -> _2 %= (StOp o:)

pushVal :: Int -> RPNComp ()
pushVal n = _1 %= (OutVal n:)

pushParen :: RPNComp ()
pushParen = _2 %= (Paren:)

--Run StateT
toRPN :: [InToken] -> Either String [OutToken]
toRPN xs = evalStateT process ([],[])
    where process = mapM_ processToken xs
                      >> get >>= \(a,b) -> (reverse a++) <$> (mapM toOut b)
          toOut :: StackElem -> RPNComp OutToken
          toOut (StOp o) = return $ OutOp o
          toOut Paren    = throwError "Unmatched left parenthesis"

--Parsing
readTokens :: String -> Either String [InToken]
readTokens = mapM f . words
    where f = let g = return . InOp in \case {
            "^" -> g Pow; "*" -> g Mul; "/" -> g Div;
            "+" -> g Add; "-" -> g Sub; "(" -> return LParen;
            ")" -> return RParen;
            a   -> case reads a of
                [] -> throwError $ "Invalid token `" ++ a ++ "`"
                [(_,x:[])] -> throwError $ "Invalid token `" ++ a ++ "`"
                [(v,[])]    -> return $ InVal v }

--Showing
showOutput (Left msg) = msg
showOutput (Right xs) = unwords $ map show xs

main = do
    a <- readline "Enter expression: "
    case a of
        Nothing -> putStrLn "Please enter a line" >> main
        Just "exit" -> return ()
        Just l      -> addHistory l >> case readTokens l of
            Left msg -> putStrLn msg >> main
            Right ts -> putStrLn (showOutput (toRPN ts)) >> main

```



```txt
Enter expression: 3 + ( ( 4 + 5 )
Unmatched left parenthesis
Enter expression: 3 + ( 4 + 5 ) )
Unmatched right parenthesis
Enter expression: 3 + ( alan + 5 )
Invalid token `alan`
Enter expression: 3 + ( 4 + 5 )
3 4 5 + +
Enter expression: 3 + 4 * 2 / ( 1 - 5 ) ^ 2 ^ 3
3 4 2 * 1 5 - 2 3 ^ ^ / +

```


=={{header|Icon}} and {{header|Unicon}}==

```Icon
procedure main()
   infix := "3 + 4 * 2 / ( 1 - 5 ) ^ 2 ^ 3"
   printf("Infix = %i\n",infix)
   printf("RPN   = %i\n",Infix2RPN(infix))
end

link printf

record op_info(pr,as)            # p=precedence, a=associativity (left=null)

procedure Infix2RPN(expr)        #: Infix to RPN parser - shunting yard
static oi
initial {
   oi := table()                                # precedence & associativity
   every oi[!"+-"]   := op_info(2)                    # 2L
   every oi[!"*/"]   := op_info(3)                    # 3L
   oi["^"]           := op_info(4,1)                  # 4R
   }

   ostack := []                                       # operator stack
   rpn    := ""                                       # rpn

   pat := sprintf("%%5s  :  %%-%ds  :  %%s\n",*expr)  # fmt
   printf(pat,"Token","Output","Op Stack")            # header

   expr ? until pos(0) do {                     # while tokens
      tab(many(' '))                                  # consume any seperator
      token := tab(upto(' ')|0)                       # get token
      printf(pat,token,rpn,list2string(ostack))       # report
      if token := numeric(token) then           # ... numeric
         rpn ||:= token || " "
      else
         if member(oi,token) then {             # ... operator
            while member(oi,op2 := ostack[1]) &
                  ( /oi[token].as & oi[token].pr <= oi[op2].pr ) |
                  ( \oi[token].as & oi[token].pr <  oi[op2].pr ) do
               rpn ||:= pop(ostack) || " "
            push(ostack,token)
            }
         else                                   # ... parenthesis
            if token == "(" then
               push(ostack,token)
            else if token == ")" then {
               until ostack[1] == "(" do
                  rpn ||:= pop(ostack) || " " |
                     stop("Unbalanced parenthesis")
               pop(ostack)                            # discard "("
               }
      }

   while token := pop(ostack) do                # ... input exhausted
      if token == ("("|")") then stop("Unbalanced parenthesis")
      else {
         rpn ||:= token || " "
         printf(pat,"",rpn,list2string(ostack))
         }

   return rpn
end

procedure list2string(L)         #: format list as a string
   every (s := "[ ") ||:= !L || " "
   return s || "]"
end
```


{{libheader|Icon Programming Library}}
[http://www.cs.arizona.edu/icon/library/src/procs/printf.icn printf.icn provides formatting]

Output:
```txt
Infix = "3 + 4 * 2 / ( 1 - 5 ) ^ 2 ^ 3"
Token  :  Output                         :  Op Stack
    3  :                                 :  [ ]
    +  :  3                              :  [ ]
    4  :  3                              :  [ + ]
    *  :  3 4                            :  [ + ]
    2  :  3 4                            :  [ * + ]
    /  :  3 4 2                          :  [ * + ]
    (  :  3 4 2 *                        :  [ / + ]
    1  :  3 4 2 *                        :  [ ( / + ]
    -  :  3 4 2 * 1                      :  [ ( / + ]
    5  :  3 4 2 * 1                      :  [ - ( / + ]
    )  :  3 4 2 * 1 5                    :  [ - ( / + ]
    ^  :  3 4 2 * 1 5 -                  :  [ / + ]
    2  :  3 4 2 * 1 5 -                  :  [ ^ / + ]
    ^  :  3 4 2 * 1 5 - 2                :  [ ^ / + ]
    3  :  3 4 2 * 1 5 - 2                :  [ ^ ^ / + ]
       :  3 4 2 * 1 5 - 2 3 ^            :  [ ^ / + ]
       :  3 4 2 * 1 5 - 2 3 ^ ^          :  [ / + ]
       :  3 4 2 * 1 5 - 2 3 ^ ^ /        :  [ + ]
       :  3 4 2 * 1 5 - 2 3 ^ ^ / +      :  [ ]
RPN   = "3 4 2 * 1 5 - 2 3 ^ ^ / + "
```



## J

Code

```J

NB. j does not have a verb based precedence.
NB. j evaluates verb noun sequences from right to left.
NB. Seriously.  18 precedence levels in C++ .

display=: ([: : (smoutput@:(, [: ; ' '&,&.>@:{:@:|:))) :: empty

Display=: adverb define
:
m display^:(0 -.@:-: x)y
)

NB. Queue, Stack, Pop: m literal name of vector to use.  verbose unless x is 0.
NB. Implementation includes display, group push and pop not available in the RC FIFO & LIFO pages
NB. As adverbs, these definitions work with any global variable.
NB. Pop needs the feature, and it helps with display as well.
Queue=: adverb define                   NB. enqueue y
('m'~)=: y ,~ (m~)
EMPTY
:
x (m,' queue')Display y
m Queue y
)

Stack=: adverb define                   NB. Stack y
('m'~)=: (|.y) , (m~)
EMPTY
:
x (m,' stack')Display y
m Stack y
)

Pop=: adverb define                     NB. Pop y items
0 m Pop y
:
y=. 0 {:@:, y                          NB. if y is empty use 0 instead
rv=. y {. (m~)
('m'~)=: y }. (m~)
x (m,' pop') Display rv
rv
)

NB. tests
TEST=: ''
'TEST'Stack'abc'
'TEST'Stack'de'
assert 'edc' -: 'TEST'Pop 3
assert 'ba' -: 'TEST'Pop 2
assert 0 (= #) TEST
'TEST'Queue'abc'
'TEST'Queue'de'
assert 'ab' -: 'TEST'Pop 2
assert 'cde' -: 'TEST'Pop 3
assert 0 (= #) TEST

any=: +./

DIGITS=: a. {~ 48+i.10                  NB. ASCII 48--57
precedence_oppression=: <;._1' +- */ ^ ( ) ',DIGITS
associativity=: 'xLLRxxL'

classify=: {:@:I.@:(1 , any@e.&>)&precedence_oppression

NB. The required tokens are also tokens in j.
NB. Use the default sequential machine ;: for lexical analysis.
rclex=: (;~ classify)"0@:;:


NB. numbers can be treated as highest precedence operators
number=: Q Queue                 NB. put numbers onto the output queue
left=: S Stack                   NB. push left paren onto the stack

NB. Until the token at the top of the stack is (, pop
NB. operators off the stack onto the output queue.
NB. Pop the left parenthesis from the stack, but not onto the output queue.
right=: 4 : 0    NB. If the token is a right parenthesis:
i=. (S~) (i. rclex) '('
if. i (= #) S~ do.
 smoutput'Check your parens!'
 throw.
end.
x Q Queue x S Pop i
x S Pop 1
EMPTY
)

NB. If the token is an operator, o1, then:
NB.
NB.  while there is  an operator token, o2, at the top  of the stack, and
NB.  either o1  is [[left-associative  and its precedence  is less  than or
NB.  equal to that  of o2]]"L*.<:", or o1 is  [[right-associative and its precedence
NB.  is less than that of o2]]"R*.<", pop o2 off the stack, onto the output queue;
NB.  [[the tally of adjacent leading truths]]"NCT"
NB.
NB.  push o1 onto the stack.
o=: 4 : 0
P=. 0 0 {:: y
L=. 'L' = P { associativity
operators=. ({.~ i.&(rclex'(')) S~
NB.    NCT          L*.<:     or       R*.<
i=. (+/@:(*./\)@:((L *. P&<:) +. ((-.L) *. P&<))@:(0&{::"1)) :: 0: operators
x Q Queue x S Pop i
x (S Stack) y
EMPTY
)

NB. terminating version of invalid
invalid=: 4 : 0
smoutput 'invalid token ',0 1 {:: y
throw.
)

NB. demonstrated invalid
invalid=: [: smoutput 'discarding invalid token ' , 0 1 {:: ]

NB. shunt_yard is a verb to implement shunt-yard parsing.
NB. verbose defaults to 0.  (quiet)
NB. use: verbosity shunt_yard_parse algebraic_string
shunt_yard_parse=: 0&$: : (4 : 0)

NB. j's data structure is array.  Rank 1 arrays (vectors)
NB. are just right for the stack and output queue.

'S Q'=: ;: 'OPERATOR OUTPUT'
('S'~)=:('Q'~)=: i.0 2

NB. Follow agenda for all tokens, result saved on global OUTPUT variable
x (invalid`o`o`o`left`right`number@.(0 0 {:: ])"2 ,:"1@:rclex) y
NB. x (invalid`o`o`o`left`right`o@.(0 0 {:: ])"2 ,:"1@:rclex) y  NB. numbers can be treated as operators
NB. check for junk on stack
if. (rclex'(') e. S~ do.
 smoutput'Check your other parens!'
 throw.
end.

NB. shift remaining operators onto the output queue
x Q Queue x S Pop # S~

NB. return the output queue
Q~
)

algebra_to_rpn=: {:@:|:@:shunt_yard_parse

fulfill_requirement=: ;@:(' '&,&.>)@:algebra_to_rpn

```

Demonstration

```J

   fulfill_requirement '3+4*2/(1-5)^2^3'
 3 4 2 * 1 5 - 2 3 ^ ^ / +

   shunt_yard_parse'3*)2+4)'
Check your parens!

   shunt_yard_parse'3*(2+4'
Check your other parens!

   algebra_to_rpn'1+x*(3+x)'
discarding invalid token x
discarding invalid token x
┌─┬─┬─┬─┬─┐
│1│3│+│*│+│
└─┴─┴─┴─┴─┘

   NB. Boxed form useful for evaluation
   algebra_to_rpn'0+666*(1+666*(2+666*(3)))'  NB. polynomial evaluation.
┌─┬───┬─┬───┬─┬───┬─┬─┬─┬─┬─┬─┬─┐
│0│666│1│666│2│666│3│*│+│*│+│*│+│
└─┴───┴─┴───┴─┴───┴─┴─┴─┴─┴─┴─┴─┘

   1 fulfill_requirement'3 + 4 * 2 / ( 1 - 5 ) ^ 2 ^ 3'
OUTPUT queue 3
OPERATOR pop
OUTPUT queue
OPERATOR stack +
OUTPUT queue 4
OPERATOR pop
OUTPUT queue
OPERATOR stack *
OUTPUT queue 2
OPERATOR pop *
OUTPUT queue *
OPERATOR stack /
OPERATOR stack (
OUTPUT queue 1
OPERATOR pop
OUTPUT queue
OPERATOR stack -
OUTPUT queue 5
OPERATOR pop -
OUTPUT queue -
OPERATOR pop (
OPERATOR pop
OUTPUT queue
OPERATOR stack ^
OUTPUT queue 2
OPERATOR pop
OUTPUT queue
OPERATOR stack ^
OUTPUT queue 3
OPERATOR pop ^ ^ / +
OUTPUT queue ^ ^ / +
 3 4 2 * 1 5 - 2 3 ^ ^ / +

```



## Java

{{works with|Java|7}}

```java
import java.util.Stack;

public class ShuntingYard {

    public static void main(String[] args) {
        String infix = "3 + 4 * 2 / ( 1 - 5 ) ^ 2 ^ 3";
        System.out.printf("infix:   %s%n", infix);
        System.out.printf("postfix: %s%n", infixToPostfix(infix));
    }

    static String infixToPostfix(String infix) {
        /* To find out the precedence, we take the index of the
           token in the ops string and divide by 2 (rounding down).
           This will give us: 0, 0, 1, 1, 2 */
        final String ops = "-+/*^";

        StringBuilder sb = new StringBuilder();
        Stack<Integer> s = new Stack<>();

        for (String token : infix.split("\\s")) {
            if (token.isEmpty())
                continue;
            char c = token.charAt(0);
            int idx = ops.indexOf(c);

            // check for operator
            if (idx != -1) {
                if (s.isEmpty())
                    s.push(idx);

                else {
                    while (!s.isEmpty()) {
                        int prec2 = s.peek() / 2;
                        int prec1 = idx / 2;
                        if (prec2 > prec1 || (prec2 == prec1 && c != '^'))
                            sb.append(ops.charAt(s.pop())).append(' ');
                        else break;
                    }
                    s.push(idx);
                }
            }
            else if (c == '(') {
                s.push(-2); // -2 stands for '('
            }
            else if (c == ')') {
                // until '(' on stack, pop operators.
                while (s.peek() != -2)
                    sb.append(ops.charAt(s.pop())).append(' ');
                s.pop();
            }
            else {
                sb.append(token).append(' ');
            }
        }
        while (!s.isEmpty())
            sb.append(ops.charAt(s.pop())).append(' ');
        return sb.toString();
    }
}
```


Output:


```txt
infix:   3 + 4 * 2 / ( 1 - 5 ) ^ 2 ^ 3
postfix: 3 4 2 * 1 5 - 2 3 ^ ^ / +
```



## JavaScript


```javascript
function Stack() {
  this.dataStore = [];
  this.top = 0;
  this.push = push;
  this.pop = pop;
  this.peek = peek;
  this.length = length;
}

function push(element) {
  this.dataStore[this.top++] = element;
}

function pop() {
  return this.dataStore[--this.top];
}

function peek() {
  return this.dataStore[this.top-1];
}

function length() {
  return this.top;
}

var infix = "3 + 4 * 2 / ( 1 - 5 ) ^ 2 ^ 3";
infix = infix.replace(/\s+/g, ''); // remove spaces, so infix[i]!=" "

var s = new Stack();
var ops = "-+/*^";
var precedence = {"^":4, "*":3, "/":3, "+":2, "-":2};
var associativity = {"^":"Right", "*":"Left", "/":"Left", "+":"Left", "-":"Left"};
var token;
var postfix = "";
var o1, o2;

for (var i = 0; i < infix.length; i++) {
  token = infix[i];
  if (token >= "0" && token <= "9") { // if token is operand (here limited to 0 <= x <= 9)
    postfix += token + " ";
  }
  else if (ops.indexOf(token) != -1) { // if token is an operator
    o1 = token;
    o2 = s.peek();
    while (ops.indexOf(o2)!=-1 && ( // while operator token, o2, on top of the stack
      // and o1 is left-associative and its precedence is less than or equal to that of o2
      (associativity[o1] == "Left" && (precedence[o1] <= precedence[o2]) ) ||
      // the algorithm on wikipedia says: or o1 precedence < o2 precedence, but I think it should be
      // or o1 is right-associative and its precedence is less than that of o2
      (associativity[o1] == "Right" && (precedence[o1] < precedence[o2]))
      )){
        postfix += o2 + " "; // add o2 to output queue
        s.pop(); // pop o2 of the stack
        o2 = s.peek(); // next round
    }
    s.push(o1); // push o1 onto the stack
  }
  else if (token == "(") { // if token is left parenthesis
    s.push(token); // then push it onto the stack
  }
  else if (token == ")") { // if token is right parenthesis
    while (s.peek() != "("){ // until token at top is (
      postfix += s.pop() + " ";
    }
    s.pop(); // pop (, but not onto the output queue
  }
}
postfix += s.dataStore.reverse().join(" ");
print(postfix);
```


Output:


```txt
infix:   3 + 4 * 2 / ( 1 - 5 ) ^ 2 ^ 3
postfix: 3 4 2 * 1 5 - 2 3 ^ ^ / +
```



## Julia

Translation from the Wikipedia reference article's pseudocode.

```julia

function parseinfix2rpn(s)
    outputq = []
    opstack = []
    infix = split(s)
    for tok in infix
        if all(isnumber, tok)
            push!(outputq, tok)
        elseif tok == "("
            push!(opstack, tok)
        elseif tok == ")"
            while !isempty(opstack) && (op = pop!(opstack)) != "("
               push!(outputq, op)
            end
        else # operator
            while !isempty(opstack)
                op = pop!(opstack)
                if Base.operator_precedence(Symbol(op)) > Base.operator_precedence(Symbol(tok)) ||
                   (Base.operator_precedence(Symbol(op)) ==
                     Base.operator_precedence(Symbol(tok)) && op != "^")
                    push!(outputq, op)
                else
                    push!(opstack, op)  # undo peek
                    break
                end
            end
            push!(opstack, tok)
        end
        println("The working output stack is $outputq")
    end
    while !isempty(opstack)
        if (op = pop!(opstack)) == "("
            throw("mismatched parentheses")
        else
            push!(outputq, op)
        end
    end
    outputq
end

teststring = "3 + 4 * 2 / ( 1 - 5 ) ^ 2 ^ 3"
println("\nResult: $teststring becomes $(join(parseinfix2rpn(teststring), ' '))")

```

{{output}}

```txt

The working output stack is Any["3"]
The working output stack is Any["3"]
The working output stack is Any["3", "4"]
The working output stack is Any["3", "4"]
The working output stack is Any["3", "4", "2"]
The working output stack is Any["3", "4", "2", "*"]
The working output stack is Any["3", "4", "2", "*"]
The working output stack is Any["3", "4", "2", "*", "1"]
The working output stack is Any["3", "4", "2", "*", "1"]
The working output stack is Any["3", "4", "2", "*", "1", "5"]
The working output stack is Any["3", "4", "2", "*", "1", "5", "-"]
The working output stack is Any["3", "4", "2", "*", "1", "5", "-"]
The working output stack is Any["3", "4", "2", "*", "1", "5", "-", "2"]
The working output stack is Any["3", "4", "2", "*", "1", "5", "-", "2"]
The working output stack is Any["3", "4", "2", "*", "1", "5", "-", "2", "3"]

Result: 3 + 4 * 2 / ( 1 - 5 ) ^ 2 ^ 3 becomes 3 4 2 * 1 5 - 2 3 ^ ^ / +

```



## Kotlin

{{trans|Java}}

```scala
// version 1.2.0

import java.util.Stack

/* To find out the precedence, we take the index of the
   token in the OPS string and divide by 2 (rounding down).
   This will give us: 0, 0, 1, 1, 2 */
const val OPS = "-+/*^"

fun infixToPostfix(infix: String): String {
    val sb = StringBuilder()
    val s = Stack<Int>()
    val rx = Regex("""\s""")
    for (token in infix.split(rx)) {
        if (token.isEmpty()) continue
        val c = token[0]
        val idx = OPS.indexOf(c)

        // check for operator
        if (idx != - 1) {
            if (s.isEmpty()) {
                s.push(idx)
            }
            else {
                while (!s.isEmpty()) {
                    val prec2 = s.peek() / 2
                    val prec1 = idx / 2
                    if (prec2 > prec1 || (prec2 == prec1 && c != '^')) {
                        sb.append(OPS[s.pop()]).append(' ')
                    }
                    else break
                }
                s.push(idx)
            }
        }
        else if (c == '(') {
            s.push(-2)  // -2 stands for '('
        }
        else if (c == ')') {
            // until '(' on stack, pop operators.
            while (s.peek() != -2) sb.append(OPS[s.pop()]).append(' ')
            s.pop()
        }
        else {
            sb.append(token).append(' ')
        }
    }
    while (!s.isEmpty()) sb.append(OPS[s.pop()]).append(' ')
    return sb.toString()
}

fun main(args: Array<String>) {
    val es = listOf(
        "3 + 4 * 2 / ( 1 - 5 ) ^ 2 ^ 3",
        "( ( 1 + 2 ) ^ ( 3 + 4 ) ) ^ ( 5 + 6 )"
    )
    for (e in es) {
        println("Infix : $e")
        println("Postfix : ${infixToPostfix(e)}\n")
    }
}
```


{{out}}

```txt

Infix : 3 + 4 * 2 / ( 1 - 5 ) ^ 2 ^ 3
Postfix : 3 4 2 * 1 5 - 2 3 ^ ^ / +

Infix : ( ( 1 + 2 ) ^ ( 3 + 4 ) ) ^ ( 5 + 6 )
Postfix : 1 2 + 3 4 + ^ 5 6 + ^

```



## Liberty BASIC


```lb

global stack$,queue$
stack$=""
queue$=""

in$ = "3 + 4 * 2 / ( 1 - 5 ) ^ 2 ^ 3"
print "Input:"
print in$

token$ = "#"
print "No", "token", "stack", "queue"

while 1
    i=i+1
    token$ = word$(in$, i)
    if token$ = "" then i=i-1: exit while
    print i, token$, reverse$(stack$), queue$

    select case
    case token$ = "("
        call stack.push token$

    case token$ = ")"
        while stack.peek$() <> "("
            'if stack is empty
            if stack$="" then print "Error: no matching '(' for token ";i: end
            call queue.push stack.pop$()
        wend
        discard$=stack.pop$()   'discard "("

    case isOperator(token$)
        op1$=token$
        while(isOperator(stack.peek$()))
            op2$=stack.peek$()
            select case
            case op2$<>"^" and precedence(op1$) = precedence(op2$)
                '"^" is the only right-associative operator
                call queue.push stack.pop$()
            case precedence(op1$) < precedence(op2$)
                call queue.push stack.pop$()
            case else
                exit while
            end select
        wend
        call stack.push op1$

    case else   'number
        'actually, wrong operator could end up here, like say %
        'If the token is a number, then add it to the output queue.
        call queue.push token$
    end select

wend

while stack$<>""
    if stack.peek$() = "(" then print "no matching ')'": end
    call queue.push stack.pop$()
wend

print "Output:"
while queue$<>""
    print queue.pop$();" ";
wend
print

end

'------------------------------------------
function isOperator(op$)
    isOperator = instr("+-*/^", op$)<>0 AND len(op$)=1
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

sub queue.push s$
    queue$=queue$+s$+"|"
end sub

function queue.pop$()
    'it does return empty on empty stack or queue
    queue.pop$=word$(queue$,1,"|")
    queue$=mid$(queue$,instr(queue$,"|")+1)
end function

function stack.pop$()
    'it does return empty on empty stack or queue
    stack.pop$=word$(stack$,1,"|")
    stack$=mid$(stack$,instr(stack$,"|")+1)
end function

function stack.peek$()
    'it does return empty on empty stack or queue
    stack.peek$=word$(stack$,1,"|")
end function

function reverse$(s$)
    reverse$ = ""
    token$="#"
    while token$<>""
        i=i+1
        token$=word$(s$,i,"|")
        reverse$ = token$;" ";reverse$
    wend
end function

```


{{out}}

```txt

Input:
3 + 4 * 2 / ( 1 - 5 ) ^ 2 ^ 3
No            token         stack         queue
1             3
2             +                           3|
3             4              +            3|
4             *              +            3|4|
5             2              + *          3|4|
6             /              + *          3|4|2|
7             (              + /          3|4|2|*|
8             1              + / (        3|4|2|*|
9             -              + / (        3|4|2|*|1|
10            5              + / ( -      3|4|2|*|1|
11            )              + / ( -      3|4|2|*|1|5|
12            ^              + /          3|4|2|*|1|5|-|
13            2              + / ^        3|4|2|*|1|5|-|
14            ^              + / ^        3|4|2|*|1|5|-|2|
15            3              + / ^ ^      3|4|2|*|1|5|-|2|
Output:
3 4 2 * 1 5 - 2 3 ^ ^ / +

```



## Mathematica


```Mathematica
rpn[str_] :=
  StringRiffle[
   ToString /@
    Module[{in = StringSplit[str], stack = {}, out = {}, next},
     While[in != {}, next = in[[1]]; in = in[[2 ;;]];
      Which[DigitQ[next], AppendTo[out, next], LetterQ[next],
       AppendTo[stack, next], next == ",",
       While[stack[[-1]] != "(", AppendTo[out, stack[[-1]]];
        stack = stack[[;; -2]]], next == "^", AppendTo[stack, "^"],
       next == "*",
       While[stack != {} && MatchQ[stack[[-1]], "^" | "*" | "/"],
        AppendTo[out, stack[[-1]]]; stack = stack[[;; -2]]];
       AppendTo[stack, "*"], next == "/",
       While[stack != {} && MatchQ[stack[[-1]], "^" | "*" | "/"],
        AppendTo[out, stack[[-1]]]; stack = stack[[;; -2]]];
       AppendTo[stack, "/"], next == "+",
       While[stack != {} &&
         MatchQ[stack[[-1]], "^" | "*" | "/" | "+" | "-"],
        AppendTo[out, stack[[-1]]]; stack = stack[[;; -2]]];
       AppendTo[stack, "+"], next == "-",
       While[stack != {} &&
         MatchQ[stack[[-1]], "^" | "*" | "/" | "+" | "-"],
        AppendTo[out, stack[[-1]]]; stack = stack[[;; -2]]];
       AppendTo[stack, "-"], next == "(", AppendTo[stack, "("],
       next == ")",
       While[stack[[-1]] =!= "(", AppendTo[out, stack[[-1]]];
        stack = stack[[;; -2]]]; stack = stack[[;; -2]];
       If[StringQ[stack[[-1]]], AppendTo[out, stack[[-1]]];
        stack = stack[[;; -2]]]]];
     While[stack != {}, AppendTo[out, stack[[-1]]];
      stack = stack[[;; -2]]]; out]];
Print[rpn["3 + 4 * 2 / ( 1 - 5 ) ^ 2 ^ 3"]];
```

{{out}}

```txt
3 4 2 * 1 5 - / 2 3 ^ ^ +
```



## Nim


```nim
import tables, strutils, sequtils, strformat

type operator = tuple[prec:int, rassoc:bool]

let ops = newTable[string, operator](
  [ ("^", (4, true )),
    ("*", (3, false)),
    ("/", (3, false)),
    ("+", (2, false)),
    ("-", (2, false)) ])

proc shuntRPN(tokens:seq[string]): string =
  var stack: seq[string]
  var op:string

  for token in tokens:
    case token
    of "(": stack.add token
    of ")":
      while stack.len > 0:
        op = stack.pop()
        if op == "(": break
        result &= op & " "
    else:
      if token in ops:
        while stack.len > 0:
          op = stack[^1]  # peek stack top
          if not (op in ops): break
          if ops[token].prec > ops[op].prec or (ops[token].rassoc and (ops[token].prec == ops[op].prec)):
            break
          discard stack.pop()
          result &= op & " "
        stack.add token
      else:
        result &= token & " "
    echo fmt"""{token:5}   {join(stack," "):18} {result:25} """

  while stack.len > 0:
    result &= stack.pop() & " "
    echo fmt"""        {join(stack," "):18} {result:25} """

let input = "3 + 4 * 2 / ( 1 - 5 ) ^ 2 ^ 3"

echo &"for infix expression: '{input}' \n", "\nTOKEN   OP STACK           RPN OUTPUT"
echo "postfix: ", shuntRPN(input.strip.split)
```


{{out}}

```txt
for infix expression: '3 + 4 * 2 / ( 1 - 5 ) ^ 2 ^ 3'

TOKEN   OP STACK           RPN OUTPUT
3                          3
+       +                  3
4       +                  3 4
*       + *                3 4
2       + *                3 4 2
/       + /                3 4 2 *
(       + / (              3 4 2 *
1       + / (              3 4 2 * 1
-       + / ( -            3 4 2 * 1
5       + / ( -            3 4 2 * 1 5
)       + /                3 4 2 * 1 5 -
^       + / ^              3 4 2 * 1 5 -
2       + / ^              3 4 2 * 1 5 - 2
^       + / ^ ^            3 4 2 * 1 5 - 2
3       + / ^ ^            3 4 2 * 1 5 - 2 3
        + / ^              3 4 2 * 1 5 - 2 3 ^
        + /                3 4 2 * 1 5 - 2 3 ^ ^
        +                  3 4 2 * 1 5 - 2 3 ^ ^ /
                           3 4 2 * 1 5 - 2 3 ^ ^ / +
postfix: 3 4 2 * 1 5 - 2 3 ^ ^ / +
```



## OCaml

{{works with|ocaml|4.04}}

```ocaml

type associativity = Left | Right;;


let prec op =
  match op with
  | "^" -> 4
  | "*" -> 3
  | "/" -> 3
  | "+" -> 2
  | "-" -> 2
  | _ -> -1;;


let assoc op =
  match op with
  | "^" -> Right
  | _ -> Left;;


let split_while p =
  let rec go ls xs =
    match xs with
    | x::xs' when p x -> go (x::ls) xs'
    | _ -> List.rev ls, xs
  in go [];;


let rec intercalate sep xs =
  match xs with
  | [] -> ""
  | [x] -> x
  | x::xs' -> x ^ sep ^ intercalate sep xs';;


let shunting_yard =
  let rec pusher stack queue tkns =
    match tkns with
    | [] -> List.rev queue @ stack
    | "("::tkns' -> pusher ("("::stack) queue tkns'
    | ")"::tkns' ->
        let mv, "("::stack' = split_while ((<>) "(") stack in
        pusher stack' (mv @ queue) tkns'
    | t::tkns' when prec t < 0 -> pusher stack (t::queue) tkns'
    | op::tkns' ->
        let mv_to_queue op2 =
          (match assoc op with
            | Left -> prec op <= prec op2
            | Right -> prec op < prec op2)
        in
        let mv, stack' = split_while mv_to_queue stack in
        pusher (op::stack') (mv @ queue) tkns'
  in pusher [] [];;


let () =
  let inp = read_line () in
  let tkns = String.split_on_char ' ' inp in
  let postfix = shunting_yard tkns in
  print_endline (intercalate " " postfix);;

```



## Perl

{{trans|Perl 6}}

```perl
my %prec = (
    '^' => 4,
    '*' => 3,
    '/' => 3,
    '+' => 2,
    '-' => 2,
    '(' => 1
);

my %assoc = (
    '^' => 'right',
    '*' => 'left',
    '/' => 'left',
    '+' => 'left',
    '-' => 'left'
);

sub shunting_yard {
    my @inp = split ' ', $_[0];
    my @ops;
    my @res;

    my $report = sub { printf "%25s    %-7s %10s %s\n", "@res", "@ops", $_[0], "@inp" };
    my $shift  = sub { $report->("shift @_");  push @ops, @_ };
    my $reduce = sub { $report->("reduce @_"); push @res, @_ };

    while (@inp) {
        my $token = shift @inp;
        if    ( $token =~ /\d/ ) { $reduce->($token) }
        elsif ( $token eq '(' )  { $shift->($token) }
        elsif ( $token eq ')' ) {
            while ( @ops and "(" ne ( my $x = pop @ops ) ) { $reduce->($x) }
        } else {
            my $newprec = $prec{$token};
            while (@ops) {
                my $oldprec = $prec{ $ops[-1] };
                last if $newprec > $oldprec;
                last if $newprec == $oldprec and $assoc{$token} eq 'right';
                $reduce->( pop @ops );
            }
            $shift->($token);
        }
    }
    $reduce->( pop @ops ) while @ops;
    @res;
}

local $, = " ";
print shunting_yard '3 + 4 * 2 / ( 1 - 5 ) ^ 2 ^ 3';

```

{{out}}

```txt
                                       reduce 3 + 4 * 2 / ( 1 - 5 ) ^ 2 ^ 3
                        3               shift + 4 * 2 / ( 1 - 5 ) ^ 2 ^ 3
                        3    +         reduce 4 * 2 / ( 1 - 5 ) ^ 2 ^ 3
                      3 4    +          shift * 2 / ( 1 - 5 ) ^ 2 ^ 3
                      3 4    + *       reduce 2 / ( 1 - 5 ) ^ 2 ^ 3
                    3 4 2    +         reduce * ( 1 - 5 ) ^ 2 ^ 3
                  3 4 2 *    +          shift / ( 1 - 5 ) ^ 2 ^ 3
                  3 4 2 *    + /        shift ( 1 - 5 ) ^ 2 ^ 3
                  3 4 2 *    + / (     reduce 1 - 5 ) ^ 2 ^ 3
                3 4 2 * 1    + / (      shift - 5 ) ^ 2 ^ 3
                3 4 2 * 1    + / ( -   reduce 5 ) ^ 2 ^ 3
              3 4 2 * 1 5    + / (     reduce - ^ 2 ^ 3
            3 4 2 * 1 5 -    + /        shift ^ 2 ^ 3
            3 4 2 * 1 5 -    + / ^     reduce 2 ^ 3
          3 4 2 * 1 5 - 2    + / ^      shift ^ 3
          3 4 2 * 1 5 - 2    + / ^ ^   reduce 3
        3 4 2 * 1 5 - 2 3    + / ^     reduce ^
      3 4 2 * 1 5 - 2 3 ^    + /       reduce ^
    3 4 2 * 1 5 - 2 3 ^ ^    +         reduce /
  3 4 2 * 1 5 - 2 3 ^ ^ /              reduce +
3 4 2 * 1 5 - 2 3 ^ ^ / +
```



## Perl 6


```perl6
my %prec =
    '^' => 4,
    '*' => 3,
    '/' => 3,
    '+' => 2,
    '-' => 2,
    '(' => 1;

my %assoc =
    '^' => 'right',
    '*' => 'left',
    '/' => 'left',
    '+' => 'left',
    '-' => 'left';

sub shunting-yard ($prog) {
    my @inp = $prog.words;
    my @ops;
    my @res;

    sub report($op) { printf "%25s    %-7s %10s %s\n", ~@res, ~@ops, $op, ~@inp }
    sub shift($t)  { report( "shift $t"); @ops.push: $t }
    sub reduce($t) { report("reduce $t"); @res.push: $t }

    while @inp {
	given @inp.shift {
	    when /\d/ { reduce $_ };
	    when '(' { shift $_ }
	    when ')' { while @ops and (my $x = @ops.pop and $x ne '(') { reduce $x } }
	    default {
		my $newprec = %prec{$_};
		while @ops {
		    my $oldprec = %prec{@ops[*-1]};
		    last if $newprec > $oldprec;
		    last if $newprec == $oldprec and %assoc{$_} eq 'right';
		    reduce @ops.pop;
		}
		shift $_;
	    }
	}
    }
    reduce @ops.pop while @ops;
    @res;
}

say shunting-yard '3 + 4 * 2 / ( 1 - 5 ) ^ 2 ^ 3';
```

{{out}}

```txt
                                       reduce 3 + 4 * 2 / ( 1 - 5 ) ^ 2 ^ 3
                        3               shift + 4 * 2 / ( 1 - 5 ) ^ 2 ^ 3
                        3    +         reduce 4 * 2 / ( 1 - 5 ) ^ 2 ^ 3
                      3 4    +          shift * 2 / ( 1 - 5 ) ^ 2 ^ 3
                      3 4    + *       reduce 2 / ( 1 - 5 ) ^ 2 ^ 3
                    3 4 2    +         reduce * ( 1 - 5 ) ^ 2 ^ 3
                  3 4 2 *    +          shift / ( 1 - 5 ) ^ 2 ^ 3
                  3 4 2 *    + /        shift ( 1 - 5 ) ^ 2 ^ 3
                  3 4 2 *    + / (     reduce 1 - 5 ) ^ 2 ^ 3
                3 4 2 * 1    + / (      shift - 5 ) ^ 2 ^ 3
                3 4 2 * 1    + / ( -   reduce 5 ) ^ 2 ^ 3
              3 4 2 * 1 5    + / (     reduce - ^ 2 ^ 3
            3 4 2 * 1 5 -    + /        shift ^ 2 ^ 3
            3 4 2 * 1 5 -    + / ^     reduce 2 ^ 3
          3 4 2 * 1 5 - 2    + / ^      shift ^ 3
          3 4 2 * 1 5 - 2    + / ^ ^   reduce 3
        3 4 2 * 1 5 - 2 3    + / ^     reduce ^
      3 4 2 * 1 5 - 2 3 ^    + /       reduce ^
    3 4 2 * 1 5 - 2 3 ^ ^    +         reduce /
  3 4 2 * 1 5 - 2 3 ^ ^ /              reduce +
3 4 2 * 1 5 - 2 3 ^ ^ / +
```



## Phix

{{Trans|Go}}

```Phix
integer show_workings = 1

constant operators  = {"^","*","/","+","-"},
         precedence = { 4,  3,  3,  2,  2 }

procedure shunting_yard(string infix, string rpn)
string res = "", sep = "", top
sequence stack = {}
sequence ops = split(substitute_all(infix,{"(",")"},{" ( "," ) "}),' ',no_empty:=1,limit:=0)
    printf(1,"Infix input: %-30s%s", {infix,iff(show_workings?'\n':'\t')})
    for i=1 to length(ops) do
        string op = ops[i]
        if op="(" then
            stack = append(stack,op)
        elsif op=")" then
            while 1 do
                top = stack[$]
                stack = stack[1..$-1]
                if top="(" then exit end if
                res &= sep&top
                sep = " "
            end while
        else
            integer k = find(op,operators)
            if k!=0 then
                integer prec = precedence[k]
                while length(stack) do
                    top = stack[$]
                    k = find(top,operators)
                    if k=0 or prec>precedence[k]
                    or (top="^" and prec=precedence[k]) then
                        exit
                    end if
                    stack = stack[1..$-1]
                    res &= sep&top
                    sep = " "
                end while
                stack = append(stack,op)
            else
                res &= sep&op
                sep = " "
            end if
        end if
        if show_workings then
            ?{op,stack,res}
        end if
    end for
    for i=length(stack) to 1 by -1 do
        string op = stack[i]
        res &= sep&op
        sep = " "
    end for
    printf(1,"result: %-22s [%s]\n", {res,iff(res=rpn?"ok","**ERROR**")})
end procedure

shunting_yard("3 + 4 * 2 / (1 - 5) ^ 2 ^ 3","3 4 2 * 1 5 - 2 3 ^ ^ / +")
show_workings = 0
shunting_yard("((1 + 2) ^ (3 + 4)) ^ (5 + 6)","1 2 + 3 4 + ^ 5 6 + ^")
shunting_yard("(1 + 2) ^ (3 + 4) ^ (5 + 6)","1 2 + 3 4 + 5 6 + ^ ^")
shunting_yard("((3 ^ 4) ^ 2 ^ 9) ^ 2 ^ 5","3 4 ^ 2 9 ^ ^ 2 5 ^ ^")
shunting_yard("(1 + 4) * (5 + 3) * 2 * 3","1 4 + 5 3 + * 2 * 3 *")
shunting_yard("1 * 2 * 3 * 4","1 2 * 3 * 4 *")
shunting_yard("1 + 2 + 3 + 4","1 2 + 3 + 4 +")
shunting_yard("(1 + 2) ^ (3 + 4)","1 2 + 3 4 + ^")
shunting_yard("(5 ^ 6) ^ 7","5 6 ^ 7 ^")
shunting_yard("5 ^ 4 ^ 3 ^ 2","5 4 3 2 ^ ^ ^")
shunting_yard("1 + 2 + 3","1 2 + 3 +")
shunting_yard("1 ^ 2 ^ 3","1 2 3 ^ ^")
shunting_yard("(1 ^ 2) ^ 3","1 2 ^ 3 ^")
shunting_yard("1 - 1 + 3","1 1 - 3 +")
shunting_yard("3 + 1 - 1","3 1 + 1 -")
shunting_yard("1 - (2 + 3)","1 2 3 + -")
shunting_yard("4 + 3 + 2","4 3 + 2 +")
shunting_yard("5 + 4 + 3 + 2","5 4 + 3 + 2 +")
shunting_yard("5 * 4 * 3 * 2","5 4 * 3 * 2 *")
shunting_yard("5 + 4 - (3 + 2)","5 4 + 3 2 + -")
shunting_yard("3 - 4 * 5","3 4 5 * -")
shunting_yard("3 * (4 - 5)","3 4 5 - *")
shunting_yard("(3 - 4) * 5","3 4 - 5 *")
shunting_yard("4 * 2 + 1 - 5","4 2 * 1 + 5 -")
shunting_yard("4 * 2 / (1 - 5) ^ 2","4 2 * 1 5 - 2 ^ /")
```

{{out}}

```txt

Infix input: 3 + 4 * 2 / (1 - 5) ^ 2 ^ 3
{"3",{},"3"}
{"+",{"+"},"3"}
{"4",{"+"},"3 4"}
{"*",{"+","*"},"3 4"}
{"2",{"+","*"},"3 4 2"}
{"/",{"+","/"},"3 4 2 *"}
{"(",{"+","/","("},"3 4 2 *"}
{"1",{"+","/","("},"3 4 2 * 1"}
{"-",{"+","/","(","-"},"3 4 2 * 1"}
{"5",{"+","/","(","-"},"3 4 2 * 1 5"}
{")",{"+","/"},"3 4 2 * 1 5 -"}
{"^",{"+","/","^"},"3 4 2 * 1 5 -"}
{"2",{"+","/","^"},"3 4 2 * 1 5 - 2"}
{"^",{"+","/","^","^"},"3 4 2 * 1 5 - 2"}
{"3",{"+","/","^","^"},"3 4 2 * 1 5 - 2 3"}
result: 3 4 2 * 1 5 - 2 3 ^ ^ / + [ok]
Infix input: ((1 + 2) ^ (3 + 4)) ^ (5 + 6)      result: 1 2 + 3 4 + ^ 5 6 + ^  [ok]
Infix input: (1 + 2) ^ (3 + 4) ^ (5 + 6)        result: 1 2 + 3 4 + 5 6 + ^ ^  [ok]
Infix input: ((3 ^ 4) ^ 2 ^ 9) ^ 2 ^ 5          result: 3 4 ^ 2 9 ^ ^ 2 5 ^ ^  [ok]
Infix input: (1 + 4) * (5 + 3) * 2 * 3          result: 1 4 + 5 3 + * 2 * 3 *  [ok]
Infix input: 1 * 2 * 3 * 4                      result: 1 2 * 3 * 4 *          [ok]
Infix input: 1 + 2 + 3 + 4                      result: 1 2 + 3 + 4 +          [ok]
Infix input: (1 + 2) ^ (3 + 4)                  result: 1 2 + 3 4 + ^          [ok]
Infix input: (5 ^ 6) ^ 7                        result: 5 6 ^ 7 ^              [ok]
Infix input: 5 ^ 4 ^ 3 ^ 2                      result: 5 4 3 2 ^ ^ ^          [ok]
Infix input: 1 + 2 + 3                          result: 1 2 + 3 +              [ok]
Infix input: 1 ^ 2 ^ 3                          result: 1 2 3 ^ ^              [ok]
Infix input: (1 ^ 2) ^ 3                        result: 1 2 ^ 3 ^              [ok]
Infix input: 1 - 1 + 3                          result: 1 1 - 3 +              [ok]
Infix input: 3 + 1 - 1                          result: 3 1 + 1 -              [ok]
Infix input: 1 - (2 + 3)                        result: 1 2 3 + -              [ok]
Infix input: 4 + 3 + 2                          result: 4 3 + 2 +              [ok]
Infix input: 5 + 4 + 3 + 2                      result: 5 4 + 3 + 2 +          [ok]
Infix input: 5 * 4 * 3 * 2                      result: 5 4 * 3 * 2 *          [ok]
Infix input: 5 + 4 - (3 + 2)                    result: 5 4 + 3 2 + -          [ok]
Infix input: 3 - 4 * 5                          result: 3 4 5 * -              [ok]
Infix input: 3 * (4 - 5)                        result: 3 4 5 - *              [ok]
Infix input: (3 - 4) * 5                        result: 3 4 - 5 *              [ok]
Infix input: 4 * 2 + 1 - 5                      result: 4 2 * 1 + 5 -          [ok]
Infix input: 4 * 2 / (1 - 5) ^ 2                result: 4 2 * 1 5 - 2 ^ /      [ok]

```

Note:

Some of the "made up" RPN used in [[Parsing/RPN_to_infix_conversion#Phix|parseRPN]] generates an infix
expression that does not re-create that (slightly dodgy) RPN when passed to this routine.
However, I have verified that the output of this routine does correctly re-generate the infix expression
when passed back through parseRPN(), and replaced several tests accordingly.
For example, both parseRPN("1 2 + 3 +") and parseRPN("1 2 3 + +") generate "1 + 2 + 3"; a round-trip needs the first.
There is a (feeble) argument that parseRPN("1 2 3 + +") should perhaps generate "1 + (2 + 3)", but it don't.


## PicoLisp

Note: "^" is a meta-character and must be escaped in strings

```PicoLisp
(de operator (Op)
   (member Op '("\^" "*" "/" "+" "-")) )

(de leftAssoc (Op)
   (member Op '("*" "/" "+" "-")) )

(de precedence (Op)
   (case Op
      ("\^" 4)
      (("*" "/") 3)
      (("+" "-") 2) ) )

(de shuntingYard (Str)
   (make
      (let (Fmt (-7 -30 -4)  Stack)
         (tab Fmt "Token" "Output" "Stack")
         (for Token (str Str "_")
            (cond
               ((num? Token) (link @))
               ((= "(" Token) (push 'Stack Token))
               ((= ")" Token)
                  (until (= "(" (car Stack))
                     (unless Stack
                        (quit "Unbalanced Stack") )
                     (link (pop 'Stack)) )
                  (pop 'Stack) )
               (T
                  (while
                     (and
                        (operator (car Stack))
                        ((if (leftAssoc (car Stack)) <= <)
                           (precedence Token)
                           (precedence (car Stack)) ) )
                     (link (pop 'Stack)) )
                  (push 'Stack Token) ) )
            (tab Fmt Token (glue " " (made)) Stack) )
         (while Stack
            (when (= "(" (car Stack))
               (quit "Unbalanced Stack") )
            (link (pop 'Stack))
            (tab Fmt NIL (glue " " (made)) Stack) ) ) ) )
```

Output:

```PicoLisp
: (shuntingYard "3 + 4 * 2 / (1 - 5) \^ 2 \^ 3")
Token  Output                        Stack
3      3
+      3                             +
4      3 4                           +
*      3 4                           *+
2      3 4 2                         *+
/      3 4 2 *                       /+
(      3 4 2 *                       (/+
1      3 4 2 * 1                     (/+
-      3 4 2 * 1                     -(/+
5      3 4 2 * 1 5                   -(/+
)      3 4 2 * 1 5 -                 /+
^      3 4 2 * 1 5 -                 ^/+
2      3 4 2 * 1 5 - 2               ^/+
^      3 4 2 * 1 5 - 2               ^^/+
3      3 4 2 * 1 5 - 2 3             ^^/+
       3 4 2 * 1 5 - 2 3 ^           ^/+
       3 4 2 * 1 5 - 2 3 ^ ^         /+
       3 4 2 * 1 5 - 2 3 ^ ^ /       +
       3 4 2 * 1 5 - 2 3 ^ ^ / +
-> (3 4 2 "*" 1 5 "-" 2 3 "\^" "\^" "/" "+")
```



## PL/I


```PL/I

cvt: procedure options (main);               /* 15 January 2012. */
   declare (in, stack, out) character (100) varying;
   declare (ch, chs) character (1);
   declare display bit (1) static initial ('0'b);

   in = '3 + 4 * 2 / ( 1 - 5 ) ^ 2 ^ 3';

   in = '(' || in || ' ) ';             /* Initialize with parentheses */

   put skip edit ('INPUT', 'STACK', 'OUTPUT') (a, col(37), a, col(47), a);

   stack = ' '; out = ''; /* Initialize */
   do while (length (in) > 0);
      ch = substr(in, 1, 1);
      select (ch);
         when (' ') ;

         when ('+', '-', '*', '/', '^')
            do;
               /* Copy any equal or higher-priority operators from the stack */
               /* to the output string */
               chs = substr(stack, 1, 1);
               do while ((spriority(chs) >= priority(ch)) & ( chs ^= ')' ) );
                  if display then put skip list ('unstacking: ' || chs);
                  out = out || ' ' || chs;
                  stack = substr(stack, 2);
                  chs = substr(stack, 1, 1);
               end;
               /* Now copy the input to the TOS. */
               if display then put skip list ('copying ' || ch || ' to TOS');
               stack = ch || stack;
            end;
         when ( '(' )
            do;
               stack = '(' || stack;
               if display then put skip list ('stacking the (' );
            end;
         when ( ')' )
            do; /* copy all operators from the stack to the output, */
                /* until a '(' is encountered. */
               chs = substr(stack, 1, 1);
               do while (chs ^= '(' );
                  if display then put skip list ('copying stack ' || chs || ' to output');
                  put skip edit (stack, out) (col(37), a, col(47), a);
                  out = out || ' ' || chs;
                  stack = substr(stack, 2);
                  chs = substr(stack, 1, 1);
               end;
               /* Now delete the '(' from the input and */
               /* the ')' from the top of the stack. */
               if display then put skip edit ('Deleting ( from TOS') (col(30), a);
               stack = substr(stack, 2);
               /* The '(' on the input is removed at the end of the loop. */
            end;
         otherwise /* it's an operand. */
            do;
               out = out || ' ';
               do while (ch ^= ' ');
                  if display then put skip list ('copying ' || ch || ' to output');
                  out = out || ch;
                  in = substr(in, 2);
                  ch = substr(in, 1, 1);
               end;
            end;
      end;
      in = substr(in, 2); /* Remove one character from the input */
      put skip edit (in, stack, out) (a, col(37), a, col(47), a);
   end;

priority: procedure (ch) returns (character(1));
   declare ch character (1);

   return ( translate(ch, '1122335', '()+-*/^' ) );
end priority;

spriority: procedure (ch) returns (character(1));
   declare ch character (1);

   return ( translate(ch, '1122334', '()+-*/^' ) );
end spriority;

end cvt;

```

Output:
<lang>
INPUT                               STACK     OUTPUT
3 + 4 * 2 / ( 1 - 5 ) ^ 2 ^ 3 )     (
+ 4 * 2 / ( 1 - 5 ) ^ 2 ^ 3 )       (          3
 4 * 2 / ( 1 - 5 ) ^ 2 ^ 3 )        +(         3
4 * 2 / ( 1 - 5 ) ^ 2 ^ 3 )         +(         3
* 2 / ( 1 - 5 ) ^ 2 ^ 3 )           +(         3 4
 2 / ( 1 - 5 ) ^ 2 ^ 3 )            *+(        3 4
2 / ( 1 - 5 ) ^ 2 ^ 3 )             *+(        3 4
/ ( 1 - 5 ) ^ 2 ^ 3 )               *+(        3 4 2
 ( 1 - 5 ) ^ 2 ^ 3 )                /+(        3 4 2 *
( 1 - 5 ) ^ 2 ^ 3 )                 /+(        3 4 2 *
 1 - 5 ) ^ 2 ^ 3 )                  (/+(       3 4 2 *
1 - 5 ) ^ 2 ^ 3 )                   (/+(       3 4 2 *
- 5 ) ^ 2 ^ 3 )                     (/+(       3 4 2 * 1
 5 ) ^ 2 ^ 3 )                      -(/+(      3 4 2 * 1
5 ) ^ 2 ^ 3 )                       -(/+(      3 4 2 * 1
) ^ 2 ^ 3 )                         -(/+(      3 4 2 * 1 5
                                    -(/+(      3 4 2 * 1 5
 ^ 2 ^ 3 )                          /+(        3 4 2 * 1 5 -
^ 2 ^ 3 )                           /+(        3 4 2 * 1 5 -
 2 ^ 3 )                            ^/+(       3 4 2 * 1 5 -
2 ^ 3 )                             ^/+(       3 4 2 * 1 5 -
^ 3 )                               ^/+(       3 4 2 * 1 5 - 2
 3 )                                ^^/+(      3 4 2 * 1 5 - 2
3 )                                 ^^/+(      3 4 2 * 1 5 - 2
)                                   ^^/+(      3 4 2 * 1 5 - 2 3
                                    ^^/+(      3 4 2 * 1 5 - 2 3
                                    ^/+(       3 4 2 * 1 5 - 2 3 ^
                                    /+(        3 4 2 * 1 5 - 2 3 ^ ^
                                    +(         3 4 2 * 1 5 - 2 3 ^ ^ /
                                               3 4 2 * 1 5 - 2 3 ^ ^ / +
                                               3 4 2 * 1 5 - 2 3 ^ ^ / +

```



## Python

Parenthesis are added to the operator table then special-cased in the code.
This solution includes the extra credit.

```python
from collections import namedtuple
from pprint import pprint as pp

OpInfo = namedtuple('OpInfo', 'prec assoc')
L, R = 'Left Right'.split()

ops = {
 '^': OpInfo(prec=4, assoc=R),
 '*': OpInfo(prec=3, assoc=L),
 '/': OpInfo(prec=3, assoc=L),
 '+': OpInfo(prec=2, assoc=L),
 '-': OpInfo(prec=2, assoc=L),
 '(': OpInfo(prec=9, assoc=L),
 ')': OpInfo(prec=0, assoc=L),
 }

NUM, LPAREN, RPAREN = 'NUMBER ( )'.split()


def get_input(inp = None):
    'Inputs an expression and returns list of (TOKENTYPE, tokenvalue)'

    if inp is None:
        inp = input('expression: ')
    tokens = inp.strip().split()
    tokenvals = []
    for token in tokens:
        if token in ops:
            tokenvals.append((token, ops[token]))
        #elif token in (LPAREN, RPAREN):
        #    tokenvals.append((token, token))
        else:
            tokenvals.append((NUM, token))
    return tokenvals

def shunting(tokenvals):
    outq, stack = [], []
    table = ['TOKEN,ACTION,RPN OUTPUT,OP STACK,NOTES'.split(',')]
    for token, val in tokenvals:
        note = action = ''
        if token is NUM:
            action = 'Add number to output'
            outq.append(val)
            table.append( (val, action, ' '.join(outq), ' '.join(s[0] for s in stack), note) )
        elif token in ops:
            t1, (p1, a1) = token, val
            v = t1
            note = 'Pop ops from stack to output'
            while stack:
                t2, (p2, a2) = stack[-1]
                if (a1 == L and p1 <= p2) or (a1 == R and p1 < p2):
                    if t1 != RPAREN:
                        if t2 != LPAREN:
                            stack.pop()
                            action = '(Pop op)'
                            outq.append(t2)
                        else:
                            break
                    else:
                        if t2 != LPAREN:
                            stack.pop()
                            action = '(Pop op)'
                            outq.append(t2)
                        else:
                            stack.pop()
                            action = '(Pop & discard "(")'
                            table.append( (v, action, ' '.join(outq), ' '.join(s[0] for s in stack), note) )
                            break
                    table.append( (v, action, ' '.join(outq), ' '.join(s[0] for s in stack), note) )
                    v = note = ''
                else:
                    note = ''
                    break
                note = ''
            note = ''
            if t1 != RPAREN:
                stack.append((token, val))
                action = 'Push op token to stack'
            else:
                action = 'Discard ")"'
            table.append( (v, action, ' '.join(outq), ' '.join(s[0] for s in stack), note) )
    note = 'Drain stack to output'
    while stack:
        v = ''
        t2, (p2, a2) = stack[-1]
        action = '(Pop op)'
        stack.pop()
        outq.append(t2)
        table.append( (v, action, ' '.join(outq), ' '.join(s[0] for s in stack), note) )
        v = note = ''
    return table

if __name__ == '__main__':
    infix = '3 + 4 * 2 / ( 1 - 5 ) ^ 2 ^ 3'
    print( 'For infix expression: %r\n' % infix )
    rp = shunting(get_input(infix))
    maxcolwidths = [len(max(x, key=len)) for x in zip(*rp)]
    row = rp[0]
    print( ' '.join('{cell:^{width}}'.format(width=width, cell=cell) for (width, cell) in zip(maxcolwidths, row)))
    for row in rp[1:]:
        print( ' '.join('{cell:<{width}}'.format(width=width, cell=cell) for (width, cell) in zip(maxcolwidths, row)))

    print('\n The final output RPN is: %r' % rp[-1][2])
```


;Sample output:

```txt
For infix expression: '3 + 4 * 2 / ( 1 - 5 ) ^ 2 ^ 3'

TOKEN         ACTION                RPN OUTPUT         OP STACK            NOTES
3     Add number to output   3
+     Push op token to stack 3                         +
4     Add number to output   3 4                       +
*     Push op token to stack 3 4                       + *
2     Add number to output   3 4 2                     + *
/     (Pop op)               3 4 2 *                   +        Pop ops from stack to output
      Push op token to stack 3 4 2 *                   + /
(     Push op token to stack 3 4 2 *                   + / (
1     Add number to output   3 4 2 * 1                 + / (
-     Push op token to stack 3 4 2 * 1                 + / ( -
5     Add number to output   3 4 2 * 1 5               + / ( -
)     (Pop op)               3 4 2 * 1 5 -             + / (    Pop ops from stack to output
      (Pop & discard "(")    3 4 2 * 1 5 -             + /
      Discard ")"            3 4 2 * 1 5 -             + /
^     Push op token to stack 3 4 2 * 1 5 -             + / ^
2     Add number to output   3 4 2 * 1 5 - 2           + / ^
^     Push op token to stack 3 4 2 * 1 5 - 2           + / ^ ^
3     Add number to output   3 4 2 * 1 5 - 2 3         + / ^ ^
      (Pop op)               3 4 2 * 1 5 - 2 3 ^       + / ^    Drain stack to output
      (Pop op)               3 4 2 * 1 5 - 2 3 ^ ^     + /
      (Pop op)               3 4 2 * 1 5 - 2 3 ^ ^ /   +
      (Pop op)               3 4 2 * 1 5 - 2 3 ^ ^ / +

 The final output RPN is: '3 4 2 * 1 5 - 2 3 ^ ^ / +'
```



## Racket


```Racket

#lang racket
;print column of width w
(define (display-col w s)
  (let* ([n-spaces (- w (string-length s))]
         [spaces (make-string n-spaces #\space)])
    (display (string-append s spaces))))
;print columns given widths (idea borrowed from PicoLisp)
(define (tab ws . ss) (for-each display-col ws ss) (newline))

(define input "3 + 4 * 2 / ( 1 - 5 ) ^ 2 ^ 3")

(define (paren? s) (or (string=? s "(") (string=? s ")")))
(define-values (prec lasso? rasso? op?)
  (let ([table '(["^" 4 r]
                 ["*" 3 l]
                 ["/" 3 l]
                 ["+" 2 l]
                 ["-" 2 l])])
    (define (asso x) (caddr (assoc x table)))
    (values (λ (x) (cadr (assoc x table)))
            (λ (x) (symbol=? (asso x) 'l))
            (λ (x) (symbol=? (asso x) 'r))
            (λ (x) (member x (map car table))))))

(define (shunt s)
  (define widths (list 8 (string-length input) (string-length input) 20))
  (tab widths "TOKEN" "OUT" "STACK" "ACTION")
  (let shunt ([out '()] [ops '()] [in (string-split s)] [action ""])
    (match in
      ['() (if (memf paren? ops)
               (error "unmatched parens")
               (reverse (append (reverse ops) out)))]
      [(cons x in)
       (tab widths x (string-join (reverse out) " ") (string-append* ops) action)
       (match x
         [(? string->number n) (shunt (cons n out) ops in (format "out ~a" n))]
         ["(" (shunt out (cons "(" ops) in "push (")]
         [")" (let-values ([(l r) (splitf-at ops (λ (y) (not (string=? y "("))))])
                (match r
                  ['() (error "unmatched parens")]
                  [(cons _ r) (shunt (append (reverse l) out) r in "clear til )")]))]
         [else (let-values ([(l r) (splitf-at ops (λ (y) (and (op? y)
                                                              ((if (lasso? x) <= <) (prec x) (prec y)))))])
                 (shunt (append (reverse l) out) (cons x r) in (format "out ~a, push ~a" l x)))])])))

```

{{out}}

```txt

> (shunt input)
TOKEN   OUT                          STACK                        ACTION
3
+       3                                                         out 3
4       3                            +                            out (), push +
*       3 4                          +                            out 4
2       3 4                          *+                           out (), push *
/       3 4 2                        *+                           out 2
(       3 4 2 *                      /+                           out (*), push /
1       3 4 2 *                      (/+                          push (
-       3 4 2 * 1                    (/+                          out 1
5       3 4 2 * 1                    -(/+                         out (), push -
)       3 4 2 * 1 5                  -(/+                         out 5
^       3 4 2 * 1 5 -                /+                           clear til )
2       3 4 2 * 1 5 -                ^/+                          out (), push ^
^       3 4 2 * 1 5 - 2              ^/+                          out 2
3       3 4 2 * 1 5 - 2              ^^/+                         out (), push ^
'("3" "4" "2" "*" "1" "5" "-" "2" "3" "^" "^" "/" "+")

```



## REXX

These REXX versions below allow multi-character tokens   (both operands and operators).

### assume expression is correct


```rexx
/*REXX pgm converts infix arith. expressions to Reverse Polish notation (shunting─yard).*/
parse arg x                                      /*obtain optional argument from the CL.*/
if x=''  then x= '3 + 4 * 2 / ( 1 - 5 ) ^ 2 ^ 3' /*Not specified?  Then use the default.*/
ox=x
x='(' space(x) ") "                              /*force stacking for the expression.   */
#=words(x)                                       /*get number of tokens in expression.  */
              do i=1  for #;   @.i=word(x, i)    /*assign the input tokens to an array. */
              end   /*i*/
tell=1                                           /*set to 0 if working steps not wanted.*/
L=max( 20, length(x) )                           /*use twenty for the minimum show width*/

say  'token'  center("input" , L, '─')     center("stack" , L%2, '─'),
              center("output", L, '─')     center("action", L,   '─')
op= ")(-+/*^";   Rop=substr(op,3);   p.=;  n=length(op);  RPN=  /*some handy-dandy vars.*/
s.=
   do i=1  for n;  _=substr(op,i,1);  s._=(i+1)%2;   p._=s._+(i==n);  end  /*i*/
$=                                               /* [↑]  assign the operator priorities.*/
   do k=1  for #;              ?=@.k             /*process each token from the  @. list.*/
     select                                      /*@.k is:  (,  operator,   ),   operand*/
     when ?=='('   then do; $="(" $;    call show 'moving'   ?   "──► stack";    end
     when isOp(?)  then do;              !=word($, 1)             /*get token from stack*/
                               do  while ! \==')'  &  s.!>=p.?
                               RPN=RPN !                          /*add token  to   RPN.*/
                               $=subword($, 2)                    /*del token from stack*/
                               call show 'unstacking:'  !
                               !=word($, 1)                       /*get token from stack*/
                               end   /*while*/
                        $=? $                                     /*add token  to  stack*/
                        call show 'moving'   ?   "──► stack"
                        end
     when ?==')'   then do;             !=word($, 1)              /*get token from stack*/
                             do  while  !\=='(';     RPN=RPN !    /*add token  to  RPN. */
                             $=subword($, 2)                      /*del token from stack*/
                             !=   word($, 1)                      /*get token from stack*/
                             call show 'moving stack' ! "──► RPN"
                             end   /*while*/
                        $=subword($, 2)                           /*del token from stack*/
                        call show 'deleting ( from the stack'
                        end
     otherwise  RPN=RPN ?                                         /*add operand to RPN. */
                call show 'moving'     ?     "──► RPN"
    end   /*select*/
   end    /*k*/
say
RPN=space(RPN $)                                 /*elide any superfluous blanks in RPN. */
say ' input:'  ox;     say " RPN──►"    RPN      /*display the input  and  the RPN.     */
parse source upper . y .                         /*invoked via the  C.L.  or  REXX pgm? */
if y=='COMMAND'  then  exit                      /*stick a fork in it,  we're all done. */
                 else  return RPN                /*return RPN to invoker  (the RESULT). */
/*──────────────────────────────────────────────────────────────────────────────────────────*/
isOp: return pos(arg(1),rOp) \== 0               /*is the first argument a "real" operator? */
show: if tell then say center(?,5) left(subword(x,k),L) left($,L%2) left(RPN,L) arg(1); return
```

'''output'''   when using the default input:

```txt

token ──────────────input─────────────── ──────stack────── ──────────────output────────────── ──────────────action──────────────
  (   ( 3 + 4 * 2 / ( 1 - 5 ) ^ 2 ^ 3 )  (                                                    moving ( ──► stack
  3   3 + 4 * 2 / ( 1 - 5 ) ^ 2 ^ 3 )    (                 3                                  moving 3 ──► RPN
  +   + 4 * 2 / ( 1 - 5 ) ^ 2 ^ 3 )      + (               3                                  moving + ──► stack
  4   4 * 2 / ( 1 - 5 ) ^ 2 ^ 3 )        + (               3 4                                moving 4 ──► RPN
  *   * 2 / ( 1 - 5 ) ^ 2 ^ 3 )          * + (             3 4                                moving * ──► stack
  2   2 / ( 1 - 5 ) ^ 2 ^ 3 )            * + (             3 4 2                              moving 2 ──► RPN
  /   / ( 1 - 5 ) ^ 2 ^ 3 )              + (               3 4 2 *                            unstacking: *
  /   / ( 1 - 5 ) ^ 2 ^ 3 )              / + (             3 4 2 *                            moving / ──► stack
  (   ( 1 - 5 ) ^ 2 ^ 3 )                ( / + (           3 4 2 *                            moving ( ──► stack
  1   1 - 5 ) ^ 2 ^ 3 )                  ( / + (           3 4 2 * 1                          moving 1 ──► RPN
  -   - 5 ) ^ 2 ^ 3 )                    - ( / + (         3 4 2 * 1                          moving - ──► stack
  5   5 ) ^ 2 ^ 3 )                      - ( / + (         3 4 2 * 1 5                        moving 5 ──► RPN
  )   ) ^ 2 ^ 3 )                        ( / + (           3 4 2 * 1 5 -                      moving stack ( ──► RPN
  )   ) ^ 2 ^ 3 )                        / + (             3 4 2 * 1 5 -                      deleting ( from the stack
  ^   ^ 2 ^ 3 )                          ^ / + (           3 4 2 * 1 5 -                      moving ^ ──► stack
  2   2 ^ 3 )                            ^ / + (           3 4 2 * 1 5 - 2                    moving 2 ──► RPN
  ^   ^ 3 )                              ^ ^ / + (         3 4 2 * 1 5 - 2                    moving ^ ──► stack
  3   3 )                                ^ ^ / + (         3 4 2 * 1 5 - 2 3                  moving 3 ──► RPN
  )   )                                  ^ / + (           3 4 2 * 1 5 - 2 3 ^                moving stack ^ ──► RPN
  )   )                                  / + (             3 4 2 * 1 5 - 2 3 ^ ^              moving stack / ──► RPN
  )   )                                  + (               3 4 2 * 1 5 - 2 3 ^ ^ /            moving stack + ──► RPN
  )   )                                  (                 3 4 2 * 1 5 - 2 3 ^ ^ / +          moving stack ( ──► RPN
  )   )                                                    3 4 2 * 1 5 - 2 3 ^ ^ / +          deleting ( from the stack

input: 3 + 4 * 2 / ( 1 - 5 ) ^ 2 ^ 3
RPN──► 3 4 2 * 1 5 - 2 3 ^ ^ / +

```


===checks expression for balanced ()===
Since these REXX versions of infix to RPN conversion affixes a leading   ''' ( '''   and trailing   ''' ) '''   to the expression, an

invalid expression such as   ''' )  (   ''' would be made legal by the aforemention affixing:     ''' )   (   '''

gets transformed into     ''' (   )   (   )   '''

Therefore, code was added to check for this condition, and also checks for mismatched parenthesis.

The   '''select'''   group could've been modified to check for mismatched parenthesis, but it would be harder to peruse the source.

```rexx
/*REXX pgm converts infix arith. expressions to Reverse Polish notation (shunting─yard).*/
parse arg x                                      /*obtain optional argument from the CL.*/
if x=''  then x= '3 + 4 * 2 / ( 1 - 5 ) ^ 2 ^ 3' /*Not specified?  Then use the default.*/
g=0                                              /* G   is a counter of   (  and  )     */
       do p=1 for words(x);       _=word(x,p)    /*catches unbalanced   ( )  and  ) (   */
       if _=='('  then g=g+1
                  else if _==')'  then do;    g=g-1;    if g<0  then g=-1e8;     end
       end   /*p*/
ox=x
x='(' space(x) ") "                              /*force stacking for the expression.   */
#=words(x)                                       /*get number of tokens in expression.  */
good= (g==0)                                     /*indicate expression is  good  or bad.*/
              do i=1  for #;   @.i=word(x, i)    /*assign the input tokens to an array. */
              end   /*i*/
tell=1                                           /*set to 0 if working steps not wanted.*/
L=max( 20, length(x) )                           /*use twenty for the minimum show width*/
if good  then say  'token'  center("input" , L, '─')     center("stack" , L%2, '─'),
                            center("output", L, '─')     center("action", L,   '─')
op= ")(-+/*^";   Rop=substr(op,3);   p.=;  n=length(op);  RPN=  /*some handy-dandy vars.*/
s.=
   do i=1  for n;  _=substr(op,i,1);   s._=(i+1)%2;   p._=s._+(i==n);  end  /*i*/
$=                                               /* [↑]  assign the operator priorities.*/
   do k=1  for #*good;         ?=@.k             /*process each token from the  @. list.*/
     select                                      /*@.k is:   (   operator   )   operand.*/
     when ?=='('   then do; $="(" $;    call show 'moving'   ?   "──► stack";    end
     when isOp(?)  then do;              !=word($, 1)             /*get token from stack*/
                               do  while ! \==')'  &  s.!>=p.?
                               RPN=RPN !                          /*add token  to   RPN.*/
                               $=subword($, 2)                    /*del token from stack*/
                               call show 'unstacking:'  !
                               !=word($, 1)                       /*get token from stack*/
                               end   /*while*/
                        $=? $                                     /*add token  to  stack*/
                        call show 'moving'   ?   "──► stack"
                        end
     when ?==')'   then do;             !=word($, 1)              /*get token from stack*/
                             do  while  !\=='(';     RPN=RPN !    /*add token   to  RPN.*/
                             $=subword($, 2)                      /*del token from stack*/
                             !=   word($, 1)                      /*get token from stack*/
                             call show 'moving stack' ! "──► RPN"
                             end   /*while*/
                        $=subword($, 2)                           /*del token from stack*/
                        call show 'deleting ( from the stack'
                        end
     otherwise  RPN=RPN ?                                         /*add operand to  RPN.*/
                call show 'moving'    ?    "──► RPN"
     end   /*select*/
   end     /*k*/
say
RPN=space(RPN $);  if \good  then RPN= '─────── error in expression ───────'   /*error? */
say ' input:'  ox;     say " RPN──►"    RPN      /*display the input  and  the RPN.     */
parse source upper . y .                         /*invoked via the  C.L.  or  REXX pgm? */
if y=='COMMAND'  then  exit                      /*stick a fork in it,  we're all done. */
                 else  return RPN                /*return RPN to invoker  (the RESULT). */
/*──────────────────────────────────────────────────────────────────────────────────────────*/
isOp: return pos(arg(1), Rop) \== 0              /*is the first argument a "real" operator? */
show: if tell then say center(?,5) left(subword(x,k),L) left($,L%2) left(RPN,L) arg(1); return
```

'''output'''   when using the input: <tt> )   ( </tt>

```txt

 input: ) (
 RPN──► ─────── error in expression ───────

```



## Ruby

See [[Parsing/RPN/Ruby]]


```ruby
rpn = RPNExpression.from_infix("3 + 4 * 2 / ( 1 - 5 ) ^ 2 ^ 3")
```

outputs

```txt
for Infix expression: 3 + 4 * 2 / ( 1 - 5 ) ^ 2 ^ 3
Term	Action	Output	Stack
3	PUSH V	["3"]	[]
+	PUSH OP	["3"]	["+"]
4	PUSH V	["3", "4"]	["+"]
*	PUSH OP	["3", "4"]	["+", "*"]
2	PUSH V	["3", "4", "2"]	["+", "*"]
/	MUL	["3", "4", "2", "*"]	["+"]	* has higher precedence than /
/	PUSH OP	["3", "4", "2", "*"]	["+", "/"]
(	OPEN_P	["3", "4", "2", "*"]	["+", "/", "("]
1	PUSH V	["3", "4", "2", "*", "1"]	["+", "/", "("]
-	PUSH OP	["3", "4", "2", "*", "1"]	["+", "/", "(", "-"]
5	PUSH V	["3", "4", "2", "*", "1", "5"]	["+", "/", "(", "-"]
)	SUB	["3", "4", "2", "*", "1", "5", "-"]	["+", "/", "("]	unwinding parenthesis
)	CLOSE_P	["3", "4", "2", "*", "1", "5", "-"]	["+", "/"]
^	PUSH OP	["3", "4", "2", "*", "1", "5", "-"]	["+", "/", "^"]
2	PUSH V	["3", "4", "2", "*", "1", "5", "-", "2"]	["+", "/", "^"]
^	PUSH OP	["3", "4", "2", "*", "1", "5", "-", "2"]	["+", "/", "^", "^"]
3	PUSH V	["3", "4", "2", "*", "1", "5", "-", "2", "3"]	["+", "/", "^", "^"]
RPN = 3 4 2 * 1 5 - 2 3 ^ ^ / +
```



## Rust


```rust
type Number = f64;

#[derive(Debug, Copy, Clone, PartialEq)]
struct Operator {
    token: char,
    operation: fn(Number, Number) -> Number,
    precedence: u8,
    is_left_associative: bool,
}

#[derive(Debug, Clone, PartialEq)]
enum Token {
    Digit(Number),
    Operator(Operator),
    LeftParen,
    RightParen,
}

impl Operator {
    fn new_token(
        token: char,
        precedence: u8,
        is_left_associative: bool,
        operation: fn(Number, Number) -> Number,
    ) -> Token {
        Token::Operator(Operator {
            token: token,
            operation: operation,
            precedence: precedence,
            is_left_associative,
        })
    }

    fn apply(&self, x: Number, y: Number) -> Number {
        (self.operation)(x, y)
    }
}

trait Stack<T> {
    fn top(&self) -> Option<T>;
}

impl<T: Clone> Stack<T> for Vec<T> {
    fn top(&self) -> Option<T> {
        if self.is_empty() {
            return None;
        }
        self.get(self.len() - 1).map(|value| value.clone())
    }
}
fn lex_token(input: char) -> Result<Token, char> {
    match input {
        '0'...'9' => Ok(Token::Digit(input.to_digit(10).unwrap() as Number)),
        '+' => Ok(Operator::new_token('+', 1, true, |x, y| x + y)),
        '-' => Ok(Operator::new_token('-', 1, true, |x, y| x - y)),
        '*' => Ok(Operator::new_token('*', 2, true, |x, y| x * y)),
        '/' => Ok(Operator::new_token('/', 2, true, |x, y| x / y)),
        '^' => Ok(Operator::new_token('^', 3, false, |x, y| x.powf(y))),
        '(' => Ok(Token::LeftParen),
        ')' => Ok(Token::RightParen),
        _ => Err(input),
    }
}

fn lex(input: String) -> Result<Vec<Token>, char> {
    input
        .chars()
        .filter(|c| !c.is_whitespace())
        .map(lex_token)
        .collect()
}

fn tilt_until(operators: &mut Vec<Token>, output: &mut Vec<Token>, stop: Token) -> bool {
    while let Some(token) = operators.pop() {
        if token == stop {
            return true;
        }
        output.push(token)
    }
    false
}

fn shunting_yard(tokens: Vec<Token>) -> Result<Vec<Token>, String> {
    let mut output: Vec<Token> = Vec::new();
    let mut operators: Vec<Token> = Vec::new();

    for token in tokens {
        match token {
            Token::Digit(_) => output.push(token),
            Token::LeftParen => operators.push(token),
            Token::Operator(operator) => {
                while let Some(top) = operators.top() {
                    match top {
                        Token::LeftParen => break,
                        Token::Operator(top_op) => {
                            let p = top_op.precedence;
                            let q = operator.precedence;
                            if (p > q) || (p == q && operator.is_left_associative) {
                                output.push(operators.pop().unwrap());
                            } else {
                                break;
                            }
                        }
                        _ => unreachable!("{:?} must not be on operator stack", token),
                    }
                }
                operators.push(token);
            }
            Token::RightParen => {
                if !tilt_until(&mut operators, &mut output, Token::LeftParen) {
                    return Err(String::from("Mismatched ')'"));
                }
            }
        }
    }

    if tilt_until(&mut operators, &mut output, Token::LeftParen) {
        return Err(String::from("Mismatched '('"));
    }

    assert!(operators.is_empty());
    Ok(output)
}

fn calculate(postfix_tokens: Vec<Token>) -> Result<Number, String> {
    let mut stack = Vec::new();

    for token in postfix_tokens {
        match token {
            Token::Digit(number) => stack.push(number),
            Token::Operator(operator) => {
                if let Some(y) = stack.pop() {
                    if let Some(x) = stack.pop() {
                        stack.push(operator.apply(x, y));
                        continue;
                    }
                }
                return Err(format!("Missing operand for operator '{}'", operator.token));
            }
            _ => unreachable!("Unexpected token {:?} during calculation", token),
        }
    }

    assert!(stack.len() == 1);
    Ok(stack.pop().unwrap())
}

fn run(input: String) -> Result<Number, String> {
    let tokens = match lex(input) {
        Ok(tokens) => tokens,
        Err(c) => return Err(format!("Invalid character: {}", c)),
    };
    let postfix_tokens = match shunting_yard(tokens) {
        Ok(tokens) => tokens,
        Err(message) => return Err(message),
    };

    calculate(postfix_tokens)
}
```



## Sidef

{{trans|Perl 6}}

```ruby
var prec = Hash(
    '^' => 4,
    '*' => 3,
    '/' => 3,
    '+' => 2,
    '-' => 2,
    '(' => 1,
)

var assoc = Hash(
    '^' => 'right',
    '*' => 'left',
    '/' => 'left',
    '+' => 'left',
    '-' => 'left',
)

func shunting_yard(prog) {
    var inp = prog.words
    var ops = []
    var res = []
 
    func report (op) {
        printf("%25s    %-7s %10s %s\n",
            res.join(' '), ops.join(' '), op, inp.join(' '))
    }

    func shift  (t)  { report( "shift #{t}"); ops << t }
    func reduce (t)  { report("reduce #{t}"); res << t }
 
    while (inp) {
        given(var t = inp.shift) {
           when (/\d/) { reduce(t) }
           when ('(')  { shift(t) }
           when (')')  {
               while (ops) {
                 (var x = ops.pop) == '(' ? break : reduce(x)
               }
           }
           default {
                var newprec = prec{t}
                while (ops) {
                    var oldprec = prec{ops[-1]}
 
                    break if (newprec > oldprec)
                    break if ((newprec == oldprec) && (assoc{t} == 'right'))
 
                    reduce(ops.pop)
                }
                shift(t)
            }
        }
    }
    while (ops) { reduce(ops.pop) }
    return res
}
 
say shunting_yard('3 + 4 * 2 / ( 1 - 5 ) ^ 2 ^ 3').join(' ')
```

{{out}}

```txt

                                       reduce 3 + 4 * 2 / ( 1 - 5 ) ^ 2 ^ 3
                        3               shift + 4 * 2 / ( 1 - 5 ) ^ 2 ^ 3
                        3    +         reduce 4 * 2 / ( 1 - 5 ) ^ 2 ^ 3
                      3 4    +          shift * 2 / ( 1 - 5 ) ^ 2 ^ 3
                      3 4    + *       reduce 2 / ( 1 - 5 ) ^ 2 ^ 3
                    3 4 2    +         reduce * ( 1 - 5 ) ^ 2 ^ 3
                  3 4 2 *    +          shift / ( 1 - 5 ) ^ 2 ^ 3
                  3 4 2 *    + /        shift ( 1 - 5 ) ^ 2 ^ 3
                  3 4 2 *    + / (     reduce 1 - 5 ) ^ 2 ^ 3
                3 4 2 * 1    + / (      shift - 5 ) ^ 2 ^ 3
                3 4 2 * 1    + / ( -   reduce 5 ) ^ 2 ^ 3
              3 4 2 * 1 5    + / (     reduce - ^ 2 ^ 3
            3 4 2 * 1 5 -    + /        shift ^ 2 ^ 3
            3 4 2 * 1 5 -    + / ^     reduce 2 ^ 3
          3 4 2 * 1 5 - 2    + / ^      shift ^ 3
          3 4 2 * 1 5 - 2    + / ^ ^   reduce 3
        3 4 2 * 1 5 - 2 3    + / ^     reduce ^
      3 4 2 * 1 5 - 2 3 ^    + /       reduce ^
    3 4 2 * 1 5 - 2 3 ^ ^    +         reduce /
  3 4 2 * 1 5 - 2 3 ^ ^ /              reduce +
3 4 2 * 1 5 - 2 3 ^ ^ / +

```



## Standard ML


```sml
structure Operator = struct
  datatype associativity = LEFT | RIGHT
  type operator = { symbol : char, assoc : associativity, precedence : int }

  val operators : operator list = [
    { symbol = #"^", precedence = 4, assoc = RIGHT },
    { symbol = #"*", precedence = 3, assoc = LEFT },
    { symbol = #"/", precedence = 3, assoc = LEFT },
    { symbol = #"+", precedence = 2, assoc = LEFT },
    { symbol = #"-", precedence = 2, assoc = LEFT }
  ]

  fun find (c : char) : operator option = List.find (fn ({symbol, ...} : operator) => symbol = c) operators

  infix cmp
  fun ({precedence=p1, assoc=a1, ...} : operator) cmp ({precedence=p2, ...} : operator) =
    case a1 of
      LEFT => p1 <= p2
    | RIGHT => p1 < p2
end

signature SHUNTING_YARD = sig
  type 'a tree
  type content

  val parse : string -> content tree
end

structure ShuntingYard : SHUNTING_YARD = struct
  structure O = Operator
  val cmp = O.cmp
  (* did you know infixity doesn't "carry out" of a structure unless you open it? TIL *)
  infix cmp
  fun pop2 (b::a::rest) = ((a, b), rest)
    | pop2 _ = raise Fail "bad input"

  datatype content = Op of char
                   | Int of int
  datatype 'a tree = Leaf
                   | Node of 'a tree * 'a * 'a tree

  fun parse_int' tokens curr = case tokens of
      [] => (List.rev curr, [])
    | t::ts => if Char.isDigit t then parse_int' ts (t::curr)
               else (List.rev curr, t::ts)

  fun parse_int tokens = let
    val (int_chars, rest) = parse_int' tokens []
  in
    ((Option.valOf o Int.fromString o String.implode) int_chars, rest)
  end

  fun parse (s : string) : content tree = let
    val tokens = String.explode s
    (* parse': tokens operator_stack trees *)
    fun parse' [] [] [result] = result
      | parse' [] (opr::os) trees =
          if opr = #"(" orelse opr = #")" then raise Fail "bad input"
          else let
            val ((a,b), trees') = pop2 trees
            val trees'' = (Node (a, Op opr, b)) :: trees'
          in
            parse' [] os trees''
          end
      | parse' (t::ts) operators trees =
          if Char.isSpace t then parse' ts operators trees else
          if t = #"(" then parse' ts (t::operators) (trees : content tree list) else
          if t = #")" then let
            (* process_operators : operators trees *)
            fun process_operators [] _ = raise Fail "bad input"
              | process_operators (opr::os) trees =
                  if opr = #"(" then (os, trees)
                  else let
                    val ((a, b), trees') = pop2 trees
                    val trees'' = (Node (a, Op opr, b)) :: trees'
                  in
                    process_operators os trees''
                  end
            val (operators', trees') = process_operators (operators : char list) (trees : content tree list)
          in
            parse' ts operators' trees'
          end else
          (case O.find (t : char) of
            SOME o1 => let
              (* process_operators : operators trees *)
              fun process_operators [] trees = ([], trees)
                | process_operators (o2::os) trees = (case O.find o2 of
                    SOME o2 =>
                      if o1 cmp o2 then let
                        val ((a, b), trees') = pop2 trees
                        val trees'' = (Node (a, Op (#symbol o2), b)) :: trees'
                      in
                        process_operators os trees''
                      end
                      else ((#symbol o2)::os, trees)
                  | NONE => (o2::os, trees))
              val (operators', trees') = process_operators operators trees
            in
              parse' ts ((#symbol o1)::operators') trees'
            end
          | NONE => let
              val (n, tokens') = parse_int (t::ts)
            in
              parse' tokens' operators ((Node (Leaf, Int n, Leaf)) :: trees)
            end)
      | parse' _ _ _ = raise Fail "bad input"
  in
    parse' tokens [] []
  end
end
```



## Swift


```Swift
import Foundation

// Using arrays for both stack and queue
struct Stack<T> {
    private(set) var elements = [T]()
    var isEmpty: Bool { return elements.isEmpty }

    mutating func push(newElement: T) {
        elements.append(newElement)
    }

    mutating func pop() -> T {
        return elements.removeLast()
    }

    func top() -> T? {
        return elements.last
    }
}

struct Queue<T> {
    private(set) var elements = [T]()
    var isEmpty: Bool { return elements.isEmpty }

    mutating func enqueue(newElement: T) {
        elements.append(newElement)
    }

    mutating func dequeue() -> T {
        return elements.removeFirst()
    }
}

enum Associativity { case Left, Right }

// Define abstract interface, which can be used to restrict Set extension
protocol OperatorType: Comparable, Hashable {
    var name: String { get }
    var precedence: Int { get }
    var associativity: Associativity { get }
}

struct Operator: OperatorType {
    let name: String
    let precedence: Int
    let associativity: Associativity
    // same operator names are not allowed
    var hashValue: Int { return "\(name)".hashValue }

    init(_ name: String, _ precedence: Int, _ associativity: Associativity) {
        self.name = name; self.precedence = precedence; self.associativity = associativity
    }
}

func ==(x: Operator, y: Operator) -> Bool {
    // same operator names are not allowed
    return x.name == y.name
}

func <(x: Operator, y: Operator) -> Bool {
    // compare operators by their precedence and associavity
    return (x.associativity == .Left && x.precedence == y.precedence) || x.precedence < y.precedence
}

extension Set where Element: OperatorType {
    func contains(op: String?) -> Bool {
        guard let operatorName = op else { return false }
        return contains { $0.name == operatorName }
    }

    subscript (operatorName: String) -> Element? {
        get {
            return filter { $0.name == operatorName }.first
        }
    }
}

// Convenience
extension String {
    var isNumber: Bool { return Double(self) != nil }
}

struct ShuntingYard {
    enum Error: ErrorType {
        case MismatchedParenthesis(String)
        case UnrecognizedToken(String)
    }

    static func parse(input: String, operators: Set<Operator>) throws -> String {
        var stack = Stack<String>()
        var output = Queue<String>()
        let tokens = input.componentsSeparatedByString(" ")

        for token in tokens {
            // Wikipedia: if token is a number add it to the output queue
            if token.isNumber {
                output.enqueue(token)
            }
            // Wikipedia: else if token is a operator:
            else if operators.contains(token) {
                // Wikipedia: while there is a operator on top of the stack and has lower precedence than current operator (token)
                while operators.contains(stack.top()) && hasLowerPrecedence(token, stack.top()!, operators) {
                    // Wikipedia: pop it off to the output queue
                    output.enqueue(stack.pop())
                }
                // Wikipedia: push current operator (token) onto the operator stack
                stack.push(token)
            }
            // Wikipedia: If the token is a left parenthesis, then push it onto the stack.
            else if token == "(" {
                stack.push(token)
            }
            // Wikipedia: If the token is a right parenthesis:
            else if token == ")" {
                // Wikipedia: Until the token at the top of the stack is a left parenthesis
                while !stack.isEmpty && stack.top() != "(" {
                    // Wikipedia: pop operators off the stack onto the output queue.
                    output.enqueue(stack.pop())
                }

                // If the stack runs out, than there are mismatched parentheses.
                if stack.isEmpty {
                    throw Error.MismatchedParenthesis(input)
                }

                // Wikipedia: Pop the left parenthesis from the stack, but not onto the output queue.
                stack.pop()
            }
            // if token is not number, operator or a parenthesis, then is not recognized
            else {
                throw Error.UnrecognizedToken(token)
            }
        }

        // Wikipedia: When there are no more tokens to read:

        // Wikipedia: While there are still operator tokens in the stack:
        while operators.contains(stack.top()) {
            // Wikipedia: Pop the operator onto the output queue.
            output.enqueue(stack.pop())
        }

        // Wikipedia: If the operator token on the top of the stack is a parenthesis, then there are mismatched parentheses
        // Note: Assume that all operators has been poped onto the output queue.
        if stack.isEmpty == false {
            throw Error.MismatchedParenthesis(input)
        }

        return output.elements.joinWithSeparator(" ")
    }

    static private func containsOperator(stack: Stack<String>, _ operators: [String: NSDictionary]) -> Bool {
        guard stack.isEmpty == false else { return false }
        // Is there a matching operator in the operators set?
        return operators[stack.top()!] != nil ? true : false
    }

    static private func hasLowerPrecedence(x: String, _ y: String, _ operators: Set<Operator>) -> Bool {
        guard let first = operators[x], let second = operators[y] else { return false }
        return first < second
    }
}

let input = "3 + 4 * 2 / ( 1 - 5 ) ^ 2 ^ 3"
let operators: Set<Operator> = [
    Operator("^", 4, .Right),
    Operator("*", 3, .Left),
    Operator("/", 3, .Left),
    Operator("+", 2, .Left),
    Operator("-", 2, .Left)
]
let output = try! ShuntingYard.parse(input, operators: operators)

print("input: \(input)")
print("output: \(output)")

```

{{out}}

```txt

input: 3 + 4 * 2 / ( 1 - 5 ) ^ 2 ^ 3
output: 3 4 2 * 1 5 - 2 3 ^ ^ / +

```



## Tcl


```tcl
package require Tcl 8.5

# Helpers
proc tokenize {str} {
    regexp -all -inline {[\d.]+|[-*^+/()]} $str
}
proc precedence op {
    dict get {^ 4 * 3 / 3 + 2 - 2} $op
}
proc associativity op {
    if {$op eq "^"} {return "right"} else {return "left"}
}

proc shunting {expression} {
    set stack {}
    foreach token [tokenize $expression] {
	if {[string is double $token]} {
	    puts "add to output: $token"
	    lappend output $token
	} elseif {$token eq "("} {
	    puts "push parenthesis"
	    lappend stack $token
	} elseif {$token eq ")"} {
	    puts "popping to parenthesis"
	    while {[lindex $stack end] ne "("} {
		lappend output [lindex $stack end]
		set stack [lreplace $stack end end]
		puts "...popped [lindex $output end] to output"
	    }
	    set stack [lreplace $stack end end]
	    puts "...found parenthesis"
	} else {
	    puts "adding operator: $token"
	    set p [precedence $token]
	    set a [associativity $token]
	    while {[llength $stack]} {
		set o2 [lindex $stack end]
		if {
		    $o2 ne "(" &&
		    (($a eq "left" && $p <= [precedence $o2]) ||
		    ($a eq "right" && $p < [precedence $o2]))
		} then {
		    puts "...popped operator $o2 to output"
		    lappend output $o2
		    set stack [lreplace $stack end end]
		} else {
		    break
		}
	    }
	    lappend stack $token
	}
	puts "\t\tOutput:\t$output\n\t\tStack:\t$stack"
    }
    puts "transferring tokens from stack to output"
    lappend output {*}[lreverse $stack]
}

puts [shunting "3 + 4 * 2 / ( 1 - 5 ) ^ 2 ^ 3"]
```

Output:

```txt

add to output: 3
		Output:	3
		Stack:
adding operator: +
		Output:	3
		Stack:	+
add to output: 4
		Output:	3 4
		Stack:	+
adding operator: *
		Output:	3 4
		Stack:	+ *
add to output: 2
		Output:	3 4 2
		Stack:	+ *
adding operator: /
...popped operator * to output
		Output:	3 4 2 *
		Stack:	+ /
push parenthesis
		Output:	3 4 2 *
		Stack:	+ / (
add to output: 1
		Output:	3 4 2 * 1
		Stack:	+ / (
adding operator: -
		Output:	3 4 2 * 1
		Stack:	+ / ( -
add to output: 5
		Output:	3 4 2 * 1 5
		Stack:	+ / ( -
popping to parenthesis
...popped - to output
...found parenthesis
		Output:	3 4 2 * 1 5 -
		Stack:	+ /
adding operator: ^
		Output:	3 4 2 * 1 5 -
		Stack:	+ / ^
add to output: 2
		Output:	3 4 2 * 1 5 - 2
		Stack:	+ / ^
adding operator: ^
		Output:	3 4 2 * 1 5 - 2
		Stack:	+ / ^ ^
add to output: 3
		Output:	3 4 2 * 1 5 - 2 3
		Stack:	+ / ^ ^
transferring tokens from stack to output
3 4 2 * 1 5 - 2 3 ^ ^ / +

```



## UNIX Shell



```bash
#!/bin/sh

getopprec() {
    case "$1" in
        '+') echo 2;;
        '-') echo 2;;
        '*') echo 3;;
        '/') echo 4;;
        '%') echo 4;;
        '^') echo 4;;
        '(') echo 5;;
    esac
}

getopassoc() {
    case "$1" in
        '^') echo r;;
        *)   echo l;;
    esac
}

showstacks() {
    [ -n "$1" ] && echo "Token: $1" || echo "End parsing"
    echo -e "\tOutput: `tr $'\n' ' ' <<< "$out"`"
    echo -e "\tOperators: `tr $'\n' ' ' <<< "$ops"`"
}

infix() {
    local out="" ops=""

    while [ "$#" -gt 0 ]; do
        grep -qE '^[0-9]+$' <<< "$1"
        if [ "$?" -eq 0 ]; then
            out="`sed -e '$a'"$1" -e '/^$/d' <<< "$out"`"

            showstacks "$1"
            shift && continue
        fi

        grep -q '^[-+*/^%]$' <<< "$1"
        if [ "$?" -eq 0 ]; then
            if [ -n "$ops" ]; then
                thispred=`getopprec "$1"`
                thisassoc=`getopassoc "$1"`
                topop="`sed -n '$p' <<< "$ops"`"
                thatpred=`getopprec "$topop"`
                thatassoc=`getopassoc "$topop"`
                while [ $thatpred -gt $thispred ] 2> /dev/null || ( [ \
                    $thatpred -eq $thispred ] 2> /dev/null && [ $thisassoc = \
                    'l' ] 2> /dev/null ); do # To /dev/null 'cus u r fake news

                    [ "$topop" = '(' ] && break

                    op="`sed -n '$p' <<< "$ops"`"
                    out="`sed -e '$a'"$op" -e '/^$/d' <<< "$out"`"
                    ops="`sed '$d' <<< "$ops"`"

                    topop="`sed -n '$p' <<< "$ops"`"
                    thatpred=`getopprec "$topop"`
                    thatassoc=`getopassoc "$topop"`
                done
            fi
            ops="`sed -e '$a'"$1" -e '/^$/d' <<< "$ops"`"

            showstacks "$1"
            shift && continue
        fi

        if [ "$1" = '(' ]; then
            ops="`sed -e '$a'"$1" -e '/^$/d' <<< "$ops"`"

            showstacks "$1"
            shift && continue
        fi

        if [ "$1" = ')' ]; then
            grep -q '^($' <<< "`sed -n '$p' <<< "$ops"`"
            while [ "$?" -ne 0 ]; do
                op="`sed -n '$p' <<< "$ops"`"
                out="`sed -e '$a'"$op" -e '/^$/d' <<< "$out"`"
                ops="`sed '$d' <<< "$ops"`"

                grep -q '^($' <<< "`sed '$p' <<< "$ops"`"
            done
            ops="`sed '$d' <<< "$ops"`"

            showstacks "$1"
            shift && continue
        fi

        shift
    done

    while [ -n "$ops" ]; do
        op="`sed -n '$p' <<< "$ops"`"
        out="`sed -e '$a'"$op" -e '/^$/d' <<< "$out"`"
        ops="`sed '$d' <<< "$ops"`"
    done

    showstacks "$1"
}

infix 3 + 4 \* 2 / \( 1 - 5 \) ^ 2 ^ 3
```



### Output

<lang>Token: 3
	Output: 3
	Operators:
Token: +
	Output: 3
	Operators: +
Token: 4
	Output: 3 4
	Operators: +
Token: *
	Output: 3 4
	Operators: + *
Token: 2
	Output: 3 4 2
	Operators: + *
Token: /
	Output: 3 4 2
	Operators: + * /
Token: (
	Output: 3 4 2
	Operators: + * / (
Token: 1
	Output: 3 4 2 1
	Operators: + * / (
Token: -
	Output: 3 4 2 1
	Operators: + * / ( -
Token: 5
	Output: 3 4 2 1 5
	Operators: + * / ( -
Token: )
	Output: 3 4 2 1 5 -
	Operators: + * /
Token: ^
	Output: 3 4 2 1 5 -
	Operators: + * / ^
Token: 2
	Output: 3 4 2 1 5 - 2
	Operators: + * / ^
Token: ^
	Output: 3 4 2 1 5 - 2
	Operators: + * / ^ ^
Token: 3
	Output: 3 4 2 1 5 - 2 3
	Operators: + * / ^ ^
End parsing
	Output: 3 4 2 1 5 - 2 3 ^ ^ / * +
	Operators:
```



## VBA


{{trans|Liberty BASIC}}


```VBA
Option Explicit
Option Base 1

Function ShuntingYard(strInfix As String) As String
Dim i As Long, j As Long, token As String, tokenArray() As String
Dim stack() As Variant, queue() As Variant, discard As String
Dim op1 As String, op2 As String

Debug.Print strInfix

' Get tokens
tokenArray = Split(strInfix)

' Initialize array (removed later)
ReDim stack(1)
ReDim queue(1)

' Loop over tokens
Do While 1
    i = i + 1
    If i - 1 > UBound(tokenArray) Then
        Exit Do
    Else
        token = tokenArray(i - 1) 'i-1 due to Split returning a Base 0
    End If
    If token = "" Then: Exit Do

    ' Print
    Debug.Print i, token, Join(stack, ","), Join(queue, ",")
    ' If-loop over tokens (either brackets, operators, or numbers)
    If token = "(" Then
        stack = push(token, stack)
    ElseIf token = ")" Then
        While Peek(stack) <> "("
            queue = push(pop(stack), queue)
        Wend
        discard = pop(stack) 'discard "("
    ElseIf isOperator(token) Then
        op1 = token
        Do While (isOperator(Peek(stack)))
'            Debug.Print Peek(stack)
            op2 = Peek(stack)
            If op2 <> "^" And precedence(op1) = precedence(op2) Then
                '"^" is the only right-associative operator
                queue = push(pop(stack), queue)
            ElseIf precedence(op1$) < precedence(op2$) Then
                queue = push(pop(stack), queue)
            Else
                Exit Do
            End If
        Loop
        stack = push(op1, stack)
    Else   'number
        'actually, wrong operator could end up here, like say %
        'If the token is a number, then add it to the output queue.
        queue = push(CStr(token), queue)
    End If
Loop

While stack(1) <> ""
    If Peek(stack) = "(" Then Debug.Print "no matching ')'": End
    queue = push(pop(stack), queue)
Wend

' Print final output
ShuntingYard = Join(queue, " ")
Debug.Print "Output:"
Debug.Print ShuntingYard
End Function

'------------------------------------------
Function isOperator(op As String) As Boolean
    isOperator = InStr("+-*/^", op) <> 0 And Len(op$) = 1
End Function

Function precedence(op As String) As Integer
    If isOperator(op$) Then
        precedence = 1 _
            - (InStr("+-*/^", op$) <> 0) _
            - (InStr("*/^", op$) <> 0) _
            - (InStr("^", op$) <> 0)
    End If
End Function

'------------------------------------------
Function push(str, stack) As Variant
Dim out() As Variant, i As Long
If Not IsEmpty(stack(1)) Then
    out = stack
    ReDim Preserve out(1 To UBound(stack) + 1)
    out(UBound(out)) = str
Else
    ReDim out(1 To 1)
    out(1) = str
End If
push = out
End Function

Function pop(stack)
pop = stack(UBound(stack))
If UBound(stack) > 1 Then
    ReDim Preserve stack(1 To UBound(stack) - 1)
Else
    stack(1) = ""
End If
End Function

Function Peek(stack)
    Peek = stack(UBound(stack))
End Function
```


{{out}}

```txt
?ShuntingYard("3 + 4 * 2 / ( 1 - 5 ) ^ 2 ^ 3")
3 + 4 * 2 / ( 1 - 5 ) ^ 2 ^ 3
 1            3
 2            +                           3
 3            4             +             3
 4            *             +             3,4
 5            2             +,*           3,4
 6            /             +,*           3,4,2
 7            (             +,/           3,4,2,*
 8            1             +,/,(         3,4,2,*
 9            -             +,/,(         3,4,2,*,1
 10           5             +,/,(,-       3,4,2,*,1
 11           )             +,/,(,-       3,4,2,*,1,5
 12           ^             +,/           3,4,2,*,1,5,-
 13           2             +,/,^         3,4,2,*,1,5,-
 14           ^             +,/,^         3,4,2,*,1,5,-,2
 15           3             +,/,^,^       3,4,2,*,1,5,-,2
Output:
3 4 2 * 1 5 - 2 3 ^ ^ / +
```



## Visual Basic .NET

{{trans|C#}}

```vbnet
Module Module1
    Class SymbolType
        Public ReadOnly symbol As String
        Public ReadOnly precedence As Integer
        Public ReadOnly rightAssociative As Boolean

        Public Sub New(symbol As String, precedence As Integer, rightAssociative As Boolean)
            Me.symbol = symbol
            Me.precedence = precedence
            Me.rightAssociative = rightAssociative
        End Sub
    End Class

    ReadOnly Operators As Dictionary(Of String, SymbolType) = New Dictionary(Of String, SymbolType) From
    {
        {"^", New SymbolType("^", 4, True)},
        {"*", New SymbolType("*", 3, False)},
        {"/", New SymbolType("/", 3, False)},
        {"+", New SymbolType("+", 2, False)},
        {"-", New SymbolType("-", 2, False)}
    }

    Function ToPostfix(infix As String) As String
        Dim tokens = infix.Split(" ")
        Dim stack As New Stack(Of String)
        Dim output As New List(Of String)

        Dim Print = Sub(action As String) Console.WriteLine("{0,-4} {1,-18} {2}", action + ":", $"stack[ {String.Join(" ", stack.Reverse())} ]", $"out[ {String.Join(" ", output)} ]")

        For Each token In tokens
            Dim iv As Integer
            Dim op1 As SymbolType
            Dim op2 As SymbolType
            If Integer.TryParse(token, iv) Then
                output.Add(token)
                Print(token)
            ElseIf Operators.TryGetValue(token, op1) Then
                While stack.Count > 0 AndAlso Operators.TryGetValue(stack.Peek(), op2)
                    Dim c = op1.precedence.CompareTo(op2.precedence)
                    If c < 0 OrElse Not op1.rightAssociative AndAlso c <= 0 Then
                        output.Add(stack.Pop())
                    Else
                        Exit While
                    End If
                End While
                stack.Push(token)
                Print(token)
            ElseIf token = "(" Then
                stack.Push(token)
                Print(token)
            ElseIf token = ")" Then
                Dim top = ""
                While stack.Count > 0
                    top = stack.Pop()
                    If top <> "(" Then
                        output.Add(top)
                    Else
                        Exit While
                    End If
                End While
                If top <> "(" Then
                    Throw New ArgumentException("No matching left parenthesis.")
                End If
                Print(token)
            End If
        Next
        While stack.Count > 0
            Dim top = stack.Pop()
            If Not Operators.ContainsKey(top) Then
                Throw New ArgumentException("No matching right parenthesis.")
            End If
            output.Add(top)
        End While
        Print("pop")
        Return String.Join(" ", output)
    End Function

    Sub Main()
        Dim infix = "3 + 4 * 2 / ( 1 - 5 ) ^ 2 ^ 3"
        Console.WriteLine(ToPostfix(infix))
    End Sub

End Module
```

{{out}}

```txt
3:   stack[  ]          out[ 3 ]
+:   stack[ + ]         out[ 3 ]
4:   stack[ + ]         out[ 3 4 ]
*:   stack[ + * ]       out[ 3 4 ]
2:   stack[ + * ]       out[ 3 4 2 ]
/:   stack[ + / ]       out[ 3 4 2 * ]
(:   stack[ + / ( ]     out[ 3 4 2 * ]
1:   stack[ + / ( ]     out[ 3 4 2 * 1 ]
-:   stack[ + / ( - ]   out[ 3 4 2 * 1 ]
5:   stack[ + / ( - ]   out[ 3 4 2 * 1 5 ]
):   stack[ + / ]       out[ 3 4 2 * 1 5 - ]
^:   stack[ + / ^ ]     out[ 3 4 2 * 1 5 - ]
2:   stack[ + / ^ ]     out[ 3 4 2 * 1 5 - 2 ]
^:   stack[ + / ^ ^ ]   out[ 3 4 2 * 1 5 - 2 ]
3:   stack[ + / ^ ^ ]   out[ 3 4 2 * 1 5 - 2 3 ]
pop: stack[  ]          out[ 3 4 2 * 1 5 - 2 3 ^ ^ / + ]
3 4 2 * 1 5 - 2 3 ^ ^ / +
```



## zkl

{{trans|Go}}

```zkl
var input="3 + 4 * 2 / ( 1 - 5 ) ^ 2 ^ 3";

var opa=Dictionary("^",T(4,True),  "*",T(3,False),    // op:(prec,rAssoc)
	    "/",T(3,False), "+",T(2,False), "-",T(2,False),
);

"infix:  ".println(input);
"postfix:".println(parseInfix(input));

fcn parseInfix(e){
   stack:=List(); // holds operators and left parenthesis
   rpn:="";
   foreach tok in (e.split(" ")){
      switch(tok){
         case("("){ stack.append(tok) } // push "(" to stack
	 case(")"){
            while(True){ // pop item ("(" or operator) from stack
               op:=stack.pop();
	       if(op=="(") break;  // discard "("
	       rpn+=" " + op;      // add operator to result
	    }
	 }
         else{  // default
	    o1:=opa.find(tok);  // op or Void
	    if(o1){ // token is an operator
	       while(stack){
                  // consider top item on stack
		  op:=stack[-1]; o2:=opa.find(op);
		  if(not o2 or o1[0]>o2[0] or
                     (o1[0]==o2[0] and o1[1])) break;
		  // top item is an operator that needs to come off
		  stack.pop();
		  rpn+=" " + op;              // add it to result
	       }
	       // push operator (the new one) to stack
	       stack.append(tok);
	    }else // token is an operand
	       rpn+=(rpn and " " or "") + tok; // add operand to result
	 }
      } // switch
      display(tok,rpn,stack);
   } // foreach
   // drain stack to result
   rpn + stack.reverse().concat(" ");
}
fcn display(tok,rpn,stack){
   "Token|".println(tok);
   "Stack|".println(stack.concat());
   "Queue|".println(rpn);
   println();
}
```

{{out}}
<pre style="height:20ex;overflow:scroll;">
infix:  3 + 4 * 2 / ( 1 - 5 ) ^ 2 ^ 3
Token|3
Stack|
Queue|3

Token|+
Stack|+
Queue|3

Token|4
Stack|+
Queue|3 4

Token|*
Stack|+*
Queue|3 4

Token|2
Stack|+*
Queue|3 4 2

Token|/
Stack|+/
Queue|3 4 2 *

Token|(
Stack|+/(
Queue|3 4 2 *

Token|1
Stack|+/(
Queue|3 4 2 * 1

Token|-
Stack|+/(-
Queue|3 4 2 * 1

Token|5
Stack|+/(-
Queue|3 4 2 * 1 5

Token|)
Stack|+/
Queue|3 4 2 * 1 5 -

Token|^
Stack|+/^
Queue|3 4 2 * 1 5 -

Token|2
Stack|+/^
Queue|3 4 2 * 1 5 - 2

Token|^
Stack|+/^^
Queue|3 4 2 * 1 5 - 2

Token|3
Stack|+/^^
Queue|3 4 2 * 1 5 - 2 3

postfix:3 4 2 * 1 5 - 2 3^ ^ / +

```



## Xojo


{{trans|VBA}}


```Xojo


Function ShuntingYard(strInfix As String) As String


  Dim i as Integer
  Dim token, tokenArray() As String
  Dim stack(), queue() As Variant
  Dim discard As String
  Dim op1, op2 As String

  Dim Left_Brackets, Right_Brackets As Integer

  Dim output As String
  Dim dbl_output As Double


  Left_Brackets = CountFields(strInfix, "(")
  Right_Brackets = CountFields(strInfix, ")")

  If Left_Brackets = Right_Brackets Then



    'Get tokens
    tokenArray = Split(strInfix," ")



    'Initialize array (removed later)
    ReDim stack(1)
    ReDim queue(1)

    'Loop over tokens
    For i = 0 to tokenArray.Ubound


    'i = i + 1
      If i  > UBound(tokenArray) Then
        Exit For
      Else
        token = tokenArray(i ) 'i-1 due to Split returning a Base 0
      End If
      If token = "" Then
        Exit For
      End If






      Dim stackString As String
      Dim queuString As String

      for m as Integer = 0 to stack.Ubound
        stackString = stackString +  " " +  stack(m)
      Next

      for m as Integer = 0 to queue.Ubound
        queuString = queuString + " " +  queue(m)
      Next

      MsgBox(Str(i) + "      " + token + "      " + stackString + "      " + queuString)

      'Window1.txtQueu.Text = Window1.txtQueu.Text + Str(i) + "      " + token + "      " + stackString + "      " + queuString + EndOfLIne




      ' If-loop over tokens (either brackets, operators, or numbers)
      If token = "(" Then
        stack.Append(token)

      ElseIf token = ")" Then
        While stack(stack.Ubound) <> "("
          queue.Append(stack.pop)
        Wend

        discard = stack.Pop 'discard "("
      ElseIf isOperator(token) Then
        op1 = token


        //Do While (isOperator(Peek(stack)))
        While isOperator(    stack(stack.Ubound)  ) = True
          op2 = stack(stack.Ubound)
          If op2 <> "^" And precedence(op1) = precedence(op2) Then
            '"^" is the only right-associative operator

            queue.Append(stack.pop)

          ElseIf precedence(op1) < precedence(op2) Then
            queue.Append(stack.Pop)
          Else
            Exit While
          End If
        Wend
        //Loop


        stack.Append(op1)
      Else   'number
        'actually, wrong operator could end up here, like say %
        'If the token is a number, then add it to the output queue.
        queue.Append(CStr(token))
      End If




    Next



    for i = 0 to queue.Ubound
      output = output  +queue(i) + " "
    next

    for i = stack.Ubound DownTo 0
      output = output + stack(i)+" "
    next



    While InStr(output, "  ") <> 0
      output = ReplaceAll(output,"  "," ")
    Wend


    output = Trim(output)


    Return output

  Else

    MsgBox("Syntax Error!" + EndOfLine + "Count left brackets: " + Str(Left_Brackets) + EndOfLine +"Count right brackets: " + Str(Right_Brackets))

  End If

End Function


Function isOperator(op As String) As Boolean

  If InStr("+-*/^", op) <> 0 and Len(op) = 1 Then
    Return True
  End If

End Function


Function precedence(op As String) As Integer

  If isOperator(op) = True Then



    If op = "+" or op = "-"  Then
      Return 2
    ElseIf op = "/" or op = "*" Then
      Return 3
    ElseIf op = "^" Then
      Return 4
    End If


  End If


End Function
```



{{out}}

```txt
?ShuntingYard("3 + 4 * 2 / ( 1 - 5 ) ^ 2 ^ 3")
3 + 4 * 2 / ( 1 - 5 ) ^ 2 ^ 3

0      3
1      +                 3
2      4         +         3
3      *         +         3 4
4      2         + *         3 4
5      /         + *         3 4 2
6      (         + /         3 4 2 *
7      1         + / (         3 4 2 *
8      -         + / (         3 4 2 * 1
9      5         + / ( -         3 4 2 * 1
10      )         + / ( -         3 4 2 * 1 5
11      ^         + /         3 4 2 * 1 5 -
12      2         + / ^         3 4 2 * 1 5 -
13      ^         + / ^         3 4 2 * 1 5 - 2
14      3         + / ^ ^         3 4 2 * 1 5 - 2

Output:
3 4 2 * 1 5 - 2 3 ^ ^ / +
```

