+++
title = "Truth table"
description = ""
date = 2019-07-04T06:24:10Z
aliases = []
[extra]
id = 10744
[taxonomies]
categories = ["task"]
tags = []
+++

A [[wp:Truth table|truth table]] is a display of the inputs to, and the output of a Boolean function organized as a table where each row gives one combination of input values and the corresponding value of the function.


## Task

# Input a Boolean function from the user as a string then calculate and print a formatted truth table for the given function.
 (One can assume that the user input is correct).
# Print and show output for Boolean functions of two and three input variables, but any program should not be limited to that many variables in the function.
# Either reverse-polish or infix notation expressions are allowed.


## Related tasks

*   [[Boolean values]]
*   [[Ternary logic]]


## See also

*   [http://mathworld.wolfram.com/TruthTable.html Wolfram MathWorld entry on truth tables].
*   [http://www.google.co.uk/search?q=truth+table&hl=en&client=firefox-a&hs=Om7&rls=org.mozilla:en-GB:official&prmd=imvns&tbm=isch&tbo=u&source=univ&sa=X&ei=C0uuTtjuH4Wt8gOF4dmYCw&ved=0CDUQsAQ&biw=941&bih=931&sei=%20Jk-uTuKKD4Sg8QOFkPGcCw some "truth table" examples from Google].





## ALGOL 68

Uses the Algol 68G specific evaluate procedure to evaluate the Boolean expressions. The expressions must therefore be infix and valid Algol 68 boolean expressions.

```algol68
# prints the truth table of a boolean expression composed of the 26 lowercase variables a..z, #
# the boolean operators AND, OR, XOR and NOT and the literal values TRUE and FALSE            #
# The evaluation is done with the Algol 68G evaluate function which is an extension           #
PROC print truth table = ( STRING expr )VOID:
     BEGIN

        # recursively prints the truth table #
        PROC print line = ( INT v )VOID:
             IF v > UPB bv
             THEN
                 # at the end of the variables - print the line #
                 FOR e TO UPB bv DO
                     IF used[ e ] THEN print( ( " ", bv[ e ], " " ) ) FI
                 OD;
                 print( ( "     ", evaluate( expr ), newline ) )
             ELIF used[ v ]
             THEN
                 # have another variable #
                 bv[ v ] := TRUE;
                 print line( v + 1 );
                 bv[ v ] := FALSE;
                 print line( v + 1 )
             ELSE
                 # this variable is not used #
                 print line( v + 1 )
             FI # print line # ;

        # returns the name of the variable number #
        PROC variable name = ( INT number )CHAR: REPR ( number + ( ABS "a" - 1 ) );

        # the 26 boolean variables #
        BOOL a := FALSE, b := FALSE, c := FALSE, d := FALSE, e := FALSE, f := FALSE;
        BOOL g := FALSE, h := FALSE, i := FALSE, j := FALSE, k := FALSE, l := FALSE;
        BOOL m := FALSE, n := FALSE, o := FALSE, p := FALSE, q := FALSE, r := FALSE;
        BOOL s := FALSE, t := FALSE, u := FALSE, v := FALSE, w := FALSE, x := FALSE;
        BOOL y := FALSE, z := FALSE;
        # table of the variables allowng access by number #
        []REF BOOL bv = ( a, b, c, d, e, f, g, h, i, j, k, l, m
                        , n, o, p, q, r, s, t, u, v, w, x, y, z
                        );
        [ 26 ]BOOL used;
        BOOL at least one variable := FALSE;
        # determine which variables are used in the expression #
        FOR v TO UPB bv DO
            used[ v ] := char in string( variable name( v ), NIL, expr );
            IF used[ v ]THEN at least one variable := TRUE FI
        OD;
        # print truth table headings #
        print( ( expr, ":", newline ) );
        FOR v TO UPB bv DO
            IF used[ v ] THEN print( ( " ", variable name( v ), " " ) ) FI
        OD;
        print( ( " value", newline ) );
        FOR v TO UPB bv DO
            IF used[ v ] THEN print( ( " - " ) ) FI
        OD;
        print( ( " -----", newline ) );
        # evaluate the expression for each cobination of variables #
        IF at least one variable
        THEN
             # the expression does not consist of literals only #
             FOR v TO UPB bv DO bv[ v ] := FALSE OD;
             print line( LWB bv )
        ELSE
             # the expression is constant #
             print( ( "     ", evaluate( expr ), newline ) )
        FI
     END # print truth table # ;

# print truth tables from the user's expressions #
print( ( "Please enter Boolean expressions using variables a, b, c, ..., z,",                  newline ) );
print( ( "operators AND, OR, NOT and XOR and literals TRUE and FALSE",                         newline ) );
print( ( "(Note operators and TRUE/FALSE must be uppercase and variables must be lower case)", newline ) );
print( ( "Enter a blank line to quit",                                                         newline ) );
WHILE
    STRING expr;
    print( ( "expression> " ) );
    read( ( expr, newline ) );
    expr /= ""
DO
    print truth table( expr )
OD
```

```txt

Please enter Boolean expressions using variables a, b, c, ..., z,
operators AND, OR, NOT and XOR and literals TRUE and FALSE
(Note operators and TRUE/FALSE must be uppercase and variables must be lower case)
Enter a blank line to quit
expression> a OR b
a OR b:
 a  b  value
 -  -  -----
 T  T      T
 T  F      T
 F  T      T
 F  F      F
expression> a AND ( b OR f )
a AND ( b OR f ):
 a  b  f  value
 -  -  -  -----
 T  T  T      T
 T  T  F      T
 T  F  T      T
 T  F  F      F
 F  T  T      F
 F  T  F      F
 F  F  T      F
 F  F  F      F
expression> ( NOT a ) OR ( b AND c )
( NOT a ) OR ( b AND c ):
 a  b  c  value
 -  -  -  -----
 T  T  T      T
 T  T  F      F
 T  F  T      F
 T  F  F      F
 F  T  T      T
 F  T  F      T
 F  F  T      T
 F  F  F      T
expression>

```



## C

```c
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#define TRUE 1
#define FALSE 0
#define STACK_SIZE 80
#define BUFFER_SIZE 100

typedef int bool;

typedef struct {
    char name;
    bool val;
} var;

typedef struct {
    int top;
    bool els[STACK_SIZE];
} stack_of_bool;

char expr[BUFFER_SIZE];
int expr_len;
var vars[24];
int vars_len;

/* stack manipulation functions */

bool is_full(stack_of_bool *sp) {
    return sp->top == STACK_SIZE - 1;
}

bool is_empty(stack_of_bool *sp) {
    return sp->top == -1;
}

bool peek(stack_of_bool *sp) {
    if (!is_empty(sp))
        return sp->els[sp->top];
    else {
        printf("Stack is empty.\n");
        exit(1);
    }
}

void push(stack_of_bool *sp, bool val) {
    if (!is_full(sp)) {
        sp->els[++(sp->top)] = val;
    }
    else {
        printf("Stack is full.\n");
        exit(1);
    }
}

bool pop(stack_of_bool *sp) {
    if (!is_empty(sp))
        return sp->els[(sp->top)--];
    else {
        printf("\nStack is empty.\n");
        exit(1);
    }
}

bool is_operator(const char c) {
   return c == '&' || c == '|' || c == '!' || c == '^';
}

int vars_index(const char c) {
   int i;
   for (i = 0; i < vars_len; ++i) {
       if (vars[i].name == c) return i;
   }
   return -1;
}

bool eval_expr() {
    int i, vi;
    char e;
    stack_of_bool s;
    stack_of_bool *sp = &s;
    s.top = -1;
    for (i = 0; i < expr_len; ++i) {
        e = expr[i];
        if (e == 'T')
            push(sp, TRUE);
        else if (e == 'F')
            push(sp, FALSE);
        else if((vi = vars_index(e)) >= 0) {
            push(sp, vars[vi].val);
        }
        else switch(e) {
            case '&':
                push(sp, pop(sp) & pop(sp));
                break;
            case '|':
                push(sp, pop(sp) | pop(sp));
                break;
            case '!':
                push(sp, !pop(sp));
                break;
            case '^':
                push(sp, pop(sp) ^ pop(sp));
                break;
            default:
                printf("\nNon-conformant character '%c' in expression.\n", e);
                exit(1);
        }
    }
    if (s.top != 0) {
        printf("\nStack should contain exactly one element.\n");
        exit(1);
    }
    return peek(sp);
}

void set_vars(int pos) {
    int i;
    if (pos > vars_len) {
        printf("\nArgument to set_vars can't be greater than the number of variables.\n");
        exit(1);
    }
    else if (pos == vars_len) {
        for (i = 0; i < vars_len; ++i) {
            printf((vars[i].val) ? "T  " : "F  ");
        }
        printf("%c\n", (eval_expr()) ? 'T' : 'F');
    }
    else {
        vars[pos].val = FALSE;
        set_vars(pos + 1);
        vars[pos].val = TRUE;
        set_vars(pos + 1);
    }
}

/* removes whitespace and converts to upper case */
void process_expr() {
    int i, count = 0;
    for (i = 0; expr[i]; ++i) {
        if (!isspace(expr[i])) expr[count++] = toupper(expr[i]);
    }
    expr[count] = '\0';
}

int main() {
    int i, h;
    char e;
    printf("Accepts single-character variables (except for 'T' and 'F',\n");
    printf("which specify explicit true or false values), postfix, with\n");
    printf("&|!^ for and, or, not, xor, respectively; optionally\n");
    printf("seperated by whitespace. Just enter nothing to quit.\n");

    while (TRUE) {
        printf("\nBoolean expression: ");
        fgets(expr, BUFFER_SIZE, stdin);
        fflush(stdin);
        process_expr();
        expr_len = strlen(expr);
        if (expr_len == 0) break;
        vars_len = 0;
        for (i = 0; i < expr_len; ++i) {
            e = expr[i];
            if (!is_operator(e) && e != 'T' && e != 'F' && vars_index(e) == -1) {
                vars[vars_len].name = e;
                vars[vars_len].val = FALSE;
                vars_len++;
            }
        }
        if (vars_len == 0) {
            printf("\nNo variables were entered.\n");
            continue;
        }
        printf("\n");
        for (i = 0; i < vars_len; ++i) {
            printf("%c  ", vars[i].name);
        }
        printf("%s\n", expr);
        h = vars_len * 3 + expr_len;
        for (i = 0; i < h; ++i) printf("=");
        printf("\n");
        set_vars(0);
    }
    return 0;
}
```


```txt

Accepts single-character variables (except for 'T' and 'F',
which specify explicit true or false values), postfix, with
&|!^ for and, or, not, xor, respectively; optionally
seperated by whitespace. Just enter nothing to quit.

Boolean expression: A B ^

A  B  AB^

### ===

F  F  F
F  T  T
T  F  T
T  T  F

Boolean expression: A B C ^ |

A  B  C  ABC^|

### ========

F  F  F  F
F  F  T  T
F  T  F  T
F  T  T  F
T  F  F  T
T  F  T  T
T  T  F  T
T  T  T  T

Boolean expression: A B C D ^ ^ ^

A  B  C  D  ABCD^^^

### =============

F  F  F  F  F
F  F  F  T  T
F  F  T  F  T
F  F  T  T  F
F  T  F  F  T
F  T  F  T  F
F  T  T  F  F
F  T  T  T  T
T  F  F  F  T
T  F  F  T  F
T  F  T  F  F
T  F  T  T  T
T  T  F  F  F
T  T  F  T  T
T  T  T  F  T
T  T  T  T  F

Boolean expression:

```



## C#

This implementation allows the user to define the characters for true/false and the operators.<br/>
To not make it too complicated, operators are limited to a single character.<br/>
Either postfix or infix expressions are allowed. Infix expressions are converted to postfix.

```c#
using System;
using System.Collections;
using System.Collections.Generic;
using System.Linq;
using System.Text;

public class TruthTable
{
    enum TokenType { Unknown, WhiteSpace, Constant, Operand, Operator, LeftParenthesis, RightParenthesis }

    readonly char trueConstant, falseConstant;
    readonly IDictionary<char, Operator> operators = new Dictionary<char, Operator>();

    public TruthTable(char falseConstant, char trueConstant)
    {
        this.trueConstant = trueConstant;
        this.falseConstant = falseConstant;
        Operators = new OperatorCollection(operators);
    }

    public OperatorCollection Operators { get; }

    public void PrintTruthTable(string expression, bool isPostfix = false)
    {
        try {
            foreach (string line in GetTruthTable(expression, isPostfix)) {
                Console.WriteLine(line);
            }
        } catch (ArgumentException ex) {
            Console.WriteLine(expression + "   " + ex.Message);
        }
    }

    public IEnumerable<string> GetTruthTable(string expression, bool isPostfix = false)
    {
        if (string.IsNullOrWhiteSpace(expression)) throw new ArgumentException("Invalid expression.");
        //Maps parameters to an index in BitSet
        //Makes sure they appear in the truth table in the order they first appear in the expression
        var parameters = expression
            .Where(c => TypeOf(c) == TokenType.Operand)
            .Distinct()
            .Reverse()
            .Select((c, i) => (symbol: c, index: i))
            .ToDictionary(p => p.symbol, p => p.index);

        int count = parameters.Count;
        if (count > 32) throw new ArgumentException("Cannot have more than 32 parameters.");
        string header = count == 0 ? expression : string.Join(" ",
            parameters.OrderByDescending(p => p.Value).Select(p => p.Key)) + " " + expression;

        if (!isPostfix) expression = ConvertToPostfix(expression);

        var values = default(BitSet);
        var stack = new Stack<char>(expression.Length);
        for (int loop = 1 << count; loop > 0; loop--) {
            foreach (char token in expression) stack.Push(token);
            bool result = Evaluate(stack, values, parameters);
            if (header != null) {
                if (stack.Count > 0) throw new ArgumentException("Invalid expression.");
                yield return header;
                header = null;
            }
            string line = (count == 0 ? "" : " ") + (result ? trueConstant : falseConstant);
            line = string.Join(" ", Enumerable.Range(0, count)
                .Select(i => values[count - i - 1] ? trueConstant : falseConstant)) + line;
            yield return line;
            values++;
        }
    }

    public string ConvertToPostfix(string infix)
    {
        var stack = new Stack<char>();
        var postfix = new StringBuilder();
        foreach (char c in infix) {
            switch (TypeOf(c)) {
            case TokenType.WhiteSpace:
                continue;
            case TokenType.Constant:
            case TokenType.Operand:
                postfix.Append(c);
                break;
            case TokenType.Operator:
                int precedence = Precedence(c);
                while (stack.Count > 0 && Precedence(stack.Peek()) > precedence) {
                    postfix.Append(stack.Pop());
                }
                stack.Push(c);
                break;
            case TokenType.LeftParenthesis:
                stack.Push(c);
                break;
            case TokenType.RightParenthesis:
                char top = default(char);
                while (stack.Count > 0) {
                    top = stack.Pop();
                    if (top == '(') break;
                    else postfix.Append(top);
                }
                if (top != '(') throw new ArgumentException("No matching left parenthesis.");
                break;
            default:
                throw new ArgumentException("Invalid character: " + c);
            }
        }
        while (stack.Count > 0) {
            char top = stack.Pop();
            if (top == '(') throw new ArgumentException("No matching right parenthesis.");
            postfix.Append(top);
        }
        return postfix.ToString();
    }

    private bool Evaluate(Stack<char> expression, BitSet values, IDictionary<char, int> parameters)
    {
        if (expression.Count == 0) throw new ArgumentException("Invalid expression.");
        char c = expression.Pop();
        TokenType type = TypeOf(c);
        while (type == TokenType.WhiteSpace) type = TypeOf(c = expression.Pop());
        switch (type) {
        case TokenType.Constant:
            return c == trueConstant;
        case TokenType.Operand:
            return values[parameters[c]];
        case TokenType.Operator:
            bool right = Evaluate(expression, values, parameters);
            Operator op = operators[c];
            if (op.Arity == 1) return op.Function(right, right);
            bool left = Evaluate(expression, values, parameters);
            return op.Function(left, right);
        default:
            throw new ArgumentException("Invalid character: " + c);
        }
    }

    private TokenType TypeOf(char c)
    {
        if (char.IsWhiteSpace(c)) return TokenType.WhiteSpace;
        if (c == '(') return TokenType.LeftParenthesis;
        if (c == ')') return TokenType.RightParenthesis;
        if (c == trueConstant || c == falseConstant) return TokenType.Constant;
        if (operators.ContainsKey(c)) return TokenType.Operator;
        if (char.IsLetter(c)) return TokenType.Operand;
        return TokenType.Unknown;
    }

    private int Precedence(char op) => operators.TryGetValue(op, out var o) ? o.Precedence : int.MinValue;
}

struct Operator
{
    public Operator(char symbol, int precedence, Func<bool, bool> function) : this(symbol, precedence, 1, (l, r) => function(r)) { }

    public Operator(char symbol, int precedence, Func<bool, bool, bool> function) : this(symbol, precedence, 2, function) { }

    private Operator(char symbol, int precedence, int arity, Func<bool, bool, bool> function) : this()
    {
        Symbol = symbol;
        Precedence = precedence;
        Arity = arity;
        Function = function;
    }

    public char Symbol { get; }
    public int Precedence { get; }
    public int Arity { get; }
    public Func<bool, bool, bool> Function { get; }
}

public class OperatorCollection : IEnumerable
{
    readonly IDictionary<char, Operator> operators;

    internal OperatorCollection(IDictionary<char, Operator> operators) {
        this.operators = operators;
    }

    public void Add(char symbol, int precedence, Func<bool, bool> function)
        => operators[symbol] = new Operator(symbol, precedence, function);
    public void Add(char symbol, int precedence, Func<bool, bool, bool> function)
        => operators[symbol] = new Operator(symbol, precedence, function);

    public void Remove(char symbol) => operators.Remove(symbol);

    IEnumerator IEnumerable.GetEnumerator() => operators.Values.GetEnumerator();
}

struct BitSet
{
    private int bits;

    private BitSet(int bits) { this.bits = bits; }

    public static BitSet operator ++(BitSet bitSet) => new BitSet(bitSet.bits + 1);

    public bool this[int index] => (bits & (1 << index)) != 0;
}

class Program
{
    public static void Main() {
        TruthTable tt = new TruthTable('F', 'T') {
            Operators = {
                { '!', 6, r => !r },
                { '&', 5, (l, r) => l && r },
                { '^', 4, (l, r) => l ^ r },
                { '|', 3, (l, r) => l || r }
            }
        };
        //Add a crazy operator:
        var rng = new Random();
        tt.Operators.Add('?', 6, r => rng.NextDouble() < 0.5);
        string[] expressions = {
            "!!!T",
            "?T",
            "F & x | T",
            "F & (x | T",
            "F & x | T)",
            "a ! (a & a)",
            "a | (a * a)",
            "a ^ T & (b & !c)",
        };
        foreach (string expression in expressions) {
            tt.PrintTruthTable(expression);
            Console.WriteLine();
        }

        //Define a different language
        tt = new TruthTable('0', '1') {
            Operators = {
                { '-', 6, r => !r },
                { '^', 5, (l, r) => l && r },
                { 'v', 3, (l, r) => l || r },
                { '>', 2, (l, r) => !l || r },
                { '=', 1, (l, r) => l == r },
            }
        };
        expressions = new[] {
            "-X v 0 = X ^ 1",
            "(H > M) ^ (S > H) > (S > M)"
        };
        foreach (string expression in expressions) {
            tt.PrintTruthTable(expression);
            Console.WriteLine();
        }
    }
}
```

```txt

!!!T
F

?T
F    //Could be T or F

x F & x | T
F T
T T

F & (x | T   No matching right parenthesis.

F & x | T)   No matching left parenthesis.

a ! (a & a)   Invalid expression.

a | (a * a)   Invalid character: *

a b c a ^ T & (b & !c)
F F F F
F F T F
F T F T
F T T F
T F F T
T F T T
T T F F
T T T T

X -X v 0 = -(X ^ 1)
0 1
1 1

H M S (H > M) ^ (S > H) > (S > M)
0 0 0 1
0 0 1 1
0 1 0 1
0 1 1 1
1 0 0 1
1 0 1 1
1 1 0 1
1 1 1 1

```



## D

```d
import std.stdio, std.string, std.array, std.algorithm, std.typecons;

struct Var {
    const char name;
    bool val;
}
const string expr;
Var[] vars;

bool pop(ref bool[] arr) pure nothrow {
    const last = arr.back;
    arr.popBack;
    return last;
}

enum isOperator = (in char c) pure => "&|!^".canFind(c);

enum varsCountUntil = (in char c) nothrow =>
    .vars.map!(v => v.name).countUntil(c).Nullable!(int, -1);

bool evalExp() {
    bool[] stack;

    foreach (immutable e; .expr) {
        if (e == 'T')
            stack ~= true;
        else if (e == 'F')
            stack ~= false;
        else if (!e.varsCountUntil.isNull)
            stack ~= .vars[e.varsCountUntil.get].val;
        else switch (e) {
            case '&':
                stack ~= stack.pop & stack.pop;
                break;
            case '|':
                stack ~= stack.pop | stack.pop;
                break;
            case '!':
                stack ~= !stack.pop;
                break;
            case '^':
                stack ~= stack.pop ^ stack.pop;
                break;
            default:
                throw new Exception("Non-conformant character '" ~
                                    e ~ "' in expression.");
        }
    }

    assert(stack.length == 1);
    return stack.back;
}

void setVariables(in size_t pos)
in {
    assert(pos <= .vars.length);
} body {
    if (pos == .vars.length)
        return writefln("%-(%s %) %s",
                        .vars.map!(v => v.val ? "T" : "F"),
                        evalExp ? "T" : "F");

    .vars[pos].val = false;
    setVariables(pos + 1);
    .vars[pos].val = true;
    setVariables(pos + 1);
}

static this() {
"Accepts single-character variables (except for 'T' and 'F',
which specify explicit true or false values), postfix, with
&|!^ for and, or, not, xor, respectively; optionally
seperated by whitespace.".writeln;

    "Boolean expression: ".write;
    .expr = readln.split.join;
}

void main() {
    foreach (immutable e; expr)
        if (!e.isOperator && !"TF".canFind(e) &&
            e.varsCountUntil.isNull)
            .vars ~= Var(e);
    if (.vars.empty)
        return;

    writefln("%-(%s %) %s", .vars.map!(v => v.name), .expr);
    setVariables(0);
}
```

```txt
Accepts single-character variables (except for 'T' and 'F',
which specify explicit true or false values), postfix, with
&|!^ for and, or, not, xor, respectively; optionally
seperated by whitespace.
Boolean expression: A B ^
A B AB^
F F F
F T T
T F T
T T F

...
Boolean expression: A B C ^ |
A B C ABC^|
F F F F
F F T T
F T F T
F T T F
T F F T
T F T T
T T F T
T T T T

...
Boolean expression: A B C D ^ ^ ^
A B C D ABCD^^^
F F F F F
F F F T T
F F T F T
F F T T F
F T F F T
F T F T F
F T T F F
F T T T T
T F F F T
T F F T F
T F T F F
T F T T T
T T F F F
T T F T T
T T T F T
T T T T F
```


=={{header|Déjà Vu}}==
```dejavu
print-line lst end:
	for v in reversed copy lst:
		print\( v chr 9 )
	print end

(print-truth-table) t n func:
	if n:
		(print-truth-table) push-through copy t 0 -- n @func
		(print-truth-table) push-through copy t 1 -- n @func
	else:
		print-line t func for in copy t

print-truth-table vars name func:
	print-line vars name
	(print-truth-table) [] len vars @func
	print "" # extra new line

stu s t u:
	or s /= t u

abcd a b c d:
	/= a /= b /= c d

print-truth-table [ "A" "B" ] "A ^ B" @/=
print-truth-table [ "S" "T" "U" ] "S | (T ^ U)" @stu
print-truth-table [ "A" "B" "C" "D" ] "A ^ (B ^ (C ^ D))" @abcd
```

```txt
A	B	A ^ B
0	0	0
0	1	1
1	0	1
1	1	0

S	T	U	S | (T ^ U)
0	0	0	0
0	0	1	1
0	1	0	1
0	1	1	0
1	0	0	1
1	0	1	1
1	1	0	1
1	1	1	1

A	B	C	D	A ^ (B ^ (C ^ D))
0	0	0	0	0
0	0	0	1	1
0	0	1	0	1
0	0	1	1	0
0	1	0	0	1
0	1	0	1	0
0	1	1	0	0
0	1	1	1	1
1	0	0	0	1
1	0	0	1	0
1	0	1	0	0
1	0	1	1	1
1	1	0	0	0
1	1	0	1	1
1	1	1	0	1
1	1	1	1	0

```



## Factor

Postfix is a natural choice. That way, we can use <code>(eval)</code> to to evaluate the expressions without much fuss.

```factor
USING: arrays combinators eval formatting io kernel listener
math.combinatorics prettyprint qw sequences splitting
vocabs.parser ;
IN: rosetta-code.truth-table

: prompt ( -- str )
    "Please enter a boolean expression using 1-long" print
    "variable names and postfix notation. Available" print
    "operators are and, or, not, and xor. Example:"  print
    "> a b and"                                      print nl
    "> " write readln nl ;

: replace-var ( str -- str' )
    dup length 1 = [ drop "%s" ] when ;

: replace-vars ( str -- str' )
    " " split [ replace-var ] map " " join ;

: extract-vars ( str -- seq )
    " " split [ length 1 = ] filter ;

: count-vars ( str -- n )
    " " split [ "%s" = ] count ;

: truth-table ( n -- seq )
    qw{ t f } swap selections ;

: print-row ( seq -- )
    [ write bl ] each ;

: print-table ( seq -- )
    [ print-row nl ] each ;

! Adds a column to the end of a two-dimensional array.
: add-col ( seq col -- seq' )
    [ flip ] dip 1array append flip ;

: header ( str -- )
    [ extract-vars ] [ ] bi
    [ print-row "| " write ] [ print ] bi*
    "
### ===========
" print ;

: solve-expr ( seq str -- ? )
    vsprintf [ "kernel" use-vocab ( -- x ) (eval) ]
    with-interactive-vocabs ;

: results ( str -- seq )
    replace-vars dup count-vars truth-table
    [ swap solve-expr unparse ] with map ;

: main ( -- )
    prompt
    [ header t ]
    [ replace-vars count-vars truth-table ]
    [ results [ "| " prepend ] map ] tri
    add-col print-table drop ;

MAIN: main
```

```txt

Please enter a boolean expression using 1-long
variable names and postfix notation. Available
operators are and, or, not, and xor. Example:
> a b and

> a b or

a b | a b or

### ===========

t t | t
t f | t
f t | t
f f | f


Please enter a boolean expression using 1-long
variable names and postfix notation. Available
operators are and, or, not, and xor. Example:
> a b and

> x y and z xor not

x y z | x y and z xor not

### ===========

t t t | t
t t f | f
t f t | f
t f f | t
f t t | f
f t f | t
f f t | f
f f f | t

```


=={{header|Fōrmulæ}}==

In [http://wiki.formulae.org/Truth_table this] page you can see the solution of this task.

Fōrmulæ programs are not textual, visualization/edition of programs is done showing/manipulating structures but not text ([http://wiki.formulae.org/Editing_F%C5%8Drmul%C3%A6_expressions more info]). Moreover, there can be multiple visual representations of the same program. Even though it is possible to have textual representation &mdash;i.e. XML, JSON&mdash; they are intended for transportation effects more than visualization and edition.

The option to show Fōrmulæ programs and their results is showing images. Unfortunately images cannot be uploaded in Rosetta Code.


## Go

Expression parsing and evaluation taken from the Arithmetic evaluation task.  Operator precedence and association are that of the Go language, and are determined by the library parser.  The unary ^ is first, then &, then | and ^ associating left to right.  Note also that the symbols &, |, and ^ operate bitwise on integer types in Go, but here since we implement our own evaluator we can apply them to the type of bool.

```go
package main

import (
    "bufio"
    "errors"
    "fmt"
    "go/ast"
    "go/parser"
    "go/token"
    "os"
    "reflect"
)

func main() {
    in := bufio.NewScanner(os.Stdin)
    for {
        fmt.Print("Expr:  ")
        in.Scan()
        if err := in.Err(); err != nil {
            fmt.Println(err)
            return
        }
        if !tt(in.Text()) {
            return
        }
    }
}

func tt(expr string) bool {
    // call library parser
    tree, err := parser.ParseExpr(expr)
    if err != nil {
        fmt.Println(err)
        return false
    }
    // create handy object to pass around
    e := &evaluator{nil, map[string]bool{}, tree}
    // library tree traversal function calls e.Visit for each node.
    // use this to collect variables of the expression.
    ast.Walk(e, tree)
    // print headings for truth table
    for _, n := range e.names {
        fmt.Printf("%-6s", n)
    }
    fmt.Println(" ", expr)
    // start recursive table generation function on first variable
    e.evalVar(0)
    return true
}

type evaluator struct {
    names []string        // variables, in order of appearance
    val   map[string]bool // map variables to boolean values
    tree  ast.Expr        // parsed expression as ast
}

// visitor function called by library Walk function.
// builds a list of unique variable names.
func (e *evaluator) Visit(n ast.Node) ast.Visitor {
    if id, ok := n.(*ast.Ident); ok {
        if !e.val[id.Name] {
            e.names = append(e.names, id.Name)
            e.val[id.Name] = true
        }
    }
    return e
}

// method recurses for each variable of the truth table, assigning it to
// false, then true.  At bottom of recursion, when all variables are
// assigned, it evaluates the expression and outputs one line of the
// truth table
func (e *evaluator) evalVar(nx int) bool {
    if nx == len(e.names) {
        // base case
        v, err := evalNode(e.tree, e.val)
        if err != nil {
            fmt.Println(" ", err)
            return false
        }
        // print variable values
        for _, n := range e.names {
            fmt.Printf("%-6t", e.val[n])
        }
        // print expression value
        fmt.Println(" ", v)
        return true
    }
    // recursive case
    for _, v := range []bool{false, true} {
        e.val[e.names[nx]] = v
        if !e.evalVar(nx + 1) {
            return false
        }
    }
    return true
}

// recursively evaluate ast
func evalNode(nd ast.Node, val map[string]bool) (bool, error) {
    switch n := nd.(type) {
    case *ast.Ident:
        return val[n.Name], nil
    case *ast.BinaryExpr:
        x, err := evalNode(n.X, val)
        if err != nil {
            return false, err
        }
        y, err := evalNode(n.Y, val)
        if err != nil {
            return false, err
        }
        switch n.Op {
        case token.AND:
            return x && y, nil
        case token.OR:
            return x || y, nil
        case token.XOR:
            return x != y, nil
        default:
            return unsup(n.Op)
        }
    case *ast.UnaryExpr:
        x, err := evalNode(n.X, val)
        if err != nil {
            return false, err
        }
        switch n.Op {
        case token.XOR:
            return !x, nil
        default:
            return unsup(n.Op)
        }
    case *ast.ParenExpr:
        return evalNode(n.X, val)
    }
    return unsup(reflect.TypeOf(nd))
}

func unsup(i interface{}) (bool, error) {
    return false, errors.New(fmt.Sprintf("%v unsupported", i))
}

```

Output:

```txt

Expr:  A ^ B
A     B       A ^ B
false false   false
false true    true
true  false   true
true  true    false
Expr:  S | ( T ^ U )
S     T     U       S | ( T ^ U )
false false false   false
false false true    true
false true  false   true
false true  true    false
true  false false   true
true  false true    true
true  true  false   true
true  true  true    true
Expr:  d^b&(c^d)
d     b     c       d^b&(c^d)
false false false   false
false false true    false
false true  false   false
false true  true    true
true  false false   true
true  false true    true
true  true  false   false
true  true  true    true

```



## Haskell



###  Reverse Polish Notation

Accepts expressions given in RPN, tokenized by whitespace.
Uses operators "&", "|", "!", "^" (xor), "=>" (implication); all other words are interpreted as variable names.


```haskell
import Control.Monad (mapM, foldM, forever)
import Data.List (unwords, unlines, nub)
import Data.Maybe (fromJust)

truthTable expr = let
    tokens = words expr
    operators = ["&", "|", "!", "^", "=>"]
    variables = nub $ filter (not . (`elem` operators)) tokens
    table = zip variables <$> mapM (const [True,False]) variables
    results = map (\r -> (map snd r) ++ (calculate tokens) r) table
    header = variables ++ ["result"]
    in
      showTable $ header : map (map show) results

-- Performs evaluation of token sequence in a given context.
-- The context is an assoc-list, which binds variable and it's value.
-- Here the monad is simple ((->) r).
calculate :: [String] -> [(String, Bool)] -> [Bool]
calculate = foldM interprete []
  where
    interprete (x:y:s) "&"  = (: s) <$> pure (x && y)
    interprete (x:y:s) "|"  = (: s) <$> pure (x || y)
    interprete (x:y:s) "^"  = (: s) <$> pure (x /= y)
    interprete (x:y:s) "=>" = (: s) <$> pure (not y || x)
    interprete (x:s)   "!"  = (: s) <$> pure (not x)
    interprete s var        = (: s) <$> fromJust . lookup var

-- pretty printing
showTable tbl = unlines $ map (unwords . map align) tbl
  where
    align txt = take colWidth $ txt ++ repeat ' '
    colWidth = max 6 $ maximum $ map length (head tbl)

main = forever $ getLine >>= putStrLn . truthTable
```


```txt
λ> main
x !
x      result
True   False
False  True

A B &
A      B      result
True   True   True
True   False  False
False  True   False
False  False  False

x1 x2 ! ^ x2 &
x1     x2     result
True   True   True
True   False  False
False  True   False
False  False  False

```



###  Infix Notation


Translation from infix notation to RPN using Parsec:

```haskell
{-# LANGUAGE FlexibleContexts #-}
import Text.Parsec

toRPN = parse impl "expression" . filter (/= ' ')
  where
    impl = chainl1 disj (op2 "=>")
    disj = chainl1 conj (op2 "|"  <|>  op2 "^")
    conj = chainl1 term (op2 "&")
    term = string "(" *> impl <* string ")" <|>
           op1 "!" <*> term <|>
           many1 alphaNum
    op1 s = (\x -> unwords [x, s])      <$ string s
    op2 s = (\x y -> unwords [x, y, s]) <$ string s
```


```haskell
λ> putStr $ truthTable $ toRPN "(Human => Mortal) & (Socratus => Human) => (Socratus => Mortal)"

Human  Mortal Socratus result
True   True   True     True
True   True   False    True
True   False  True     True
True   False  False    True
False  True   True     True
False  True   False    True
False  False  True     True
False  False  False    True
```



## J


Implementation:


```j
truthTable=:3 :0
  assert. -. 1 e. 'data expr names table' e.&;: y
  names=. ~. (#~ _1 <: nc) ;:expr=. y
  data=. #:i.2^#names
  (names)=. |:data
  (' ',;:inv names,<expr),(1+#@>names,<expr)":data,.".expr
)
```


The argument is expected to be a valid boolean J sentence which, among other things, does not use any of the words used within this implementation (but any single-character name is valid).

Example use:


```j
   truthTable '-.b'
 b -.b
 0   1
 1   0
   truthTable 'a*b'
 a b a*b
 0 0   0
 0 1   0
 1 0   0
 1 1   1
   truthTable 'a+.b'
 a b a+.b
 0 0    0
 0 1    1
 1 0    1
 1 1    1
   truthTable 'a<:b'
 a b a<:b
 0 0    1
 0 1    1
 1 0    0
 1 1    1
   truthTable '(a*bc)+.d'
 a bc d (a*bc)+.d
 0  0 0         0
 0  0 1         1
 0  1 0         0
 0  1 1         1
 1  0 0         0
 1  0 1         1
 1  1 0         1
 1  1 1         1
```



## Java

This takes an expression from the command line in reverse Polish notation. The supported operators are & | ^ ! and you probably need to escape them so that your shell doesn't interpret them. As an exercise for the reader, you could make it prompt the user for input (which would avoid the escaping issue), or accept infix expressions (see other examples here for how to turn infix into RPN).

```java
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.Stack;

public class TruthTable {
    public static void main( final String... args ) {
        System.out.println( new TruthTable( args ) );
    }

    private interface Operator {
        boolean evaluate( Stack<Boolean> s );
    }

    /**
     * Supported operators and what they do. For more ops, add entries here.
     */
    private static final Map<String,Operator> operators = new HashMap<String,Operator>() {{
        // Can't use && or || because shortcut evaluation may mean the stack is not popped enough
        put( "&", stack -> Boolean.logicalAnd( stack.pop(), stack.pop() ) );
        put( "|", stack -> Boolean.logicalOr( stack.pop(), stack.pop() ) );
        put( "!", stack -> ! stack.pop() );
        put( "^", stack -> ! stack.pop().equals ( stack.pop() ) );
    }};

    private final List<String> variables;
    private final String[]     symbols;

    /**
     * Constructs a truth table for the symbols in an expression.
     */
    public TruthTable( final String... symbols ) {
        final Set<String> variables = new LinkedHashSet<>();

        for ( final String symbol : symbols ) {
            if ( ! operators.containsKey( symbol ) ) {
                variables.add( symbol );
            }
        }
        this.variables = new ArrayList<>( variables );
        this.symbols = symbols;
    }

    @Override
    public String toString () {
        final StringBuilder result = new StringBuilder();

        for ( final String variable : variables ) {
            result.append( variable ).append( ' ' );
        }
        result.append( ' ' );
        for ( final String symbol : symbols ) {
            result.append( symbol ).append ( ' ' );
        }
        result.append( '\n' );
        for ( final List<Boolean> values : enumerate( variables.size () ) ) {
            final Iterator<String> i = variables.iterator();

            for ( final Boolean value : values ) {
                result.append(
                    String.format(
                        "%-" + i.next().length() + "c ",
                        value ? 'T' : 'F'
                    )
                );
            }
            result.append( ' ' )
                .append( evaluate( values ) ? 'T' : 'F' )
                .append( '\n' );
        }

        return result.toString ();
    }

    /**
     * Recursively generates T/F values
     */
    private static List<List<Boolean>> enumerate( final int size ) {
        if ( 1 == size )
            return new ArrayList<List<Boolean>>() {{
                add( new ArrayList<Boolean>() {{ add(false); }} );
                add( new ArrayList<Boolean>() {{ add(true);  }} );
            }};

        return new ArrayList<List<Boolean>>() {{
            for ( final List<Boolean> head : enumerate( size - 1 ) ) {
                add( new ArrayList<Boolean>( head ) {{ add(false); }} );
                add( new ArrayList<Boolean>( head ) {{ add(true);  }} );
            }
        }};
    }

    /**
     * Evaluates the expression for a set of values.
     */
    private boolean evaluate( final List<Boolean> enumeration ) {
        final Iterator<Boolean>   i      = enumeration.iterator();
        final Map<String,Boolean> values = new HashMap<>();
        final Stack<Boolean>      stack  = new Stack<>();

        variables.forEach ( v -> values.put( v, i.next() ) );
        for ( final String symbol : symbols ) {
            final Operator op = operators.get ( symbol );

            // Reverse Polish notation makes this bit easy
            stack.push(
                null == op
                    ? values.get ( symbol )
                    : op.evaluate ( stack )
            );
        }
        return stack.pop();
    }
}
```

Note that the escape character is ^ for Windows

```txt
C:\rosettacode> java TruthTable a b c ^^ ^|
a b c  a b c ^ |
F F F  F
F F T  T
F T F  T
F T T  F
T F F  T
T F T  T
T T F  T
T T T  T

C:\rosettacode> java TruthTable Jim Spock Bones ^^ ^& Scotty ^|
Jim Spock Bones Scotty  Jim Spock Bones ^ & Scotty |
F   F     F     F       F
F   F     F     T       T
F   F     T     F       F
F   F     T     T       T
F   T     F     F       F
F   T     F     T       T
F   T     T     F       F
F   T     T     T       T
T   F     F     F       F
T   F     F     T       T
T   F     T     F       T
T   F     T     T       T
T   T     F     F       T
T   T     F     T       T
T   T     T     F       F
T   T     T     T       T
```



## JavaScript

Actually a HTML document. Save as a .html document and double-click it. You should be fine.

```javascript
<!DOCTYPE html><html><head><title>Truth table</title><script>
var elem,expr,vars;
function isboolop(chr){return "&|!^".indexOf(chr)!=-1;}
function varsindexof(chr){
	var i;
	for(i=0;i<vars.length;i++){if(vars[i][0]==chr)return i;}
	return -1;
}
function printtruthtable(){
	var i,str;
	elem=document.createElement("pre");
	expr=prompt("Boolean expression:\nAccepts single-character variables (except for \"T\" and \"F\", which specify explicit true or false values), postfix, with \"&|!^\" for and, or, not, xor, respectively; optionally seperated by whitespace.").replace(/\s/g,"");
	vars=[];
	for(i=0;i<expr.length;i++)if(!isboolop(expr[i])&&expr[i]!="T"&&expr[i]!="F"&&varsindexof(expr[i])==-1)vars.push([expr[i],-1]);
	if(vars.length==0)return;
	str="";
	for(i=0;i<vars.length;i++)str+=vars[i][0]+" ";
	elem.innerHTML="<b>"+str+expr+"</b>\n";
	vars[0][1]=false;
	truthpartfor(1);
	vars[0][1]=true;
	truthpartfor(1);
	vars[0][1]=-1;
	document.body.appendChild(elem);
}
function truthpartfor(index){
	if(index==vars.length){
		var str,i;
		str="";
		for(i=0;i<index;i++)str+=(vars[i][1]?"<b>T</b>":"F")+" ";
		elem.innerHTML+=str+(parsebool()?"<b>T</b>":"F")+"\n";
		return;
	}
	vars[index][1]=false;
	truthpartfor(index+1);
	vars[index][1]=true;
	truthpartfor(index+1);
	vars[index][1]=-1;
}
function parsebool(){
	var stack,i,idx;
	console.log(vars);
	stack=[];
	for(i=0;i<expr.length;i++){
		if(expr[i]=="T")stack.push(true);
		else if(expr[i]=="F")stack.push(false);
		else if((idx=varsindexof(expr[i]))!=-1)stack.push(vars[idx][1]);
		else if(isboolop(expr[i])){
			switch(expr[i]){
				case "&":stack.push(stack.pop()&stack.pop());break;
				case "|":stack.push(stack.pop()|stack.pop());break;
				case "!":stack.push(!stack.pop());break;
				case "^":stack.push(stack.pop()^stack.pop());break;
			}
		} else alert("Non-conformant character "+expr[i]+" in expression. Should not be possible.");
		console.log(stack);
	}
	return stack[0];
}
</script></head><body onload="printtruthtable()"></body></html>
```

```txt
A B AB^
F F F
F T T
T F T
T T F
```

```txt
A B C ABC^|
F F F F
F F T T
F T F T
F T T F
T F F T
T F T T
T T F T
T T T T
```



## Julia

'''Module''':

```julia
module TruthTable

using Printf
using MacroTools

isvariablename(::Any) = false
isvariablename(s::Symbol) = all(x -> isletter(x) || x == '_', string(s))

function table(expr)
    if !isvariablename(expr) && !Meta.isexpr(expr, :call)
        throw(ArgumentError("expr must be a boolean expression"))
    end

    exprstr = string(expr)
    # Collect variable names
    symset = Set{Symbol}()
    MacroTools.prewalk(expr) do node
        isvariablename(node) && push!(symset, node)
        return node
    end
    symlist = collect(symset)

    # Create assignment assertions + evaluate
    blocks = Vector{Expr}(undef, 2 ^ length(symlist) + 1)
    blocks[1] = quote
        println(join(lpad.($(symlist), 6), " | "), " || ", $exprstr)
    end
    for (i, tup) in enumerate(Iterators.product(Iterators.repeated((false, true), length(symlist))...))
        blocks[i + 1] = quote
            let $(Expr(:(=), Expr(:tuple, symlist...), Expr(:tuple, tup...)))
                println(join(lpad.($(Expr(:tuple, symlist...)), 6), " | "), " || ", lpad($expr, $(length(exprstr))))
            end
        end
    end

    return esc(Expr(:block, blocks...))
end

macro table(expr)
    return table(expr)
end

end  # module TruthTable
```


'''Main''':

```julia
TruthTable.@table !a
TruthTable.@table a | b
TruthTable.@table (a ⊻ b) | (c & a)
TruthTable.@table (a & b) | (c ⊻ d)

```


```txt
     a || !a
 false || true
  true || false
     a |      b || a | b
 false |  false || false
  true |  false ||  true
 false |   true ||  true
  true |   true ||  true
     a |      b |      c || (a ⊻ b) | c & a
 false |  false |  false ||           false
  true |  false |  false ||            true
 false |   true |  false ||            true
  true |   true |  false ||           false
 false |  false |   true ||           false
  true |  false |   true ||            true
 false |   true |   true ||            true
  true |   true |   true ||            true
     a |      b |      d |      c || a & b | (c ⊻ d)
 false |  false |  false |  false ||           false
  true |  false |  false |  false ||           false
 false |   true |  false |  false ||           false
  true |   true |  false |  false ||            true
 false |  false |   true |  false ||            true
  true |  false |   true |  false ||            true
 false |   true |   true |  false ||            true
  true |   true |   true |  false ||            true
 false |  false |  false |   true ||            true
  true |  false |  false |   true ||            true
 false |   true |  false |   true ||            true
  true |   true |  false |   true ||            true
 false |  false |   true |   true ||           false
  true |  false |   true |   true ||           false
 false |   true |   true |   true ||           false
  true |   true |   true |   true ||            true

```



## Kotlin

```scala
// Version 1.2.31

import java.util.Stack

class Variable(val name: Char, var value: Boolean = false)

lateinit var expr: String
var variables = mutableListOf<Variable>()

fun Char.isOperator() = this in "&|!^"

fun Char.isVariable() = this in variables.map { it.name }

fun evalExpression(): Boolean {
    val stack = Stack<Boolean>()

    for (e in expr) {
        stack.push(
            if (e == 'T')
                true
            else if (e == 'F')
                false
            else if (e.isVariable())
                variables.single { it.name == e }.value
            else when (e) {
                '&'   -> stack.pop() and stack.pop()
                '|'   -> stack.pop() or  stack.pop()
                '!'   -> !stack.pop()
                '^'   -> stack.pop() xor stack.pop()
                else  -> throw RuntimeException("Non-conformant character '$e' in expression")
            }
        )
    }

    require(stack.size == 1)
    return stack.peek()
}

fun setVariables(pos: Int) {
    require(pos <= variables.size)
    if (pos == variables.size) {
        val vs = variables.map { if (it.value) "T" else "F" }.joinToString("  ")
        val es = if (evalExpression()) "T" else "F"
        return println("$vs  $es")
    }
    variables[pos].value = false
    setVariables(pos + 1)
    variables[pos].value = true
    setVariables(pos + 1)
}

fun main(args: Array<String>) {
    println("Accepts single-character variables (except for 'T' and 'F',")
    println("which specify explicit true or false values), postfix, with")
    println("&|!^ for and, or, not, xor, respectively; optionally")
    println("seperated by spaces or tabs. Just enter nothing to quit.")

    while (true) {
        print("\nBoolean expression: ")
        expr = readLine()!!.toUpperCase().replace(" ", "").replace("\t", "")
        if (expr == "") return
        variables.clear()
        for (e in expr) {
            if (!e.isOperator() && e !in "TF" && !e.isVariable()) variables.add(Variable(e))
        }
        if (variables.isEmpty()) return
        val vs = variables.map { it.name }.joinToString("  ")
        println("\n$vs  $expr")
        val h = vs.length + expr.length + 2
        repeat(h) { print("=") }
        println("\n")
        setVariables(0)
    }
}
```


Sample session:

```txt

Accepts single-character variables (except for 'T' and 'F',
which specify explicit true or false values), postfix, with
&|!^ for and, or, not, xor, respectively; optionally
seperated by spaces or tabs. Just enter nothing to quit.

Boolean expression: A B ^

A  B  AB^

### ===


F  F  F
F  T  T
T  F  T
T  T  F

Boolean expression: A B C ^ |

A  B  C  ABC^|

### ========


F  F  F  F
F  F  T  T
F  T  F  T
F  T  T  F
T  F  F  T
T  F  T  T
T  T  F  T
T  T  T  T

Boolean expression: A B C D ^ ^ ^

A  B  C  D  ABCD^^^

### =============


F  F  F  F  F
F  F  F  T  T
F  F  T  F  T
F  F  T  T  F
F  T  F  F  T
F  T  F  T  F
F  T  T  F  F
F  T  T  T  T
T  F  F  F  T
T  F  F  T  F
T  F  T  F  F
T  F  T  T  T
T  T  F  F  F
T  T  F  T  T
T  T  T  F  T
T  T  T  T  F

Boolean expression:

```



## Liberty BASIC

This at first seems trivial, given our lovely 'eval' function. However it is complicated by LB's use of 'non-zero' for 'true', and by the requirements of accepting different numbers and names of variables.
My program assumes all space-separated words in the expression$ are either a logic-operator, bracket delimiter, or variable name. Since a truth table for 8 or more variables is of silly length, I regard that as a practical limit.

```lb

print
    print " TRUTH TABLES"
    print
    print " Input a valid Boolean expression for creating the truth table "
    print " Use lowercase 'and', 'or', 'xor', '(', 'not(' and ')'."
    print
    print " Take special care to precede closing bracket with a space."
    print
    print " You can use any alphanumeric variable names, but no spaces."
    print " You can refer again to a variable used already."
    print " Program assumes <8 variables will be used.."
    print
    print " eg 'A xor B and not( C or A )'"
    print " or 'Too_High xor not( Fuel_Out )'"

    print

 [start]
    input "        "; expression$
    if expression$ ="" then [start]

    print

    'used$           =""
    numVariables    =0  '   count of detected variable names
    variableNames$  ="" '   filled with detected variable names
    i               =1  '   index to space-delimited word in the expression$

  [parse]
    m$ =word$( expression$, i, " ")
    if m$ ="" then [analyse]
    '   is it a reserved word, or a variable name already met?
    if m$ <>"and" and m$ <>"or" and m$ <>"not(" and m$ <>")" and m$ <>"xor"_
     and not( instr( variableNames$, m$)) then
        variableNames$ =variableNames$ +m$ +" ": numVariables =numVariables +1
    end if

    i =i +1
    goto [parse]

  [analyse]
    for i =1 to numVariables
        ex$          =FindReplace$( expression$, word$( variableNames$, i, " "), chr$( 64 +i), 1)
        expression$  =ex$
    next i

    'print " "; numVariables; " variables, simplifying to "; expression$

    print ,;
    for j =1 to numVariables
        print word$( variableNames$, j, " "),
    next j
    print "Result"
    print

    for i =0 to ( 2^numVariables) -1
        print ,;
        A                         =i mod 2:          print A,
        if numVariables >1 then B =int( i /2) mod 2: print B,
        if numVariables >2 then C =int( i /4) mod 2: print C,
        if numVariables >3 then D =int( i /4) mod 2: print D,
        if numVariables >4 then E =int( i /4) mod 2: print E,
        if numVariables >5 then F =int( i /4) mod 2: print F,
        if numVariables >6 then G =int( i /4) mod 2: print G,
        '   .......................... etc

        'e =eval( expression$)
        if eval( expression$) <>0 then e$ ="1" else e$ ="0"
        print "==>  "; e$
    next i

    print

    goto [start]

    end

function FindReplace$( FindReplace$, find$, replace$, replaceAll)
    if ( ( FindReplace$ <>"") and ( find$ <>"")) then
        fLen = len( find$)
        rLen = len( replace$)
        do
            fPos            = instr( FindReplace$, find$, fPos)
            if not( fPos) then exit function
            pre$            = left$( FindReplace$, fPos -1)
            post$           =  mid$( FindReplace$, fPos +fLen)
            FindReplace$    = pre$ +replace$ +post$
            fPos            = fPos +(rLen -fLen) +1
        loop while ( replaceAll)
    end if
end function

```


```txt

        Too_High and Fuel_Out
              Too_High      Fuel_Out      Result

              0             0             ==>  0
              1             0             ==>  0
              0             1             ==>  0
              1             1             ==>  1

        Fat or Ugly and not( Rich )
              Fat           Ugly          Rich          Result

              0             0             0             ==>  0
              1             0             0             ==>  1
              0             1             0             ==>  1
              1             1             0             ==>  1
              0             0             1             ==>  0
              1             0             1             ==>  0
              0             1             1             ==>  0
              1             1             1             ==>  0

```



## Mathematica


```Mathematica
VariableNames[data_] := Module[ {TokenRemoved},
 TokenRemoved = StringSplit[data,{"~And~","~Or~","~Xor~","!","(",")"}];
 Union[Select[Map[StringTrim,TokenRemoved] , Not[StringMatchQ[#,""]]&]]
]

TruthTable[BooleanEquation_] := Module[ {TestDataSet},
  TestDataSet = MapThread[Rule,{ToExpression@VariableNames[BooleanEquation],#}]&/@
     Tuples[{False,True}, Length[VariableNames[BooleanEquation]]];

  Join[List[Flatten[{VariableNames[BooleanEquation],BooleanEquation}]],
    Flatten[{#/.Rule[x_,y_] -> y,ReplaceAll[ToExpression[BooleanEquation],#]}]&/@TestDataSet]//Grid
]
```


Example usage:

```txt
TruthTable["V ~Xor~ (B ~Xor~ (K ~Xor~ D ) )"]

B	D	K	V	V ~Xor~ (B ~Xor~ (K ~Xor~ D ) )
False	False	False	False	False
False	False	False	True	True
False	False	True	False	True
False	False	True	True	False
False	True	False	False	True
False	True	False	True	False
False	True	True	False	False
False	True	True	True	True
True	False	False	False	True
True	False	False	True	False
True	False	True	False	False
True	False	True	True	True
True	True	False	False	False
True	True	False	True	True
True	True	True	False	True
True	True	True	True	False
```



## Maxima


```Maxima
/* Maxima already has the following logical operators
          =, # (not equal), not, and, or
define some more and set 'binding power' (operator
precedence) for them
*/
infix("xor", 60)$
"xor"(A,B):= (A or B) and not(A and B)$

infix("=>", 59)$
"=>"(A,B):= not A or B$

/*
Substitute variables `r' in `e' with values taken from list `l' where
`e' is expression, `r' is a list of independent variables, `l' is a
list of the values
lsubst( '(A + B), ['A, 'B], [1, 2]);
1 + 2;
*/
lsubst(e, r, l):= ev(e, maplist( lambda([x, y], x=y), r, l), 'simp)$

/*
"Cartesian power" `n' of list `b'. Returns a list of lists of the form
[<x_1>, ..., <x_n>], were <x_1>, .. <x_n> are elements of list `b'
cartesian_power([true, false], 2);
[[true, true], [true, false], [false, true], [false, false]];
cartesian_power([true, false], 3);
[[true, true, true], [true, true, false], [true, false, true],
[true, false, false], [false, true, true], [false, true, false],
[false, false, true], [false, false, false]];
*/
cartesian_power(b, n) := block(
    [aux_lst: makelist(setify(b), n)],
    listify(apply(cartesian_product, aux_lst))
    )$

gen_table(expr):= block(
  [var_lst: listofvars(expr), st_lst, res_lst, m],
  st_lst: cartesian_power([true, false], length(var_lst)),
  res_lst: create_list(lsubst(expr, var_lst, val_lst), val_lst, st_lst),
  m      : apply('matrix, cons(var_lst, st_lst)),
  addcol(m, cons(expr, res_lst))
  );

/* examples */
gen_table('(not A));
gen_table('(A xor B));
gen_table('(Jim and (Spock xor Bones) or Scotty));
gen_table('(A => (B and A)));
gen_table('(V xor (B xor (K xor D ) )));
```


OUtput of the last example:
<lang>
            [   V      B      K      D    V xor (B xor (K xor D)) ]
            [                                                     ]
            [ true   true   true   true            false          ]
            [                                                     ]
            [ true   true   true   false           true           ]
            [                                                     ]
            [ true   true   false  true            true           ]
            [                                                     ]
            [ true   true   false  false           false          ]
            [                                                     ]
            [ true   false  true   true            true           ]
            [                                                     ]
            [ true   false  true   false           false          ]
            [                                                     ]
            [ true   false  false  true            false          ]
            [                                                     ]
            [ true   false  false  false           true           ]
            [                                                     ]
            [ false  true   true   true            true           ]
            [                                                     ]
            [ false  true   true   false           false          ]
            [                                                     ]
            [ false  true   false  true            false          ]
            [                                                     ]
            [ false  true   false  false           true           ]
            [                                                     ]
            [ false  false  true   true            false          ]
            [                                                     ]
            [ false  false  true   false           true           ]
            [                                                     ]
            [ false  false  false  true            true           ]
            [                                                     ]
            [ false  false  false  false           false          ]

```



## PARI/GP

Uses infix Boolean expressions with <code>+</code> for OR, <code>*</code> for AND, and the constants <code>0</code> and <code>1</code> for FALSE and TRUE.

It would be easy to modify the program to take <code>+</code> for XOR instead.

```parigp
vars(P)={
	my(v=List(),x);
	while(type(P)=="t_POL",
		x=variable(P);
		listput(v,x);
		P=subst(P,x,1)
	);
	Vec(v)
};
truthTable(P)={
	my(var=vars(P),t,b);
	for(i=0,2^#var-1,
		t=eval(P);
		for(j=1,#var,
			b=bittest(i,j-1);
			t=subst(t,var[j],b);
			print1(b)
		);
		print(!!t)
	);
};
truthTable("x+y") \\ OR
truthTable("x*y") \\ AND
```

```txt
000
101
011
111

000
100
010
111
```



## Pascal

```Pascal

program TruthTables;
const
  StackSize = 80;

type
  TVariable = record
    Name: Char;
    Value: Boolean;
  end;

  TStackOfBool = record
    Top: Integer;
    Elements: array [0 .. StackSize - 1] of Boolean;
  end;

var
  Expression: string;
  Variables: array [0 .. 23] of TVariable;
  VariablesLength: Integer;
  i, h: Integer;
  e: Char;

// Stack manipulation functions
function IsFull(var s: TStackOfBool): Boolean;
begin
  IsFull := s.Top = StackSize - 1;
end;

function IsEmpty(var s: TStackOfBool): Boolean;
begin
  IsEmpty := s.Top = -1;
end;

function Peek(var s: TStackOfBool): Boolean;
begin
  if not IsEmpty(s) then
    Peek := s.Elements[s.Top]
  else
  begin
    Writeln('Stack is empty.');
    Halt;
  end;
end;

procedure Push(var s: TStackOfBool; val: Boolean);
begin
  if not IsFull(s) then
  begin
    Inc(s.Top);
    s.Elements[s.Top] := val;
  end
  else
  begin
    Writeln('Stack is full.');
    Halt;
  end
end;

function Pop(var s: TStackOfBool): Boolean;
begin
  if not IsEmpty(s) then
  begin
    Pop := s.Elements[s.Top];
    Dec(s.Top);
  end
  else
  begin
    Writeln;
    Writeln('Stack is empty.');
    Halt;
  end
end;

function IsOperator(const c: Char): Boolean;
begin
  IsOperator := (c = '&') or (c = '|') or (c = '!') or (c = '^');
end;

function VariableIndex(const c: Char): Integer;
var
  i: Integer;
begin
  for i := 0 to VariablesLength - 1 do
    if Variables[i].Name = c then
    begin
      VariableIndex := i;
      Exit;
    end;
  VariableIndex := -1;
end;

function EvaluateExpression: Boolean;
var
  i, vi: Integer;
  e: Char;
  s: TStackOfBool;
begin
  s.Top := -1;
  for i := 1 to Length(Expression) do
  begin
    e := Expression[i];
    vi := VariableIndex(e);
    if e = 'T' then
      Push(s, True)
    else if e = 'F' then
      Push(s, False)
    else if vi >= 0 then
      Push(s, Variables[vi].Value)
    else
    begin
      case e of
        '&':
          Push(s, Pop(s) and Pop(s));
        '|':
          Push(s, Pop(s) or Pop(s));
        '!':
          Push(s, not Pop(s));
        '^':
          Push(s, Pop(s) xor Pop(s));
      else
        Writeln;
        Writeln('Non-conformant character ', e, ' in expression.', e);
        Halt;
      end;
    end;
  end;
  if s.Top < 0 then
  begin
    Writeln;
    Writeln('Stack should contain exactly one element.');
    Halt;
  end;
  EvaluateExpression := Peek(s);
end;

procedure SetVariables(pos: Integer);
var
  i: Integer;
begin
  if pos > VariablesLength then
  begin
    Writeln;
    Writeln('Argument to SetVariables cannot be greater than the number of variables.');
    Halt;
  end
  else if pos = VariablesLength then
  begin
    for i := 0 to VariablesLength - 1 do
    begin
      if Variables[i].Value then
        Write('T  ')
      else
        Write('F  ');
    end;
    if EvaluateExpression then
      Writeln('T')
    else
      Writeln('F');
  end
  else
  begin
    Variables[pos].Value := False;
    SetVariables(pos + 1);
    Variables[pos].Value := True;
    SetVariables(pos + 1);
  end
end;

// removes space and converts to upper case
procedure ProcessExpression;
var
  i: Integer;
  exprTmp: string;
begin
  exprTmp := '';
  for i := 1 to Length(Expression) do
  begin
    if Expression[i] <> ' ' then
      exprTmp := Concat(exprTmp, Expression[i]);
  end;
  Expression := exprTmp
end;

begin
  Writeln('Accepts single-character variables (except for ''T'' and ''F'',');
  Writeln('which specify explicit true or false values), postfix, with');
  Writeln('&|!^ for and, or, not, xor, respectively; optionally');
  Writeln('seperated by space. Just enter nothing to quit.');

  while (True) do
  begin
    Writeln;
    Write('Boolean expression: ');
    ReadLn(Expression);
    ProcessExpression;
    if Length(Expression) = 0 then
      Break;
    VariablesLength := 0;
    for i := 1 to Length(Expression) do
    begin
      e := Expression[i];
      if (not IsOperator(e)) and (e <> 'T') and (e <> 'F') and
        (VariableIndex(e) = -1) then
      begin
        Variables[VariablesLength].Name := e;
        Variables[VariablesLength].Value := False;
        Inc(VariablesLength);
      end;
    end;
    if VariablesLength = 0 then
    begin
      Writeln;
      Writeln('No variables were entered.');
      Continue;
    end;
    Writeln;
    for i := 0 to VariablesLength - 1 do
    begin
      Write(Variables[i].Name, '  ');
    end;
    Writeln(Expression);
    h := VariablesLength * 3 + Length(Expression);
    for i := 0 to h - 1 do
      Write('=');
    Writeln;
    SetVariables(0);
  end;
end.

```

```txt

Accepts single-character variables (except for 'T' and 'F',
which specify explicit true or false values), postfix, with
&|!^ for and, or, not, xor, respectively; optionally
seperated by space. Just enter nothing to quit.

Boolean expression: A B ^

A  B  AB^

### ===

F  F  F
F  T  T
T  F  T
T  T  F

Boolean expression: A B C ^ |

A  B  C  ABC^|

### ========

F  F  F  F
F  F  T  T
F  T  F  T
F  T  T  F
T  F  F  T
T  F  T  T
T  T  F  T
T  T  T  T

Boolean expression: A B C D ^ ^ ^

A  B  C  D  ABCD^^^

### =============

F  F  F  F  F
F  F  F  T  T
F  F  T  F  T
F  F  T  T  F
F  T  F  F  T
F  T  F  T  F
F  T  T  F  F
F  T  T  T  T
T  F  F  F  T
T  F  F  T  F
T  F  T  F  F
T  F  T  T  T
T  T  F  F  F
T  T  F  T  T
T  T  T  F  T
T  T  T  T  F

Boolean expression:

```



## Perl

Note: can't process stuff like "X xor Y"; "xor" would be treated as a variable name here.

```perl
#!/usr/bin/perl

sub truth_table {
	my $s = shift;
	my (%seen, @vars);
	for ($s =~ /([a-zA-Z_]\w*)/g) {
		$seen{$_} //= do { push @vars, $_; 1 };
	}

	print "\n", join("\t", @vars, $s), "\n", '-' x 40, "\n";
	@vars = map("\$$_", @vars);

	$s =~ s/([a-zA-Z_]\w*)/\$$1/g;
	$s = "print(".join(',"\t", ', map("($_?'T':'F')", @vars, $s)).",\"\\n\")";
	$s = "for my $_ (0, 1) { $s }" for (reverse @vars);
	eval $s;
}

truth_table 'A ^ A_1';
truth_table 'foo & bar | baz';
truth_table 'Jim & (Spock ^ Bones) | Scotty';
```
```txt

A       A_1     A ^ A_1
----------------------------------------
F       F       F
F       T       T
T       F       T
T       T       F

foo     bar     baz     foo & bar | baz
----------------------------------------
F       F       F       F
F       F       T       T
F       T       F       F
F       T       T       T
T       F       F       F
T       F       T       T
T       T       F       T
T       T       T       T

Jim     Spock   Bones   Scotty  Jim & (Spock ^ Bones) | Scotty
----------------------------------------
F       F       F       F       F
...<snip for space -- not like you're gonna verify it anyway>...
T       T       T       T       T

```



## Perl 6

```perl6
use MONKEY-SEE-NO-EVAL;

sub MAIN ($x) {
    my @n = $x.comb(/<ident>/);
    my &fun = EVAL "-> {('\\' «~« @n).join(',')} \{ [{ (|@n,"so $x").join(',') }] \}";

    say (|@n,$x).join("\t");
    .join("\t").say for map &fun, flat map { .fmt("\%0{+@n}b").comb».Int».so }, 0 ..^ 2**@n;
    say '';
}
```

```txt

$ truthtable 'A ^ B'
A	B	A ^ B
False	False	False
False	True	True
True	False	True
True	True	False

$ truthtable 'foo & bar | baz'
foo	bar	baz	foo & bar | baz
False	False	False	False
False	False	True	True
False	True	False	False
False	True	True	True
True	False	False	False
True	False	True	True
True	True	False	True
True	True	True	True

$ truthtable 'Jim & (Spock ^ Bones) | Scotty'
Jim	Spock	Bones	Scotty	Jim & (Spock ^ Bones) | Scotty
False	False	False	False	False
False	False	False	True	True
False	False	True	False	False
False	False	True	True	True
False	True	False	False	False
False	True	False	True	True
False	True	True	False	False
False	True	True	True	True
True	False	False	False	False
True	False	False	True	True
True	False	True	False	True
True	False	True	True	True
True	True	False	False	True
True	True	False	True	True
True	True	True	False	False
True	True	True	True	True
```



## Phix

Expression parsing and evaluation similar to that in the Arithmetic evaluation task.

```Phix
sequence opstack = {}
object token
object op = 0   -- 0 = none
string s        -- the expression being parsed
integer sidx    -- idx to ""
integer ch      -- s[sidx]

procedure err(string msg)
    printf(1,"%s\n%s^ %s\n\nPressEnter...",{s,repeat(' ',sidx-1),msg})
    {} = wait_key()
    abort(0)
end procedure

procedure nxtch()
    sidx += 1
    ch = iff(sidx>length(s)?-1:s[sidx])
end procedure

procedure skipspaces()
    while find(ch," \t\r\n")!=0 do nxtch() end while
end procedure

procedure get_token()
    skipspaces()
    if find(ch,"()!") then
        token = s[sidx..sidx]
        nxtch()
    else
        integer tokstart = sidx
        if ch=-1 then token = "eof" return end if
        while 1 do
            nxtch()
            if ch<'A' then exit end if
        end while
        token = s[tokstart..sidx-1]
    end if
end procedure

procedure Match(string t)
    if token!=t then err(t&" expected") end if
    get_token()
end procedure

procedure PopFactor()
object p2 = opstack[$]
    if op="not" then
        opstack[$] = {0,op,p2}
    else
        opstack = opstack[1..$-1]
        opstack[$] = {opstack[$],op,p2}
    end if
    op = 0
end procedure

sequence names -- {"false","true",...}
sequence flags -- {   0,     1,  ,...}

procedure PushFactor(string t)
    if op!=0 then PopFactor() end if
    integer k = find(t,names)
    if k=0 then
        names = append(names,t)
        k = length(names)
    end if
    opstack = append(opstack,k)
end procedure

procedure PushOp(string t)
    if op!=0 then PopFactor() end if
    op = t
end procedure

procedure Factor()
    if token="not"
    or token="!" then
        get_token()
        Factor()
        if op!=0 then PopFactor() end if
        PushOp("not")
    elsif token="(" then
        get_token()
        Expr(0)
        Match(")")
    elsif not find(token,{"and","or","xor"}) then
        PushFactor(token)
        if ch!=-1 then
            get_token()
        end if
    else
        err("syntax error")
    end if
end procedure

constant {operators,
          precedence} = columnize({{"not",6},
                                   {"and",5},
                                   {"xor",4},
                                   {"or",3}})

procedure Expr(integer p)
    Factor()
    while 1 do
        integer k = find(token,operators)
        if k=0 then exit end if
        integer thisp = precedence[k]
        if thisp<p then exit end if
        get_token()
        Expr(thisp)
        PushOp(operators[k])
    end while
end procedure

function eval(object s)
    if atom(s) then
        if s>=1 then s = flags[s] end if
        return s
    end if
    object {lhs,op,rhs} = s
    lhs = eval(lhs)
    rhs = eval(rhs)
    if op="and" then
        return lhs and rhs
    elsif op="or" then
        return lhs or rhs
    elsif op="xor" then
        return lhs xor rhs
    elsif op="not" then
        return not rhs
    else
        ?9/0
    end if
end function

function next_comb()
    integer fdx = length(flags)
    while flags[fdx]=1 do
        flags[fdx] = 0
        fdx -= 1
    end while
    if fdx<=2 then return false end if  -- all done
    flags[fdx] = 1
    return true
end function

function fmt(bool b)
    return {"0","1"}[b+1]   -- for 0/1
--  return {"F","T"}[b+1]   -- for F/T
end function

procedure test(string expr)
    opstack = {}
    op = 0
    names = {"false","true"}
    s = expr
    sidx = 0
    nxtch()
    get_token()
    Expr(0)
    if op!=0 then PopFactor() end if
    if length(opstack)!=1 then err("some error") end if
    flags = repeat(0,length(names))
    flags[2] = 1 -- set "true" true
    printf(1,"%s  %s\n",{join(names[3..$]),s})
    while 1 do
        for i=3 to length(flags) do -- (skipping true&false)
            printf(1,"%s%s",{fmt(flags[i]),repeat(' ',length(names[i]))})
        end for
        printf(1," %s\n",{fmt(eval(opstack[1]))})
        if not next_comb() then exit end if
    end while
    puts(1,"\n")
end procedure

test("young and not (ugly or poor)")
while 1 do
    puts(1,"input expression:")
    string t = trim(gets(0))
    puts(1,"\n")
    if t="" then exit end if
    test(t)
end while
```

```txt

young ugly poor  young and not (ugly or poor)
0     0    0     0
0     0    1     0
0     1    0     0
0     1    1     0
1     0    0     1
1     0    1     0
1     1    0     0
1     1    1     0

input expression:

```



## PicoLisp


```PicoLisp
(de truthTable (Expr)
   (let Vars
      (uniq
         (make
            (setq Expr
               (recur (Expr)  # Convert infix to prefix notation
                  (cond
                     ((atom Expr) (link Expr))
                     ((== 'not (car Expr))
                        (list 'not (recurse (cadr Expr))) )
                     (T
                        (list
                           (cadr Expr)
                           (recurse (car Expr))
                           (recurse (caddr Expr)) ) ) ) ) ) ) )
      (for V Vars
         (prin (align -7 V)) )
      (prinl)
      (bind (mapcar cons Vars)
         (do (** 2 (length Vars))
            (for "V" Vars
               (space (if (print (val "V")) 6 4)) )
            (println (eval Expr))
            (find '(("V") (set "V" (not (val "V")))) Vars) ) ) ) )
```

Test:



```PicoLisp
: (truthTable (str "A and (B or C)"))
A      B      C
NIL    NIL    NIL    NIL
T      NIL    NIL    NIL
NIL    T      NIL    NIL
T      T      NIL    T
NIL    NIL    T      NIL
T      NIL    T      T
NIL    T      T      NIL
T      T      T      T

: (truthTable (str "not (Foo and (Bar or Mumble))"))
Foo    Bar    Mumble
NIL    NIL    NIL    T
T      NIL    NIL    T
NIL    T      NIL    T
T      T      NIL    NIL
NIL    NIL    T      T
T      NIL    T      NIL
NIL    T      T      T
T      T      T      NIL

: (truthTable (str "(A xor B) and (B or C)"))
A      B      C
NIL    NIL    NIL    NIL
T      NIL    NIL    NIL
NIL    T      NIL    T
T      T      NIL    NIL
NIL    NIL    T      NIL
T      NIL    T      T
NIL    T      T      T
T      T      T      NIL

: (truthTable (str "(A xor B) and ((not B) or C)"))
A      B      C
NIL    NIL    NIL    NIL
T      NIL    NIL    T
NIL    T      NIL    NIL
T      T      NIL    NIL
NIL    NIL    T      NIL
T      NIL    T      T
NIL    T      T      T
T      T      T      NIL
```



## Prolog

```prolog
/*
	To evaluate the truth table a line of text is inputted and then there are three steps
	Let's say the expression is:
	'not a and (b or c)'

	Step 1: tokenize into atoms and brackets
	eg: Tokenized = [ not, a, and, '(', b, or, c, ')' ].

	Step 2: convert to a term that can be evaluated, and get out the variables
	eg: Expression = op(and, op(not, a), op(or, b, c)), Variables = [ a, b, c ]

	Step 3: permeate over the variables, substituting the values for each var, and evaluate the expression for each permutation
	eg: [ 0, 0, 0]
		op(and, op(not, 0), op(or, 0, 0))
		op(and, 1, op(or, 0, 0))
		op(and, 1, 0)
		0

		[ 0, 0, 1]
		op(and, op(not, 0), op(or, 0, 1))
		op(and, 1, op(or, 0, 0))
		op(and, 1, 1)
		1
*/
truth_table :-
	current_input(In),
	read_line_to_codes(In, Line),
	atom_codes(A, Line),
	atom_chars(A, Chars),

	% parse everything into the form we want
	phrase(tok(Tok), Chars, _),
	phrase(expr(Expr,Vars), Tok, _),
	list_to_set(Vars,VarSet),

	% evaluate
	print_expr(Expr, VarSet), !.

print_expr(Expr, Vars) :-
	% write the header (once)
	maplist(format('~p '), Vars),
	format('~n'),

	% write the results for as many times as there are rows
	eval_expr(Expr, Vars, Tvals, R),
	maplist(format('~p '), Tvals),
	format('~p~n', R),
	fail.
print_expr(_, _).


% Step 1 - tokenize the input into spaces, brackets and atoms
tok([A|As]) --> spaces(_), chars([X|Xs]), {atom_codes(A, [X|Xs])}, spaces(_), tok(As).
tok([A|As]) --> spaces(_), bracket(A), spaces(_), tok(As).
tok([]) --> [].
chars([X|Xs]) --> char(X), { dif(X, ')'), dif(X, '(') }, !, chars(Xs).
chars([]) --> [].
spaces([X|Xs]) --> space(X), !, spaces(Xs).
spaces([]) --> [].
bracket('(') --> ['('].
bracket(')') --> [')'].


% Step 2 - Parse the expression into an evaluable term
expr(op(I, E, E2), V) --> starter(E, V1), infix(I), expr(E2, V2), { append(V1, V2, V) }.
expr(E, V) --> starter(E, V).

starter(op(not, E),V) --> [not], expr(E, V).
starter(E,V) --> ['('], expr(E,V), [')'].
starter(V,[V]) --> variable(V).

infix(or) --> [or].
infix(and) --> [and].
infix(xor) --> [xor].
infix(nand) --> [nand].

variable(V) --> [V], \+ infix(V), \+ bracket(V).
space(' ') --> [' '].
char(X) --> [X], { dif(X, ' ') }.


% Step 3 - evaluate the parsed expression
eval_expr(Expr, Vars, Tvals, R) :-
	length(Vars,Len),
	length(Tvals, Len),
	maplist(truth_val, Tvals),
	eval(Expr, [Tvals,Vars],R).

eval(X, [Vals,Vars], R) :- nth1(N,Vars,X), nth1(N,Vals,R).
eval(op(Op,A,B), V, R) :- eval(A,V,Ae), eval(B,V,Be), e(Op,Ae,Be,R).
eval(op(not,A), V, R) :- eval(A,V,Ae), e(not,Ae,R).

truth_val(0). truth_val(1).

e(or,0,0,0). e(or,0,1,1). e(or,1,0,1). e(or,1,1,1).
e(and,0,0,0). e(and,0,1,0). e(and,1,0,0). e(and,1,1,1).
e(xor,0,0,0). e(xor,0,1,1). e(xor,1,0,1). e(xor,1,1,0).
e(nand,0,0,1). e(nand,0,1,1). e(nand,1,0,1). e(nand,1,1,0).
e(not, 1, 0). e(not, 0, 1).
```

```txt

?- truth_table.
|: not a and (b or c)
a b c
0 0 0 0
0 0 1 1
0 1 0 1
0 1 1 1
1 0 0 0
1 0 1 0
1 1 0 0
1 1 1 0
true.

?-

```



## Python

This accepts correctly formatted Python boolean expressions.

```python
from itertools import product

while True:
    bexp = input('\nBoolean expression: ')
    bexp = bexp.strip()
    if not bexp:
        print("\nThank you")
        break
    code = compile(bexp, '<string>', 'eval')
    names = code.co_names
    print('\n' + ' '.join(names), ':', bexp)
    for values in product(range(2), repeat=len(names)):
        env = dict(zip(names, values))
        print(' '.join(str(v) for v in values), ':', eval(code, env))

```


;Sample output:
<pre style="height: 40ex; overflow: scroll">Boolean expression: A ^ B

A B : A ^ B
0 0 : 0
0 1 : 1
1 0 : 1
1 1 : 0

Boolean expression: S | ( T ^ U )

S T U : S | ( T ^ U )
0 0 0 : 0
0 0 1 : 1
0 1 0 : 1
0 1 1 : 0
1 0 0 : 1
1 0 1 : 1
1 1 0 : 1
1 1 1 : 1

Boolean expression: A ^ (B ^ (C ^ D))

A B C D : A ^ (B ^ (C ^ D))
0 0 0 0 : 0
0 0 0 1 : 1
0 0 1 0 : 1
0 0 1 1 : 0
0 1 0 0 : 1
0 1 0 1 : 0
0 1 1 0 : 0
0 1 1 1 : 1
1 0 0 0 : 1
1 0 0 1 : 0
1 0 1 0 : 0
1 0 1 1 : 1
1 1 0 0 : 0
1 1 0 1 : 1
1 1 1 0 : 1
1 1 1 1 : 0

Boolean expression:

Thank you
```



## Racket


Since the requirement is to read an expression dynamically, <tt>eval</tt> is a natural choice.  The following isn't trying to protect against bad inputs when doing that.


```Racket

#lang racket

(define (collect-vars sexpr)
  (sort
   (remove-duplicates
    (let loop ([x sexpr])
      (cond [(boolean? x) '()]
            [(symbol? x) (list x)]
            [(list? x) (append-map loop (cdr x))]
            [else (error 'truth-table "Bad expression: ~e" x)])))
   string<? #:key symbol->string))

(define ns (make-base-namespace))

(define (truth-table sexpr)
  (define vars (collect-vars sexpr))
  (printf "~a => ~s\n" (string-join (map symbol->string vars)) sexpr)
  (for ([i (expt 2 (length vars))])
    (define vals
      (map (λ(x) (eq? #\1 x))
           (reverse (string->list (~r i #:min-width (length vars)
                                        #:pad-string "0"
                                        #:base 2)))))
    (printf "~a => ~a\n" (string-join (map (λ(b) (if b "T" "F")) vals))
            (if (eval `(let (,@(map list vars vals)) ,sexpr) ns) "T" "F"))))

(printf "Enter an expression: ")
(truth-table (read))

```


Sample run:

```txt

Enter an expression: (and (or z x) (or y (not z)))
x y z => (and (or z x) (or y (not z)))
F F F => F
T F F => T
F T F => F
T T F => T
F F T => F
T F T => F
F T T => T
T T T => T

```



## REXX

I had the thought that this program would just transform the boolean expression into what REXX approves of, and just step

through the 26 possible propositional constants (which makes a deeply nested DO construct, if nothing else, it looks pretty).

I later added support for all 16 boolean functions --- REXX natively supports three infix operators:
::*   '''&'''     (and)
::*   '''|'''        (or)
::*   '''&&'''     (xor)
and one prefix operator:
::*   '''¬'''     (not,   negation).
Some REXX interpreters also (or instead) support:
::*   '''\'''     (backslash)
::*   '''/'''     (forward slash,   solidus)
::*   '''~'''     (tilde)
::*   '''^'''     (caret,   circumflex,   hat)
Also included is support for two boolean values: '''TRUE''' and '''FALSE''' which are part of boolean expressions.

```rexx
/*REXX program displays a truth table of  variables and an expression.   Infix notation */
/*─────────────── is supported with one character propositional constants;  variables   */
/*─────────────── (propositional constants) that are allowed:  A──►Z,  a──►z   except u.*/
/*─────────────── All propositional constants are case insensitive (except lowercase u).*/

parse arg userText                               /*get optional expression from the CL. */
if userText\=''  then do                         /*Got one?   Then show user's stuff.   */
                      call truthTable userText   /*display truth table for the userText.*/
                      exit                       /*we're finished with the user's text. */
                      end

call truthTable  "G ^ H ; XOR"                   /*text after ; is echoed to the output.*/
call truthTable  "i | j ; OR"
call truthTable  "G nxor H ; NXOR"
call truthTable  "k ! t ; NOR"
call truthTable  "p & q ; AND"
call truthTable  "e ¡ f ; NAND"
call truthTable  "S | (T ^ U)"
call truthTable  "(p=>q) v (q=>r)"
call truthTable  "A ^ (B ^ (C ^ D))"
exit                                             /*quit while we're ahead,  by golly.   */

    /* ↓↓↓ no way, Jose. ↓↓↓ */                  /* [↓]  shows a 32,768 line truth table*/
call truthTable  "A^ (B^ (C^ (D^ (E^ (F^ (G^ (H^ (I^ (J^ (L^ (L^ (M^ (N^O)  ))))))))))))"
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
truthTable: procedure; parse arg $ ';' comm 1 $o;        $o=  strip($o);      hdrPCs=
               $= translate(strip($), '|', "v");         $u=  $;              upper $u
              $u= translate($u, '()()()', "[]{}«»");     $$.= 0;              PCs=
            @abc= 'abcdefghijklmnopqrstuvwxyz';          @abcU= @abc;         upper @abcU

/* ╔═════════════════════╦════════════════════════════════════════════════════════════╗
   ║                     ║                  bool(bitsA, bitsB, BF)                    ║
   ║                     ╟────────────────────────────────────────────────────────────╢
   ║                     ║ performs the boolean function  BF    ┌──────┬─────────┐    ║
   ║                     ║      on the   A   bitstring          │  BF  │ common  │    ║
   ║                     ║    with the   B   bitstring.         │ value│  name   │    ║
   ║                     ║                                      ├──────┼─────────┤    ║
   ║                     ║ BF   must be a  one to four bit      │ 0000 │boolfalse│    ║
   ║                     ║ value  (from  0000 ──► 1111),        │ 0001 │ and     │    ║
   ║  This boxed table   ║ leading zeroes can be omitted.       │ 0010 │ NaIMPb  │    ║
   ║ was re─constructed  ║                                      │ 0011 │ boolB   │    ║
   ║   from an old IBM   ║ BF   may have multiple values (one   │ 0100 │ NbIMPa  │    ║
   ║    publicastion:    ║ for each pair of bitstrings):        │ 0101 │ boolA   │    ║
   ║                     ║                                      │ 0110 │ xor     │    ║
   ║   "PL/I Language    ║  ┌──────┬──────┬───────────────┐     │ 0111 │ or      │    ║
   ║   Specifications"   ║  │ Abit │ Bbit │   returns     │     │ 1000 │ nor     │    ║
   ║                     ║  ├──────┼──────┼───────────────┤     │ 1001 │ nxor    │    ║
   ║                     ║  │   0  │   0  │ 1st bit in BF │     │ 1010 │ notB    │    ║
   ║                     ║  │   0  │   1  │ 2nd bit in BF │     │ 1011 │ bIMPa   │    ║
   ║   ─── March 1969.   ║  │   1  │   0  │ 3rd bit in BF │     │ 1100 │ notA    │    ║
   ║                     ║  │   1  │   1  │ 4th bit in BF │     │ 1101 │ aIMPb   │    ║
   ║                     ║  └──────┴──────┴───────────────┘     │ 1110 │ nand    │    ║
   ║                     ║                                      │ 1111 │booltrue │    ║
   ║                     ║                                   ┌──┴──────┴─────────┤    ║
   ║                     ║                                   │ A  0101           │    ║
   ║                     ║                                   │ B  0011           │    ║
   ║                     ║                                   └───────────────────┘    ║
   ╚═════════════════════╩════════════════════════════════════════════════════════════╝ */

  @= 'ff'x                                       /* [↓]  ───── infix operators (0──►15) */
  op.=                                           /*Note:   a  single quote  (')  wasn't */
                                                 /*            implemented for negation.*/
  op.0 = 'false  boolFALSE'                      /*unconditionally  FALSE               */
  op.1 = '&      and *'                          /* AND,  conjunction                   */
  op.2 = 'naimpb NaIMPb'                         /*not A implies B                      */
  op.3 = 'boolb  boolB'                          /*B  (value of)                        */
  op.4 = 'nbimpa NbIMPa'                         /*not B implies A                      */
  op.5 = 'boola  boolA'                          /*A  (value of)                        */
  op.6 = '&&     xor % ^'                        /* XOR,  exclusive OR                  */
  op.7 = '|      or + v'                         /*  OR,  disjunction                   */
  op.8 = 'nor    nor ! ↓'                        /* NOR,  not OR,  Pierce operator      */
  op.9 = 'xnor   xnor nxor'                      /*NXOR,  not exclusive OR,  not XOR    */
  op.10= 'notb   notB'                           /*not B  (value of)                    */
  op.11= 'bimpa  bIMPa'                          /*    B  implies A                     */
  op.12= 'nota   notA'                           /*not A  (value of)                    */
  op.13= 'aimpb  aIMPb'                          /*    A  implies B                     */
  op.14= 'nand   nand ¡ ↑'                       /*NAND,  not AND,  Sheffer operator    */
  op.15= 'true   boolTRUE'                       /*unconditionally   TRUE               */
                                                 /*alphabetic names that need changing. */
  op.16= '\   NOT ~ ─ . ¬'                       /* NOT,  negation                      */
  op.17= '>   GT'                                /*conditional                          */
  op.18= '>=  GE ─> => ──> ==>'   "1a"x          /*conditional;     (see note below.)──┐*/
  op.19= '<   LT'                                /*conditional                         │*/
  op.20= '<=  LE <─ <= <── <=='                  /*conditional                         │*/
  op.21= '\=  NE ~= ─= .= ¬='                    /*conditional                         │*/
  op.22= '=   EQ EQUAL EQUALS ='  "1b"x          /*bi─conditional;  (see note below.)┐ │*/
  op.23= '0   boolTRUE'                          /*TRUEness                          │ │*/
  op.24= '1   boolFALSE'                         /*FALSEness                         ↓ ↓*/
                                                 /* [↑] glphys  '1a'x  and  "1b"x  can't*/
                                                 /*     displayed under most DOS' & such*/
    do jj=0  while  op.jj\=='' | jj<16           /*change opers ──► into what REXX likes*/
    new= word(op.jj, 1)                          /*obtain the 1st token of  infex table.*/
                                                 /* [↓]  process the rest of the tokens.*/
      do kk=2  to words(op.jj)                   /*handle each of the tokens separately.*/
      _=word(op.jj, kk);          upper _        /*obtain another token from infix table*/
      if wordpos(_, $u)==0   then iterate        /*no such animal in this string.       */
      if datatype(new, 'm')  then new!= @        /*it            needs to be transcribed*/
                             else new!= new      /*it  doesn't   need   "  "     "      */
      $u= changestr(_, $u, new!)                 /*transcribe the function (maybe).     */
      if new!==@  then $u= changeFunc($u,@,new)  /*use the internal boolean name.       */
      end   /*kk*/
    end     /*jj*/

  $u=translate($u, '()', "{}")                   /*finish cleaning up the transcribing. */

        do jj=1  for length(@abcU)               /*see what variables are being used.   */
        _= substr(@abcU, jj, 1)                  /*use the available upercase aLphabet. */
        if pos(_,$u) == 0  then iterate          /*Found one?    No, then keep looking. */
        $$.jj= 1                                 /*found:  set upper bound for it.      */
          PCs= PCs _                             /*also, add to propositional constants.*/
        hdrPCs=hdrPCS center(_,length('false'))  /*build a PC header for transcribing.  */
        end   /*jj*/

  ptr= '_────►_'                                 /*a (text) pointer for the truth table.*/
   $u= PCs '('$u")"                              /*separate the  PCs  from expression.  */
  hdrPCs= substr(hdrPCs, 2)                      /*create a header for the  PCs.        */
  say hdrPCs left('', length(ptr) - 1)   $o      /*display  PC  header and expression.  */
  say copies('───── ', words(PCs))    left('', length(ptr) -2)  copies('─', length($o))
                                                 /*Note:  "true"s:  are right─justified.*/
                do a=0  to $$.1
                 do b=0  to $$.2
                  do c=0  to $$.3
                   do d=0  to $$.4
                    do e=0  to $$.5
                     do f=0  to $$.6
                      do g=0  to $$.7
                       do h=0  to $$.8
                        do i=0  to $$.9
                         do j=0  to $$.10
                          do k=0  to $$.11
                           do l=0  to $$.12
                            do m=0  to $$.13
                             do n=0  to $$.14
                              do o=0  to $$.15
                               do p=0  to $$.16
                                do q=0  to $$.17
                                 do r=0  to $$.18
                                  do s=0  to $$.19
                                   do t=0  to $$.20
                                    do u=0  to $$.21
                                     do !=0  to $$.22
                                      do w=0  to $$.23
                                       do x=0  to $$.24
                                        do y=0  to $$.25
                                         do z=0  to $$.26;         interpret   '_='   $u
 /*evaluate truth T.*/
                                         _= changestr(1, _, '_true') /*convert 1──►_true*/
                                         _= changestr(0, _, 'false') /*convert 0──►false*/
                                         _= insert(ptr,  _, wordindex(_, words(_) )  - 1)
                                         say translate(_, , '_')     /*display truth tab*/
                                         end   /*z*/
                                        end    /*y*/
                                       end     /*x*/
                                      end      /*w*/
                                     end       /*v*/
                                    end        /*u*/
                                   end         /*t*/
                                  end          /*s*/
                                 end           /*r*/
                                end            /*q*/
                               end             /*p*/
                              end              /*o*/
                             end               /*n*/
                            end                /*m*/
                           end                 /*l*/
                          end                  /*k*/
                         end                   /*j*/
                        end                    /*i*/
                       end                     /*h*/
                      end                      /*g*/
                     end                       /*f*/
                    end                        /*e*/
                   end                         /*d*/
                  end                          /*c*/
                 end                           /*b*/
                end                            /*a*/
  say;  say
  return
/*──────────────────────────────────────────────────────────────────────────────────────*/
scan: procedure; parse arg x,at;      L= length(x);   t=L;    Lp=0;    apost=0;    quote=0
      if at<0  then      do;   t=1;   x= translate(x, '()', ")(")
                         end                      /* [↓]  get 1 or 2 chars at location J*/

            do j=abs(at)  to t  by sign(at);      _=substr(x, j ,1);   __=substr(x, j, 2)
            if quote             then do;  if _\=='"'    then iterate
                                           if __=='""'   then do;  j= j+1;  iterate;  end
                                           quote=0;  iterate
                                      end
            if apost             then do;  if _\=="'"    then iterate
                                           if __=="''"   then do;  j= j+1;  iterate;  end
                                           apost=0;   iterate
                                      end
            if _== '"'           then do;  quote=1;   iterate;  end
            if _== "'"           then do;  apost=1;   iterate;  end
            if _== ' '           then iterate
            if _== '('           then do;  Lp= Lp+1;  iterate;  end
            if Lp\==0            then do;  if _==')'     then Lp= Lp-1;     iterate;  end
            if datatype(_, 'U')  then return j - (at<0)
            if at<0              then return j + 1              /*is   _    uppercase ? */
            end   /*j*/

      return min(j, L)
/*──────────────────────────────────────────────────────────────────────────────────────*/
changeFunc: procedure;  parse arg z, fC, newF ;           funcPos= 0

              do forever
              funcPos= pos(fC, z, funcPos + 1);           if funcPos==0  then return z
              origPos= funcPos
                    z= changestr(fC, z, ",'"newF"',") /*arg 3 ≡  ",'" || newF || "-',"  */
              funcPos= funcPos + length(newF) + 4
                where= scan(z, funcPos)       ;           z= insert(    '}',  z,  where)
                where= scan(z, 1 - origPos)   ;           z= insert('bool{',  z,  where)
              end   /*forever*/
/*──────────────────────────────────────────────────────────────────────────────────────*/
bool: procedure; arg a,?,b                              /* ◄─── ARG uppercases all args.*/

                          select                        /*SELECT chooses which function.*/
                 /*0*/    when ? == 'FALSE'   then  return 0
                 /*1*/    when ? == 'AND'     then  return a & b
                 /*2*/    when ? == 'NAIMPB'  then  return \ (\a & \b)
                 /*3*/    when ? == 'BOOLB'   then  return b
                 /*4*/    when ? == 'NBIMPA'  then  return \ (\b & \a)
                 /*5*/    when ? == 'BOOLA'   then  return a
                 /*6*/    when ? == 'XOR'     then  return a && b
                 /*7*/    when ? == 'OR'      then  return a |  b
                 /*8*/    when ? == 'NOR'     then  return \ (a |  b)
                 /*9*/    when ? == 'XNOR'    then  return \ (a && b)
                 /*a*/    when ? == 'NOTB'    then  return \ b
                 /*b*/    when ? == 'BIMPA'   then  return \ (b & \a)
                 /*c*/    when ? == 'NOTA'    then  return \ a
                 /*d*/    when ? == 'AIMPB'   then  return \ (a & \b)
                 /*e*/    when ? == 'NAND'    then  return \ (a &  b)
                 /*f*/    when ? == 'TRUE'    then  return 1
                          otherwise                 return -13
                          end   /*select*/              /* [↑]  error, unknown function.*/
```

Some older REXXes don't have a   '''changestr'''   BIF, so one is included here   ──►   [[CHANGESTR.REX]].

(Output is shown at three-quarter size.)
<pre style="font-size:75%;height:115ex">
  G     H          G ^ H ; XOR
───── ─────        ───────────
false false  ────► false
false  true  ────►  true
 true false  ────►  true
 true  true  ────► false


  I     J          i | j ; OR
───── ─────        ──────────
false false  ────► false
false  true  ────►  true
 true false  ────►  true
 true  true  ────►  true


  G     H          G nxor H ; NXOR
───── ─────        ───────────────
false false  ────►  true
false  true  ────► false
 true false  ────► false
 true  true  ────►  true


  K     T          k ! t ; NOR
───── ─────        ───────────
false false  ────►  true
false  true  ────► false
 true false  ────► false
 true  true  ────► false


  P     Q          p & q ; AND
───── ─────        ───────────
false false  ────► false
false  true  ────► false
 true false  ────► false
 true  true  ────►  true


  E     F          e ¡ f ; NAND
───── ─────        ────────────
false false  ────►  true
false  true  ────►  true
 true false  ────►  true
 true  true  ────► false


  S     T     U          S | (T ^ U)
───── ───── ─────        ───────────
false false false  ────► false
false false  true  ────►  true
false  true false  ────►  true
false  true  true  ────► false
 true false false  ────►  true
 true false  true  ────►  true
 true  true false  ────►  true
 true  true  true  ────►  true


  P     Q     R          (p=>q) v (q=>r)
───── ───── ─────        ───────────────
false false false  ────►  true
false false  true  ────►  true
false  true false  ────►  true
false  true  true  ────►  true
 true false false  ────►  true
 true false  true  ────►  true
 true  true false  ────►  true
 true  true  true  ────►  true


  A     B     C     D          A ^ (B ^ (C ^ D))
───── ───── ───── ─────        ─────────────────
false false false false  ────► false
false false false  true  ────►  true
false false  true false  ────►  true
false false  true  true  ────► false
false  true false false  ────►  true
false  true false  true  ────► false
false  true  true false  ────► false
false  true  true  true  ────►  true
 true false false false  ────►  true
 true false false  true  ────► false
 true false  true false  ────► false
 true false  true  true  ────►  true
 true  true false false  ────► false
 true  true false  true  ────►  true
 true  true  true false  ────►  true
 true  true  true  true  ────► false

```



## Ruby

Uses <code>eval</code>, so blindly trusts the user's input. The core <code>true</code> and <code>false</code> objects understand the methods <code>&</code> (and), <code>|</code> (or), <code>!</code> (not) and <code>^</code> (xor) -- [http://www.ruby-doc.org/core-1.9.2/TrueClass.html]

```ruby
loop do
  print "\ninput a boolean expression (e.g. 'a & b'): "
  expr = gets.strip.downcase
  break if expr.empty?

  vars = expr.scan(/\p{Alpha}+/)
  if vars.empty?
    puts "no variables detected in your boolean expression"
    next
  end

  vars.each {|v| print "#{v}\t"}
  puts "| #{expr}"

  prefix = []
  suffix = []
  vars.each do |v|
    prefix << "[false, true].each do |#{v}|"
    suffix << "end"
  end

  body = vars.inject("puts ") {|str, v| str + "#{v}.to_s + '\t' + "}
  body += '"| " + eval(expr).to_s'

  eval (prefix + [body] + suffix).join("\n")
end
```


Example
<pre style="height: 40ex; overflow: scroll">input a boolean expression (e.g. 'a & b'): !a
a       | !a
false   | true
true    | false

input a boolean expression (e.g. 'a & b'): a|!b
a       b       | a|!b
false   false   | true
false   true    | false
true    false   | true
true    true    | true

input a boolean expression (e.g. 'a & b'): ((a^b)^c)^d
a       b       c       d       | ((a^b)^c)^d
false   false   false   false   | false
false   false   false   true    | true
false   false   true    false   | true
false   false   true    true    | false
false   true    false   false   | true
false   true    false   true    | false
false   true    true    false   | false
false   true    true    true    | true
true    false   false   false   | true
true    false   false   true    | false
true    false   true    false   | false
true    false   true    true    | true
true    true    false   false   | false
true    true    false   true    | true
true    true    true    false   | true
true    true    true    true    | false
```



## Sidef

A simple solution which accepts arbitrary user-input:

```ruby
loop {
  var expr = Sys.readln("\nBoolean expression (e.g. 'a & b'): ").strip.lc
  break if expr.is_empty;

  var vars = expr.scan(/[[:alpha:]]+/)
  if (vars.is_empty) {
    say "no variables detected in your boolean expression"
    next
  }

  var prefix = [];
  var suffix = [];

  vars.each { |v|
    print "#{v}\t"
    prefix << "[false, true].each { |#{v}|"
    suffix << "}"
  }
  say "| #{expr}"

  var body = ("say (" + vars.map{|v| v+",'\t'," }.join + " '| ', #{expr})")
  eval(prefix + [body] + suffix -> join("\n"))
}
```

```txt

Boolean expression (e.g. 'a & b'): (a & b) | c
a	b	c	| (a & b) | c
false	false	false	| false
false	false	true	| true
false	true	false	| false
false	true	true	| true
true	false	false	| false
true	false	true	| true
true	true	false	| true
true	true	true	| true

```



## Tcl


```tcl
package require Tcl 8.5

puts -nonewline "Enter a boolean expression: "
flush stdout
set exp [gets stdin]

# Generate the nested loops as the body of a lambda term.
set vars [lsort -unique [regexp -inline -all {\$\w+} $exp]]
set cmd [list format [string repeat "%s\t" [llength $vars]]%s]
append cmd " {*}\[[list subst $vars]\] \[[list expr $exp]\]"
set cmd "puts \[$cmd\]"
foreach v [lreverse $vars] {
    set cmd [list foreach [string range $v 1 end] {0 1} $cmd]
}

puts [join $vars \t]\tResult
apply [list {} $cmd]
```

Sample run:

```txt

Enter a boolean expression: ($a&&$b)||$c
$a	$b	$c	Result
0	0	0	0
0	0	1	1
0	1	0	0
0	1	1	1
1	0	0	0
1	0	1	1
1	1	0	1
1	1	1	1

```


