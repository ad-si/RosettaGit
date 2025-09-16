+++
title = "Compiler/syntax analyzer"
description = ""
date = 2019-05-15T20:49:56Z
aliases = []
[extra]
id = 21137
[taxonomies]
categories = ["task"]
tags = []
languages = [
  "algol_w",
  "awk",
  "c",
  "cobol",
  "forth",
  "go",
  "java",
  "julia",
  "m2000_interpreter",
  "perl",
  "phix",
  "python",
  "scheme",
]
+++

## Task

{{task}}Syntax Analyzer

A Syntax analyzer transforms a token stream (from the [[Compiler/lexical_analyzer|Lexical analyzer]])
into a Syntax tree, based on a grammar.

Take the output from the Lexical analyzer [[Compiler/lexical_analyzer|task]],
and convert it to an [https://en.wikipedia.org/wiki/Abstract_syntax_tree Abstract Syntax Tree (AST)],
based on the grammar below.  The output should be in a [[Flatten_a_list|flattened format.]]

The program should read input from a file and/or stdin, and write output to a file and/or
stdout.  If the language being used has a parser module/library/class, it would be great
if two versions of the solution are provided:  One without the parser module, and one
with.

The simple programming language to be analyzed is more or less a (very tiny) subset of
[[C]]. The formal grammar in
[https://en.wikipedia.org/wiki/Extended_Backus%E2%80%93Naur_Form Extended Backus-Naur Form (EBNF)]:


```EBNF

    stmt_list           =   {stmt} ;

    stmt                =   ';'
                          | Identifier '=' expr ';'
                          | 'while' paren_expr stmt
                          | 'if' paren_expr stmt ['else' stmt]
                          | 'print' '(' prt_list ')' ';'
                          | 'putc' paren_expr ';'
                          | '{' stmt_list '}'
                          ;

    paren_expr          =   '(' expr ')' ;

    prt_list            =   (string | expr) {',' (String | expr)} ;

    expr                =   and_expr            {'||' and_expr} ;
    and_expr            =   equality_expr       {'&&' equality_expr} ;
    equality_expr       =   relational_expr     [('==' | '!=') relational_expr] ;
    relational_expr     =   addition_expr       [('<' | '<=' | '>' | '>=') addition_expr] ;
    addition_expr       =   multiplication_expr {('+' | '-') multiplication_expr} ;
    multiplication_expr =   primary             {('*' | '/' | '%') primary } ;
    primary             =   Identifier
                          | Integer
                          | '(' expr ')'
                          | ('+' | '-' | '!') primary
                          ;
```


The resulting AST should be formulated as a Binary Tree.

;Example - given the simple program (below), stored in a file called while.t, create the list of tokens, using one of the Lexical analyzer [[Compiler/lexical_analyzer|solutions]]

 lex < while.t > while.lex

;Run one of the Syntax analyzer [[Compiler/syntax_analyzer|solutions]]:

 parse < while.lex > while.ast

;The following table shows the input to lex, lex output, and the AST produced by the parser:

{| class="wikitable"
|-
! Input to lex
! Output from lex, input to parse
! Output from parse
|-
| style="vertical-align:top" |

```c
count = 1;
 while (count < 10) {
     print("count is: ", count, "\n");
     count = count + 1;
 }
```


| style="vertical-align:top" |
<b>
```txt

    1      1 Identifier      count
    1      7 Op_assign
    1      9 Integer             1
    1     10 Semicolon
    2      1 Keyword_while
    2      7 LeftParen
    2      8 Identifier      count
    2     14 Op_less
    2     16 Integer            10
    2     18 RightParen
    2     20 LeftBrace
    3      5 Keyword_print
    3     10 LeftParen
    3     11 String          "count is: "
    3     23 Comma
    3     25 Identifier      count
    3     30 Comma
    3     32 String          "\n"
    3     36 RightParen
    3     37 Semicolon
    4      5 Identifier      count
    4     11 Op_assign
    4     13 Identifier      count
    4     19 Op_add
    4     21 Integer             1
    4     22 Semicolon
    5      1 RightBrace
    6      1 End_of_input

```
</b>

| style="vertical-align:top" |
<b>
```txt

Sequence
Sequence
;
Assign
Identifier    count
Integer       1
While
Less
Identifier    count
Integer       10
Sequence
Sequence
;
Sequence
Sequence
Sequence
;
Prts
String        "count is: "
;
Prti
Identifier    count
;
Prts
String        "\n"
;
Assign
Identifier    count
Add
Identifier    count
Integer       1

```
</b>
|}

;Specifications

;List of node type names:


```txt

Identifier String Integer Sequence If Prtc Prts Prti While Assign Negate Not Multiply Divide Mod
Add Subtract Less LessEqual Greater GreaterEqual Equal NotEqual And Or

```


In the text below, Null/Empty nodes are represented by ";".

;Non-terminal (internal) nodes:

For Operators, the following nodes should be created:

 Multiply Divide Mod Add Subtract Less LessEqual Greater GreaterEqual Equal NotEqual And Or

For each of the above nodes, the left and right sub-nodes are the operands of the
respective operation.

In pseudo S-Expression format:

 (Operator expression expression)

Negate, Not

For these node types, the left node is the operand, and the right node is null.

 (Operator expression ;)

Sequence - sub-nodes are either statements or Sequences.

If - left node is the expression, the right node is If node, with it's left node being the
if-true statement part, and the right node being the if-false (else) statement part.

 (If expression (If statement else-statement))

If there is not an else, the tree becomes:

 (If expression (If statement ;))

Prtc

 (Prtc (expression) ;)

Prts

 (Prts (String "the string") ;)

Prti

 (Prti (Integer 12345) ;)

While - left node is the expression, the right node is the statement.

 (While expression statement)

Assign - left node is the left-hand side of the assignment, the right node is the
right-hand side of the assignment.

 (Assign Identifier expression)

Terminal (leaf) nodes:

 Identifier: (Identifier ident_name)
 Integer:    (Integer 12345)
 String:     (String "Hello World!")
 ";":        Empty node

;Some simple examples
Sequences denote a list node; they are used to represent a list. semicolon's represent a null node, e.g., the end of this path.

This simple program:

    a=11;

Produces the following AST, encoded as a binary tree:

Under each non-leaf node are two '|' lines.  The first represents the left sub-node, the second represents the right sub-node:

    (1) Sequence
    (2)     |-- ;
    (3)     |-- Assign
    (4)         |-- Identifier: a
    (5)         |-- Integer: 11

In flattened form:

    (1) Sequence
    (2) ;
    (3) Assign
    (4) Identifier    a
    (5) Integer       11


This program:

    a=11;
    b=22;
    c=33;

Produces the following AST:

    ( 1) Sequence
    ( 2)     |-- Sequence
    ( 3)     |   |-- Sequence
    ( 4)     |   |   |-- ;
    ( 5)     |   |   |-- Assign
    ( 6)     |   |       |-- Identifier: a
    ( 7)     |   |       |-- Integer: 11
    ( 8)     |   |-- Assign
    ( 9)     |       |-- Identifier: b
    (10)     |       |-- Integer: 22
    (11)     |-- Assign
    (12)         |-- Identifier: c
    (13)         |-- Integer: 33

In flattened form:

    ( 1) Sequence
    ( 2) Sequence
    ( 3) Sequence
    ( 4) ;
    ( 5) Assign
    ( 6) Identifier    a
    ( 7) Integer       11
    ( 8) Assign
    ( 9) Identifier    b
    (10) Integer       22
    (11) Assign
    (12) Identifier    c
    (13) Integer       33

;Pseudo-code for the parser.

Uses [https://www.engr.mun.ca/~theo/Misc/exp_parsing.htm Precedence Climbing] for expression parsing, and
[https://en.wikipedia.org/wiki/Recursive_descent_parser Recursive Descent] for statement parsing. The AST is also built:


```python
def expr(p)
    if tok is "("
        x = paren_expr()
    elif tok in ["-", "+", "!"]
        gettok()
        y = expr(precedence of operator)
        if operator was "+"
            x = y
        else
            x = make_node(operator, y)
    elif tok is an Identifier
        x = make_leaf(Identifier, variable name)
        gettok()
    elif tok is an Integer constant
        x = make_leaf(Integer, integer value)
        gettok()
    else
        error()

    while tok is a binary operator and precedence of tok >= p
        save_tok = tok
        gettok()
        q = precedence of save_tok
        if save_tok is not right associative
            q += 1
        x = make_node(Operator save_tok represents, x, expr(q))

    return x

def paren_expr()
    expect("(")
    x = expr(0)
    expect(")")
    return x

def stmt()
    t = NULL
    if accept("if")
        e = paren_expr()
        s = stmt()
        t = make_node(If, e, make_node(If, s, accept("else") ? stmt() : NULL))
    elif accept("putc")
        t = make_node(Prtc, paren_expr())
        expect(";")
    elif accept("print")
        expect("(")
        repeat
            if tok is a string
                e = make_node(Prts, make_leaf(String, the string))
                gettok()
            else
                e = make_node(Prti, expr(0))

            t = make_node(Sequence, t, e)
        until not accept(",")
        expect(")")
        expect(";")
    elif tok is ";"
        gettok()
    elif tok is an Identifier
        v = make_leaf(Identifier, variable name)
        gettok()
        expect("=")
        t = make_node(Assign, v, expr(0))
        expect(";")
    elif accept("while")
        e = paren_expr()
        t = make_node(While, e, stmt()
    elif accept("{")
        while tok not equal "}" and tok not equal end-of-file
            t = make_node(Sequence, t, stmt())
        expect("}")
    elif tok is end-of-file
        pass
    else
        error()
    return t

def parse()
    t = NULL
    gettok()
    repeat
        t = make_node(Sequence, t, stmt())
    until tok is end-of-file
    return t
```


;Once the AST is built, it should be output in a [[Flatten_a_list|flattened format.]]  This can be as simple as the following:


```python
def prt_ast(t)
    if t == NULL
        print(";\n")
    else
        print(t.node_type)
        if t.node_type in [Identifier, Integer, String]     # leaf node
            print the value of the Ident, Integer or String, "\n"
        else
            print("\n")
            prt_ast(t.left)
            prt_ast(t.right)
```


;If the AST is correctly built, loading it into a subsequent program should be as simple as:


```python
def load_ast()
    line = readline()
    # Each line has at least one token
    line_list = tokenize the line, respecting double quotes

    text = line_list[0] # first token is always the node type

    if text == ";"   # a terminal node
        return NULL

    node_type = text # could convert to internal form if desired

    # A line with two tokens is a leaf node
    # Leaf nodes are: Identifier, Integer, String
    # The 2nd token is the value
    if len(line_list) > 1
        return make_leaf(node_type, line_list[1])

    left = load_ast()
    right = load_ast()
    return make_node(node_type, left, right)
```


Finally, the AST can also be tested by running it against one of the AST Interpreter [[Compiler/AST_interpreter|solutions]].

;Test program, assuming this is in a file called prime.t:  lex <prime.t | parse

{| class="wikitable"
|-
! Input to lex
! Output from lex, input to parse
! Output from parse
|-
| style="vertical-align:top" |

```c
/*
 Simple prime number generator
 */
count = 1;
n = 1;
limit = 100;
while (n < limit) {
    k=3;
    p=1;
    n=n+2;
    while ((k*k<=n) && (p)) {
        p=n/k*k!=n;
        k=k+2;
    }
    if (p) {
        print(n, " is prime\n");
        count = count + 1;
    }
}
print("Total primes found: ", count, "\n");
```


| style="vertical-align:top" |
<b>
```txt

    4      1 Identifier      count
    4      7 Op_assign
    4      9 Integer             1
    4     10 Semicolon
    5      1 Identifier      n
    5      3 Op_assign
    5      5 Integer             1
    5      6 Semicolon
    6      1 Identifier      limit
    6      7 Op_assign
    6      9 Integer           100
    6     12 Semicolon
    7      1 Keyword_while
    7      7 LeftParen
    7      8 Identifier      n
    7     10 Op_less
    7     12 Identifier      limit
    7     17 RightParen
    7     19 LeftBrace
    8      5 Identifier      k
    8      6 Op_assign
    8      7 Integer             3
    8      8 Semicolon
    9      5 Identifier      p
    9      6 Op_assign
    9      7 Integer             1
    9      8 Semicolon
   10      5 Identifier      n
   10      6 Op_assign
   10      7 Identifier      n
   10      8 Op_add
   10      9 Integer             2
   10     10 Semicolon
   11      5 Keyword_while
   11     11 LeftParen
   11     12 LeftParen
   11     13 Identifier      k
   11     14 Op_multiply
   11     15 Identifier      k
   11     16 Op_lessequal
   11     18 Identifier      n
   11     19 RightParen
   11     21 Op_and
   11     24 LeftParen
   11     25 Identifier      p
   11     26 RightParen
   11     27 RightParen
   11     29 LeftBrace
   12      9 Identifier      p
   12     10 Op_assign
   12     11 Identifier      n
   12     12 Op_divide
   12     13 Identifier      k
   12     14 Op_multiply
   12     15 Identifier      k
   12     16 Op_notequal
   12     18 Identifier      n
   12     19 Semicolon
   13      9 Identifier      k
   13     10 Op_assign
   13     11 Identifier      k
   13     12 Op_add
   13     13 Integer             2
   13     14 Semicolon
   14      5 RightBrace
   15      5 Keyword_if
   15      8 LeftParen
   15      9 Identifier      p
   15     10 RightParen
   15     12 LeftBrace
   16      9 Keyword_print
   16     14 LeftParen
   16     15 Identifier      n
   16     16 Comma
   16     18 String          " is prime\n"
   16     31 RightParen
   16     32 Semicolon
   17      9 Identifier      count
   17     15 Op_assign
   17     17 Identifier      count
   17     23 Op_add
   17     25 Integer             1
   17     26 Semicolon
   18      5 RightBrace
   19      1 RightBrace
   20      1 Keyword_print
   20      6 LeftParen
   20      7 String          "Total primes found: "
   20     29 Comma
   20     31 Identifier      count
   20     36 Comma
   20     38 String          "\n"
   20     42 RightParen
   20     43 Semicolon
   21      1 End_of_input

```
</b>

| style="vertical-align:top" |
<b>
```txt

Sequence
Sequence
Sequence
Sequence
Sequence
;
Assign
Identifier    count
Integer       1
Assign
Identifier    n
Integer       1
Assign
Identifier    limit
Integer       100
While
Less
Identifier    n
Identifier    limit
Sequence
Sequence
Sequence
Sequence
Sequence
;
Assign
Identifier    k
Integer       3
Assign
Identifier    p
Integer       1
Assign
Identifier    n
Add
Identifier    n
Integer       2
While
And
LessEqual
Multiply
Identifier    k
Identifier    k
Identifier    n
Identifier    p
Sequence
Sequence
;
Assign
Identifier    p
NotEqual
Multiply
Divide
Identifier    n
Identifier    k
Identifier    k
Identifier    n
Assign
Identifier    k
Add
Identifier    k
Integer       2
If
Identifier    p
If
Sequence
Sequence
;
Sequence
Sequence
;
Prti
Identifier    n
;
Prts
String        " is prime\n"
;
Assign
Identifier    count
Add
Identifier    count
Integer       1
;
Sequence
Sequence
Sequence
;
Prts
String        "Total primes found: "
;
Prti
Identifier    count
;
Prts
String        "\n"
;

```
</b>
|}

; Additional examples

Your solution should pass all the test cases above and the additional tests found '''[[Compiler/Sample_programs|Here]]'''.

The C and Python versions can be considered reference implementations.

;Related Tasks

* [[Compiler/lexical_analyzer|Lexical Analyzer task]]
* [[Compiler/code_generator|Code Generator task]]
* [[Compiler/virtual_machine_interpreter|Virtual Machine Interpreter task]]
* [[Compiler/AST_interpreter|AST Interpreter task]]

<hr>
__TOC__


## ALGOL W


```algolw
begin % syntax analyser %
    % parse tree nodes %
    record node( integer         type
               ; reference(node) left, right
               ; integer         iValue % nString/nIndentifier number or nInteger value %
               );
    integer     nIdentifier, nString, nInteger, nSequence, nIf,   nPrtc, nPrts
          ,     nPrti,       nWhile,  nAssign,  nNegate,   nNot,  nMultiply
          ,     nDivide,     nMod,    nAdd,     nSubtract, nLess, nLessEqual
          ,     nGreater,    nGreaterEqual,     nEqual,    nNotEqual,    nAnd, nOr
          ;
    string(14) array ndName ( 1 :: 25 );
    % tokens - names must match those output by the lexical analyser %
    integer     tkType, tkLine, tkColumn, tkLength, tkIntegerValue;
    integer     tOp_multiply   , tOp_divide        , tOp_mod       , tOp_add
          ,     tOp_subtract   , tOp_negate        , tOp_less      , tOp_lessequal
          ,     tOp_greater    , tOp_greaterequal  , tOp_equal     , tOp_notequal
          ,     tOp_not        , tOp_assign        , tOp_and       , tOp_or
          ,     tLeftParen     , tRightParen       , tLeftBrace    , tRightBrace
          ,     tSemicolon     , tComma            , tKeyword_if   , tKeyword_else
          ,     tKeyword_while , tKeyword_print    , tKeyword_putc , tIdentifier
          ,     tInteger       , tString           , tEnd_of_input
          ,     MAX_TOKEN_TYPE, PRIMARY_PREC
          ;
    string(16)  array tkName         ( 1 :: 31 );
    integer     array tkPrec, tkNode ( 1 :: 31 );
    % string literals and identifiers - uses a linked list - a hash table might be better... %
    string(1)   array text ( 0 :: 4095 );
    integer     textNext, TEXT_MAX;
    record textElement ( integer start, length; reference(textElement) next );
    reference(textElement) idList, stList;

    % returns a new node with left and right branches %
    reference(node) procedure opNode ( integer value opType; reference(node) value opLeft, opRight ) ; begin
        node( opType, opLeft, opRight, 0 )
    end opNode ;

    % returns a new operand node %
    reference(node) procedure operandNode ( integer value opType, opValue ) ; begin
        node( opType, null, null, opValue )
    end operandNode ;

    % reports an error %
    procedure synError( integer value line, column; string(80) value message ); begin
        integer errorPos;
        write( i_w := 1, s_w := 0, "**** Error at(", line, ",", column, "): " );
        errorPos := 0;
        while errorPos < 80 and message( errorPos // 1 ) not = "." do begin
            writeon( s_w := 0, message( errorPos // 1 ) );
            errorPos := errorPos + 1
        end while_not_at_end_of_message ;
        writeon( s_w := 0, "." )
    end synError ;

    % reports an error and stops %
    procedure fatalError( integer value line, column; string(80) value message ); begin
        synError( line, column, message );
        assert( false )
    end fatalError ;

    % prints a node and its sub-nodes %
    procedure writeNode( reference(node) value n ) ; begin
        % prints an identifier or string from text %
        procedure writeOnText( reference(textElement) value txHead; integer value txNumber ) ;
        begin
            reference(textElement) txPos;
            integer                count;
            txPos := txHead;
            count := 1;
            while count < txNumber and txPos not = null do begin
                txPos := next(txPos);
                count := count + 1
            end while_text_element_not_found ;
            if txPos = null then fatalError( 0, txNumber, "INTERNAL ERROR: text not found." )
            else for cPos := 0 until length(txPos) - 1 do writeon( text( start(txPos) + cPos ) );
            if text( start(txPos) ) = """" then writeon( """" );
        end writeOnText ;

        if n = null then write( ";" )
        else begin
            write( ndName( type(n) ) );
            if      type(n) = nInteger    then writeon( iValue(n) )
            else if type(n) = nIdentifier then writeOnText( idList, iValue(n) )
            else if type(n) = nString     then writeOnText( stList, iValue(n) )
            else begin
                writeNode(  left(n) );
                writeNode( right(n) )
            end
        end
    end writeNode ;

    % reads a token from standard input %
    procedure readToken ; begin

        % parses a string from line and stores it in a string in the text array %
        % - if it is not already present in the specified textElement list.     %
        % returns the position of the string in the text array                  %
        integer procedure readString ( reference(textElement) value result txList; string(1) value terminator ) ; begin
            string(256) str;
            integer     sLen, sPos, ePos;
            logical     found;
            reference(textElement) txPos, txLastPos;
            % get the text of the string %
            str  := " ";
            sLen := 0;
            str( sLen // 1 ) := line( lPos // 1 );
            sLen := sLen + 1;
            lPos := lPos + 1;
            while lPos <= 255 and line( lPos // 1 ) not = terminator do begin
                str( sLen // 1 ) := line( lPos // 1 );
                sLen := sLen + 1;
                lPos := lPos + 1
            end while_more_string ;
            if lPos > 255 then fatalError( tkLine, tkColumn, "Unterminated String in token file." );
            % attempt to find the text in the list of strings/identifiers %
            txLastPos := txPos := txList;
            found := false;
            ePos := 0;
            while not found and txPos not = null do begin
                ePos  := ePos + 1;
                found := ( length(txPos) = sLen );
                sPos  := 0;
                while found and sPos < sLen do begin
                    found := str( sPos // 1 ) = text( start(txPos) + sPos );
                    sPos  := sPos + 1
                end while_not_found ;
                txLastPos := txPos;
                if not found then txPos := next(txPos)
            end while_string_not_found ;
            if not found then begin
                % the string/identifier is not in the list - add it %
                ePos := ePos + 1;
                if txList = null then txList := textElement( textNext, sLen, null )
                                 else next(txLastPos) := textElement( textNext, sLen, null );
                if textNext + sLen > TEXT_MAX then fatalError( tkLine, tkColumn, "Text space exhausted." )
                else begin
                    for cPos := 0 until sLen - 1 do begin
                        text( textNext ) := str( cPos // 1 );
                        textNext := textNext + 1
                    end for_cPos
                end
            end if_not_found ;
            ePos
        end readString ;

        % gets an integer from the line - no checks for valid digits %
        integer procedure readInteger ; begin
            integer n;
            while line( lPos // 1 ) = " " do lPos := lPos + 1;
            n := 0;
            while line( lPos // 1 ) not = " " do begin
                n    := ( n * 10 ) + ( decode( line( lPos // 1 ) ) - decode( "0" ) );
                lPos := lPos + 1
            end while_not_end_of_integer ;
            n
        end readInteger ;

        string(256) line;
        string(16)  name;
        integer     lPos, tPos;
        tPos := lPos := 0;
        readcard( line );
        % get the line and column numbers %
        tkLine   := readInteger;
        tkColumn := readInteger;
        % get the token name %
        while line( lPos // 1 ) = " " do lPos := lPos + 1;
        name := "";
        while lPos < 256 and line( lPos // 1 ) not = " " do begin
            name( tPos // 1 ) := line( lPos // 1 );
            lPos := lPos + 1;
            tPos := tPos + 1
        end  while_more_name ;
        % determine the token type %
        tkType         := 1;
        tkIntegerValue := 0;
        while tkType <= MAX_TOKEN_TYPE and name not = tkName( tkType ) do tkType := tkType + 1;
        if tkType > MAX_TOKEN_TYPE then fatalError( tkLine, tkColumn, "Malformed token" );
        % handle the additional parameter for identifier/string/integer %
        if tkType = tInteger or tkType = tIdentifier or tkType = tString then begin
            while line( lPos // 1 ) = " " do lPos := lPos + 1;
            if      tkType = tInteger    then tkIntegerValue := readInteger
            else if tkType = tIdentifier then tkIntegerValue := readString( idList, " "  )
            else  % tkType = tString     %    tkIntegerValue := readString( stList, """" )
        end if_token_with_additional_parameter ;
    end readToken ;

    % parses a statement %
    reference(node) procedure parseStatement ; begin
        reference(node) stmtNode, stmtExpr;

        % skips the current token if it is expectedToken,              %
        % returns true if the token was expectedToken, false otherwise %
        logical procedure have ( integer value expectedToken ) ; begin
            logical haveExpectedToken;
            haveExpectedToken := ( tkType = expectedToken );
            if haveExpectedToken and tkType not = tEnd_of_input then readToken;
            haveExpectedToken
        end have ;

        % issues an error message and skips past the next semi-colon or to end of input %
        procedure skipStatement ( string(80) value message ) ; begin
            synError( tkLine, tkColumn, message );
            while tkType not = tEnd_of_input and not have( tSemicolon ) do readToken
        end skipStatement ;

        % checks we have a semicolon, issues an error and skips the statement if not %
        procedure mustBeEndOfStatement ; begin
            if not have( tSemicolon ) then skipStatement( """;"" expected." )
        end mustBeEndOfStatement ;

        % skips the current token if it is "(" and issues an error if it isn't %
        procedure mustBeLeftParen ; begin
            if not have( tLeftParen ) then synError( tkLine, tkColumn, """("" expected." )
        end % mustBeLeftParen % ;

        % skips the current token if it is ")" and issues an error if it isn't %
        procedure mustBeRightParen ; begin
            if not have( tRightParen ) then synError( tkLine, tkColumn, """)"" expected." )
        end % mustBeRightParen % ;

        % gets the next token and parses an expression with the specified precedence %
        reference(node) procedure nextAndparseExpr ( integer value precedence ) ; begin
            readToken;
            parseExpr( precedence )
        end nextAndParseExpr ;

        % parses an expression with the specified precedence %
        % all operators are assumed to be left-associative %
        reference(node) procedure parseExpr ( integer value precedence ) ; begin

            % handles a single token primary %
            reference(node) procedure simplePrimary ( integer value primaryNodeType ) ; begin
                reference(node) primaryNode;
                primaryNode := operandNode( primaryNodeType, tkIntegerValue );
                readToken;
                primaryNode
            end simplePrimary ;

            reference(node) exprNode;

            if precedence < PRIMARY_PREC  then begin
                exprNode := parseExpr( precedence + 1 );
                while tkPrec( tkType ) = precedence do begin
                    integer op;
                    op := tkNode( tkType );
                    exprNode := opNode( op, exprNode, nextAndParseExpr( precedence + 1 ) )
                end while_op_at_this_precedence_level
                end
            else if tkType = tIdentifier  then exprNode := simplePrimary( nIdentifier )
            else if tkType = tInteger     then exprNode := simplePrimary( nInteger    )
            else if tkType = nString      then begin
                synError( tkLine, tkColumn, "Unexpected string literal." );
                exprNode := simplePrimary( nInteger )
                end
            else if tkType = tLeftParen   then exprNode := parseParenExpr
            else if tkType = tOp_add      then exprNode := nextAndParseExpr( precedence )
            else if tkType = tOp_subtract then exprNode := opNode( nNegate, nextAndParseExpr( precedence ), null )
            else if tkType = tOp_not      then exprNode := opNode( nNot,    nextAndParseExpr( precedence ), null )
            else begin
                synError( tkLine, tkColumn, "Syntax error in expression." );
                exprNode := simplePrimary( nInteger )
            end;
            exprNode
        end parseExpr ;

        % parses a preenthesised expression %
        reference(node) procedure parseParenExpr ; begin
            reference(node) exprNode;
            mustBeLeftParen;
            exprNode := parseExpr( 0 );
            mustBeRightParen;
            exprNode
        end parseParenExpr ;

        % parse statement depending on it's first token %
        if      tkType = tIdentifier then begin % assignment statement %
            stmtExpr := operandNode( nIdentifier, tkIntegerValue );
            % skip the identifier and check for "=" %
            readToken;
            if not have( tOp_Assign ) then synError( tkLine, tkColumn, "Expected ""="" in assignment statement." );
            stmtNode := opNode( nAssign, stmtExpr, parseExpr( 0 ) );
            mustBeEndOfStatement
            end
        else if have( tKeyword_while ) then begin
            stmtExpr := parseParenExpr;
            stmtNode := opNode( nWhile, stmtExpr, parseStatement )
            end
        else if have( tkeyword_if ) then begin
            stmtExpr := parseParenExpr;
            stmtNode := opNode( nIf, stmtExpr, opNode( nIf, parseStatement, null ) );
            if have( tKeyword_else ) then % have an "else" part % right(right(stmtNode)) := parseStatement
            end
        else if have( tKeyword_Print ) then begin
            mustBeLeftParen;
            stmtNode := null;
            while begin
                if tkType = tString then begin
                    stmtNode  := opNode( nSequence, stmtNode, opNode( nPrts, operandNode( nString, tkIntegerValue ), null ) );
                    readToken
                    end
                else stmtNode := opNode( nSequence, stmtNode, opNode( nPrti, parseExpr( 0 ), null ) );
                have( tComma )
            end do begin end;
            mustBeRightparen;
            mustBeEndOfStatement;
            end
        else if have( tKeyword_Putc ) then begin
            stmtNode := opNode( nPrtc, parseParenExpr, null );
            mustBeEndOfStatement
            end
        else if have( tLeftBrace ) then begin % block %
            stmtNode := parseStatementList( tRightBrace );
            if not have( tRightBrace ) then synError( tkLine, tkColumn, "Expected ""}""." );
            end
        else if have( tSemicolon ) then stmtNode := null
        else begin % unrecognised statement %
            skipStatement( "Unrecognised statement." );
            stmtNode := null
        end if_various_tokens ;
        stmtNode
    end parseStatement ;

    % parses a statement list ending with the specified terminator %
    reference(node) procedure parseStatementList ( integer value terminator ) ; begin
        reference(node) listNode;
        listNode := null;
        while tkType not = terminator and tkType not = tEnd_of_input do listNode := opNode( nSequence, listNode, parseStatement );
        listNode
    end parseStatementList ;

    nIdentifier      :=  1; ndName( nIdentifier      ) := "Identifier";   nString    :=  2; ndName( nString   ) := "String";
    nInteger         :=  3; ndName( nInteger         ) := "Integer";      nSequence  :=  4; ndName( nSequence ) := "Sequence";
    nIf              :=  5; ndName( nIf              ) := "If";           nPrtc      :=  6; ndName( nPrtc     ) := "Prtc";
    nPrts            :=  7; ndName( nPrts            ) := "Prts";         nPrti      :=  8; ndName( nPrti     ) := "Prti";
    nWhile           :=  9; ndName( nWhile           ) := "While";        nAssign    := 10; ndName( nAssign   ) := "Assign";
    nNegate          := 11; ndName( nNegate          ) := "Negate";       nNot       := 12; ndName( nNot      ) := "Not";
    nMultiply        := 13; ndName( nMultiply        ) := "Multiply";     nDivide    := 14; ndName( nDivide   ) := "Divide";
    nMod             := 15; ndName( nMod             ) := "Mod";          nAdd       := 16; ndName( nAdd      ) := "Add";
    nSubtract        := 17; ndName( nSubtract        ) := "Subtract";     nLess      := 18; ndName( nLess     ) := "Less";
    nLessEqual       := 19; ndName( nLessEqual       ) := "LessEqual"  ;  nGreater   := 20; ndName( nGreater  ) := "Greater";
    nGreaterEqual    := 21; ndName( nGreaterEqual    ) := "GreaterEqual"; nEqual     := 22; ndName( nEqual    ) := "Equal";
    nNotEqual        := 23; ndName( nNotEqual        ) := "NotEqual";     nAnd       := 24; ndName( nAnd      ) := "And";
    nOr              := 25; ndName( nOr              ) := "Or";
    tOp_multiply     :=  1; tkName( tOp_multiply     ) := "Op_multiply";     tkPrec( tOp_multiply     ) :=  5;
    tOp_divide       :=  2; tkName( tOp_divide       ) := "Op_divide";       tkPrec( tOp_divide       ) :=  5;
    tOp_mod          :=  3; tkName( tOp_mod          ) := "Op_mod";          tkPrec( tOp_mod          ) :=  5;
    tOp_add          :=  4; tkName( tOp_add          ) := "Op_add";          tkPrec( tOp_add          ) :=  4;
    tOp_subtract     :=  5; tkName( tOp_subtract     ) := "Op_subtract";     tkPrec( tOp_subtract     ) :=  4;
    tOp_negate       :=  6; tkName( tOp_negate       ) := "Op_negate";       tkPrec( tOp_negate       ) := -1;
    tOp_less         :=  7; tkName( tOp_less         ) := "Op_less";         tkPrec( tOp_less         ) :=  3;
    tOp_lessequal    :=  8; tkName( tOp_lessequal    ) := "Op_lessequal";    tkPrec( tOp_lessequal    ) :=  3;
    tOp_greater      :=  9; tkName( tOp_greater      ) := "Op_greater";      tkPrec( tOp_greater      ) :=  3;
    tOp_greaterequal := 10; tkName( tOp_greaterequal ) := "Op_greaterequal"; tkPrec( tOp_greaterequal ) :=  3;
    tOp_equal        := 11; tkName( tOp_equal        ) := "Op_equal";        tkPrec( tOp_equal        ) :=  2;
    tOp_notequal     := 12; tkName( tOp_notequal     ) := "Op_notequal";     tkPrec( tOp_notequal     ) :=  2;
    tOp_not          := 13; tkName( tOp_not          ) := "Op_not";          tkPrec( tOp_not          ) := -1;
    tOp_assign       := 14; tkName( tOp_assign       ) := "Op_assign";       tkPrec( tOp_assign       ) := -1;
    tOp_and          := 15; tkName( tOp_and          ) := "Op_and";          tkPrec( tOp_and          ) :=  1;
    tOp_or           := 16; tkName( tOp_or           ) := "Op_or";           tkPrec( tOp_or           ) :=  0;
    tLeftParen       := 17; tkName( tLeftParen       ) := "LeftParen";       tkPrec( tLeftParen       ) := -1;
    tRightParen      := 18; tkName( tRightParen      ) := "RightParen";      tkPrec( tRightParen      ) := -1;
    tLeftBrace       := 19; tkName( tLeftBrace       ) := "LeftBrace";       tkPrec( tLeftBrace       ) := -1;
    tRightBrace      := 20; tkName( tRightBrace      ) := "RightBrace";      tkPrec( tRightBrace      ) := -1;
    tSemicolon       := 21; tkName( tSemicolon       ) := "Semicolon";       tkPrec( tSemicolon       ) := -1;
    tComma           := 22; tkName( tComma           ) := "Comma";           tkPrec( tComma           ) := -1;
    tKeyword_if      := 23; tkName( tKeyword_if      ) := "Keyword_if";      tkPrec( tKeyword_if      ) := -1;
    tKeyword_else    := 24; tkName( tKeyword_else    ) := "Keyword_else";    tkPrec( tKeyword_else    ) := -1;
    tKeyword_while   := 25; tkName( tKeyword_while   ) := "Keyword_while";   tkPrec( tKeyword_while   ) := -1;
    tKeyword_print   := 26; tkName( tKeyword_print   ) := "Keyword_print";   tkPrec( tKeyword_print   ) := -1;
    tKeyword_putc    := 27; tkName( tKeyword_putc    ) := "Keyword_putc";    tkPrec( tKeyword_putc    ) := -1;
    tIdentifier      := 28; tkName( tIdentifier      ) := "Identifier";      tkPrec( tIdentifier      ) := -1;
    tInteger         := 29; tkName( tInteger         ) := "Integer";         tkPrec( tInteger         ) := -1;
    tString          := 30; tkName( tString          ) := "String";          tkPrec( tString          ) := -1;
    tEnd_of_input    := 31; tkName( tEnd_of_input    ) := "End_of_input";    tkPrec( tEnd_of_input    ) := -1;
    MAX_TOKEN_TYPE   := 31; TEXT_MAX := 4095; textNext := 0; PRIMARY_PREC := 6;
    for tkPos := 1 until MAX_TOKEN_TYPE do tkNode( tkPos ) := - tkPos;
    tkNode( tOp_multiply     ) := nMultiply;  tkNode( tOp_divide   ) := nDivide;   tkNode( tOp_mod          ) := nMod;
    tkNode( tOp_add          ) := nAdd;       tkNode( tOp_subtract ) := nSubtract; tkNode( tOp_less         ) := nLess;
    tkNode( tOp_lessequal    ) := nLessEqual; tkNode( tOp_greater  ) := nGreater;  tkNode( tOp_greaterequal ) := nGreaterEqual;
    tkNode( tOp_equal        ) := nEqual;     tkNode( tOp_notequal ) := nNotEqual; tkNode( tOp_not          ) := nNot;
    tkNode( tOp_and          ) := nAnd;       tkNode( tOp_or       ) := nOr;
    stList := idList := null;

    % parse the output from the lexical analyser and output the linearised parse tree %
    readToken;
    writeNode( parseStatementList( tEnd_of_input ) )
end.
```

Output from parsing the Prime Numbers example program.

```txt

Sequence
Sequence
Sequence
Sequence
Sequence
;
Assign
Identifier    count
Integer                    1
Assign
Identifier    n
Integer                    1
Assign
Identifier    limit
Integer                  100
While
Less
Identifier    n
Identifier    limit
Sequence
Sequence
Sequence
Sequence
Sequence
;
Assign
Identifier    k
Integer                    3
Assign
Identifier    p
Integer                    1
Assign
Identifier    n
Add
Identifier    n
Integer                    2
While
And
LessEqual
Multiply
Identifier    k
Identifier    k
Identifier    n
Identifier    p
Sequence
Sequence
;
Assign
Identifier    p
NotEqual
Multiply
Divide
Identifier    n
Identifier    k
Identifier    k
Identifier    n
Assign
Identifier    k
Add
Identifier    k
Integer                    2
If
Identifier    p
If
Sequence
Sequence
;
Sequence
Sequence
;
Prti
Identifier    n
;
Prts
String        " is prime\n"
;
Assign
Identifier    count
Add
Identifier    count
Integer                    1
;
Sequence
Sequence
Sequence
;
Prts
String        "Total primes found: "
;
Prti
Identifier    count
;
Prts
String        "\n"
;

```



## AWK

Tested with gawk 4.1.1 and mawk 1.3.4.

```AWK

function Token_assign(tk, attr,      attr_array, n, i) {
  n=split(attr, attr_array)
  for(i=1; i<=n; i++)
    Tokens[tk,i-1] = attr_array[i]
}

#*** show error and exit
function error(msg) {
  printf("(%s, %s) %s\n", err_line, err_col, msg)
  exit(1)
}

function gettok(      line, n, i) {
  getline line
  if (line == "")
    error("empty line")
  n=split(line, line_list)
  # line col Ident var_name
  # 1    2   3     4
  err_line = line_list[1]
  err_col  = line_list[2]
  tok_text = line_list[3]
  tok = all_syms[tok_text]
  for (i=5; i<=n; i++)
    line_list[4] = line_list[4] " " line_list[i]
  if (tok == "")
    error("Unknown token " tok_text)
  tok_other = ""
  if (tok == "tk_Integer" || tok == "tk_Ident" || tok =="tk_String")
    tok_other = line_list[4]
}

function make_node(oper, left, right, value) {
  node_type [next_free_node_index] = oper
  node_left [next_free_node_index] = left
  node_right[next_free_node_index] = right
  node_value[next_free_node_index] = value
  return next_free_node_index ++
}

function make_leaf(oper, n) {
  return make_node(oper, 0, 0, n)
}

function expect(msg, s) {
  if (tok == s) {
    gettok()
    return
  }
  error(msg ": Expecting '" Tokens[s,TK_NAME] "', found '" Tokens[tok,TK_NAME] "'")
}

function expr(p,       x, op, node) {
  x = 0
  if (tok == "tk_Lparen") {
    x = paren_expr()
  } else if (tok == "tk_Sub" || tok == "tk_Add") {
    if (tok == "tk_Sub")
      op = "tk_Negate"
    else
      op = "tk_Add"
    gettok()
    node = expr(Tokens["tk_Negate",TK_PRECEDENCE]+0)
    if (op == "tk_Negate")
      x = make_node("nd_Negate", node)
    else
      x = node
  } else if (tok == "tk_Not") {
    gettok()
    x = make_node("nd_Not", expr(Tokens["tk_Not",TK_PRECEDENCE]+0))
  } else if (tok == "tk_Ident") {
    x = make_leaf("nd_Ident", tok_other)
    gettok()
  } else if (tok == "tk_Integer") {
    x = make_leaf("nd_Integer", tok_other)
    gettok()
  } else {
    error("Expecting a primary, found: " Tokens[tok,TK_NAME])
  }
  while (((Tokens[tok,TK_IS_BINARY]+0) > 0) && ((Tokens[tok,TK_PRECEDENCE]+0) >= p)) {
    op = tok
    gettok()
    q = Tokens[op,TK_PRECEDENCE]+0
    if (! (Tokens[op,TK_RIGHT_ASSOC]+0 > 0))
      q += 1
    node = expr(q)
    x = make_node(Tokens[op,TK_NODE], x, node)
  }
  return x
}

function paren_expr(        node) {
  expect("paren_expr", "tk_Lparen")
  node = expr(0)
  expect("paren_expr", "tk_Rparen")
  return node
}

function stmt(              t, e, s, s2, v) {
  t = 0
  if (tok == "tk_If") {
    gettok()
    e = paren_expr()
    s = stmt()
    s2 = 0
    if (tok == "tk_Else") {
      gettok()
      s2 = stmt()
    }
    t = make_node("nd_If", e, make_node("nd_If", s, s2))
  } else if (tok == "tk_Putc") {
    gettok()
    e = paren_expr()
    t = make_node("nd_Prtc", e)
    expect("Putc", "tk_Semi")
  } else if (tok == "tk_Print") {
    gettok()
    expect("Print", "tk_Lparen")
    while (1) {
      if (tok == "tk_String") {
        e = make_node("nd_Prts", make_leaf("nd_String", tok_other))
        gettok()
      } else {
        e = make_node("nd_Prti", expr(0))
      }
      t = make_node("nd_Sequence", t, e)
      if (tok != "tk_Comma")
        break
      gettok()
    }
    expect("Print", "tk_Rparen")
    expect("Print", "tk_Semi")
  } else if (tok == "tk_Semi") {
    gettok()
  } else if (tok == "tk_Ident") {
    v = make_leaf("nd_Ident", tok_other)
    gettok()
    expect("assign", "tk_Assign")
    e = expr(0)
    t = make_node("nd_Assign", v, e)
    expect("assign", "tk_Semi")
  } else if (tok == "tk_While") {
    gettok()
    e = paren_expr()
    s = stmt()
    t = make_node("nd_While", e, s)
  } else if (tok == "tk_Lbrace") {
    gettok()
    while (tok != "tk_Rbrace" && tok != "tk_EOI")
      t = make_node("nd_Sequence", t, stmt())
    expect("Lbrace", "tk_Rbrace")
  } else if (tok == "tk_EOI") {
  } else {
    error("Expecting start of statement, found: " Tokens[tok,TK_NAME])
  }
  return t
}

function parse(         t) {
  t = 0   # None
  gettok()
  while (1) {
    t = make_node("nd_Sequence", t, stmt())
    if (tok == "tk_EOI" || t == 0)
      break
  }
  return t
}

function prt_ast(t) {
  if (t == 0) {
    print(";")
  } else {
    printf("%-14s", Display_nodes[node_type[t]])
    if ((node_type[t] == "nd_Ident") || (node_type[t] == "nd_Integer"))
      printf("%s\n", node_value[t])
    else if (node_type[t] == "nd_String") {
      printf("%s\n", node_value[t])
    } else {
      print("")
      prt_ast(node_left[t])
      prt_ast(node_right[t])
    }
  }
}

BEGIN {
  all_syms["End_of_input"    ] = "tk_EOI"
  all_syms["Op_multiply"     ] = "tk_Mul"
  all_syms["Op_divide"       ] = "tk_Div"
  all_syms["Op_mod"          ] = "tk_Mod"
  all_syms["Op_add"          ] = "tk_Add"
  all_syms["Op_subtract"     ] = "tk_Sub"
  all_syms["Op_negate"       ] = "tk_Negate"
  all_syms["Op_not"          ] = "tk_Not"
  all_syms["Op_less"         ] = "tk_Lss"
  all_syms["Op_lessequal"    ] = "tk_Leq"
  all_syms["Op_greater"      ] = "tk_Gtr"
  all_syms["Op_greaterequal" ] = "tk_Geq"
  all_syms["Op_equal"        ] = "tk_Eq"
  all_syms["Op_notequal"     ] = "tk_Neq"
  all_syms["Op_assign"       ] = "tk_Assign"
  all_syms["Op_and"          ] = "tk_And"
  all_syms["Op_or"           ] = "tk_Or"
  all_syms["Keyword_if"      ] = "tk_If"
  all_syms["Keyword_else"    ] = "tk_Else"
  all_syms["Keyword_while"   ] = "tk_While"
  all_syms["Keyword_print"   ] = "tk_Print"
  all_syms["Keyword_putc"    ] = "tk_Putc"
  all_syms["LeftParen"       ] = "tk_Lparen"
  all_syms["RightParen"      ] = "tk_Rparen"
  all_syms["LeftBrace"       ] = "tk_Lbrace"
  all_syms["RightBrace"      ] = "tk_Rbrace"
  all_syms["Semicolon"       ] = "tk_Semi"
  all_syms["Comma"           ] = "tk_Comma"
  all_syms["Identifier"      ] = "tk_Ident"
  all_syms["Integer"         ] = "tk_Integer"
  all_syms["String"          ] = "tk_String"

  Display_nodes["nd_Ident"   ] = "Identifier"
  Display_nodes["nd_String"  ] = "String"
  Display_nodes["nd_Integer" ] = "Integer"
  Display_nodes["nd_Sequence"] = "Sequence"
  Display_nodes["nd_If"      ] = "If"
  Display_nodes["nd_Prtc"    ] = "Prtc"
  Display_nodes["nd_Prts"    ] = "Prts"
  Display_nodes["nd_Prti"    ] = "Prti"
  Display_nodes["nd_While"   ] = "While"
  Display_nodes["nd_Assign"  ] = "Assign"
  Display_nodes["nd_Negate"  ] = "Negate"
  Display_nodes["nd_Not"     ] = "Not"
  Display_nodes["nd_Mul"     ] = "Multiply"
  Display_nodes["nd_Div"     ] = "Divide"
  Display_nodes["nd_Mod"     ] = "Mod"
  Display_nodes["nd_Add"     ] = "Add"
  Display_nodes["nd_Sub"     ] = "Subtract"
  Display_nodes["nd_Lss"     ] = "Less"
  Display_nodes["nd_Leq"     ] = "LessEqual"
  Display_nodes["nd_Gtr"     ] = "Greater"
  Display_nodes["nd_Geq"     ] = "GreaterEqual"
  Display_nodes["nd_Eql"     ] = "Equal"
  Display_nodes["nd_Neq"     ] = "NotEqual"
  Display_nodes["nd_And"     ] = "And"
  Display_nodes["nd_Or"      ] = "Or"

  TK_NAME         =          0
  TK_RIGHT_ASSOC  =                   1
  TK_IS_BINARY    =                     2
  TK_IS_UNARY     =                       3
  TK_PRECEDENCE   =                          4
  TK_NODE         =                             5
  Token_assign("tk_EOI"    , "EOI     0 0 0 -1 -1        ")
  Token_assign("tk_Mul"    , "*       0 1 0 13 nd_Mul    ")
  Token_assign("tk_Div"    , "/       0 1 0 13 nd_Div    ")
  Token_assign("tk_Mod"    , "%       0 1 0 13 nd_Mod    ")
  Token_assign("tk_Add"    , "+       0 1 0 12 nd_Add    ")
  Token_assign("tk_Sub"    , "-       0 1 0 12 nd_Sub    ")
  Token_assign("tk_Negate" , "-       0 0 1 14 nd_Negate ")
  Token_assign("tk_Not"    , "!       0 0 1 14 nd_Not    ")
  Token_assign("tk_Lss"    , "<       0 1 0 10 nd_Lss    ")
  Token_assign("tk_Leq"    , "<=      0 1 0 10 nd_Leq    ")
  Token_assign("tk_Gtr"    , ">       0 1 0 10 nd_Gtr    ")
  Token_assign("tk_Geq"    , ">=      0 1 0 10 nd_Geq    ")
  Token_assign("tk_Eql"    , "==      0 1 0  9 nd_Eql    ")
  Token_assign("tk_Neq"    , "!=      0 1 0  9 nd_Neq    ")
  Token_assign("tk_Assign" , "=       0 0 0 -1 nd_Assign ")
  Token_assign("tk_And"    , "&&      0 1 0  5 nd_And    ")
  Token_assign("tk_Or"     , "||      0 1 0  4 nd_Or     ")
  Token_assign("tk_If"     , "if      0 0 0 -1 nd_If     ")
  Token_assign("tk_Else"   , "else    0 0 0 -1 -1        ")
  Token_assign("tk_While"  , "while   0 0 0 -1 nd_While  ")
  Token_assign("tk_Print"  , "print   0 0 0 -1 -1        ")
  Token_assign("tk_Putc"   , "putc    0 0 0 -1 -1        ")
  Token_assign("tk_Lparen" , "(       0 0 0 -1 -1        ")
  Token_assign("tk_Rparen" , ")       0 0 0 -1 -1        ")
  Token_assign("tk_Lbrace" , "{       0 0 0 -1 -1        ")
  Token_assign("tk_Rbrace" , "}       0 0 0 -1 -1        ")
  Token_assign("tk_Semi"   , ";       0 0 0 -1 -1        ")
  Token_assign("tk_Comma"  , ",       0 0 0 -1 -1        ")
  Token_assign("tk_Ident"  , "Ident   0 0 0 -1 nd_Ident  ")
  Token_assign("tk_Integer", "Integer 0 0 0 -1 nd_Integer")
  Token_assign("tk_String" , "String  0 0 0 -1 nd_String ")

  input_file = "-"
  err_line   = 0
  err_col    = 0
  tok        = ""
  tok_text   = ""
  next_free_node_index = 1

  if (ARGC > 1)
    input_file = ARGV[1]
  t = parse()
  prt_ast(t)
}

```


<b>

```txt

Sequence
Sequence
;
Assign
Identifier    count
Integer       1
While
Less
Identifier    count
Integer       10
Sequence
Sequence
;
Sequence
Sequence
Sequence
;
Prts
String        "count is: "
;
Prti
Identifier    count
;
Prts
String        "\n"
;
Assign
Identifier    count
Add
Identifier    count
Integer       1

```

</b>


## C

Tested with gcc 4.81 and later, compiles warning free with -Wall -Wextra

```c
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include <stdbool.h>
#include <ctype.h>

#define NELEMS(arr) (sizeof(arr) / sizeof(arr[0]))

typedef enum {
    tk_EOI, tk_Mul, tk_Div, tk_Mod, tk_Add, tk_Sub, tk_Negate, tk_Not, tk_Lss, tk_Leq, tk_Gtr,
    tk_Geq, tk_Eql, tk_Neq, tk_Assign, tk_And, tk_Or, tk_If, tk_Else, tk_While, tk_Print,
    tk_Putc, tk_Lparen, tk_Rparen, tk_Lbrace, tk_Rbrace, tk_Semi, tk_Comma, tk_Ident,
    tk_Integer, tk_String
} TokenType;

typedef enum {
    nd_Ident, nd_String, nd_Integer, nd_Sequence, nd_If, nd_Prtc, nd_Prts, nd_Prti, nd_While,
    nd_Assign, nd_Negate, nd_Not, nd_Mul, nd_Div, nd_Mod, nd_Add, nd_Sub, nd_Lss, nd_Leq,
    nd_Gtr, nd_Geq, nd_Eql, nd_Neq, nd_And, nd_Or
} NodeType;

typedef struct {
    TokenType tok;
    int err_ln;
    int err_col;
    char *text;             /* ident or string literal or integer value */
} tok_s;

typedef struct Tree {
    NodeType node_type;
    struct Tree *left;
    struct Tree *right;
    char *value;
} Tree;

// dependency: Ordered by tok, must remain in same order as TokenType enum
struct {
    char       *text, *enum_text;
    TokenType   tok;
    bool        right_associative, is_binary, is_unary;
    int         precedence;
    NodeType    node_type;
} atr[] = {
    {"EOI",             "End_of_input"   , tk_EOI,     false, false, false, -1, -1        },
    {"*",               "Op_multiply"    , tk_Mul,     false, true,  false, 13, nd_Mul    },
    {"/",               "Op_divide"      , tk_Div,     false, true,  false, 13, nd_Div    },
    {"%",               "Op_mod"         , tk_Mod,     false, true,  false, 13, nd_Mod    },
    {"+",               "Op_add"         , tk_Add,     false, true,  false, 12, nd_Add    },
    {"-",               "Op_subtract"    , tk_Sub,     false, true,  false, 12, nd_Sub    },
    {"-",               "Op_negate"      , tk_Negate,  false, false, true,  14, nd_Negate },
    {"!",               "Op_not"         , tk_Not,     false, false, true,  14, nd_Not    },
    {"<",               "Op_less"        , tk_Lss,     false, true,  false, 10, nd_Lss    },
    {"<=",              "Op_lessequal"   , tk_Leq,     false, true,  false, 10, nd_Leq    },
    {">",               "Op_greater"     , tk_Gtr,     false, true,  false, 10, nd_Gtr    },
    {">=",              "Op_greaterequal", tk_Geq,     false, true,  false, 10, nd_Geq    },
    {"==",              "Op_equal"       , tk_Eql,     false, true,  false,  9, nd_Eql    },
    {"!=",              "Op_notequal"    , tk_Neq,     false, true,  false,  9, nd_Neq    },
    {"=",               "Op_assign"      , tk_Assign,  false, false, false, -1, nd_Assign },
    {"&&",              "Op_and"         , tk_And,     false, true,  false,  5, nd_And    },
    {"||",              "Op_or"          , tk_Or,      false, true,  false,  4, nd_Or     },
    {"if",              "Keyword_if"     , tk_If,      false, false, false, -1, nd_If     },
    {"else",            "Keyword_else"   , tk_Else,    false, false, false, -1, -1        },
    {"while",           "Keyword_while"  , tk_While,   false, false, false, -1, nd_While  },
    {"print",           "Keyword_print"  , tk_Print,   false, false, false, -1, -1        },
    {"putc",            "Keyword_putc"   , tk_Putc,    false, false, false, -1, -1        },
    {"(",               "LeftParen"      , tk_Lparen,  false, false, false, -1, -1        },
    {")",               "RightParen"     , tk_Rparen,  false, false, false, -1, -1        },
    {"{",               "LeftBrace"      , tk_Lbrace,  false, false, false, -1, -1        },
    {"}",               "RightBrace"     , tk_Rbrace,  false, false, false, -1, -1        },
    {";",               "Semicolon"      , tk_Semi,    false, false, false, -1, -1        },
    {",",               "Comma"          , tk_Comma,   false, false, false, -1, -1        },
    {"Ident",           "Identifier"     , tk_Ident,   false, false, false, -1, nd_Ident  },
    {"Integer literal", "Integer"        , tk_Integer, false, false, false, -1, nd_Integer},
    {"String literal",  "String"         , tk_String,  false, false, false, -1, nd_String },
};

char *Display_nodes[] = {"Identifier", "String", "Integer", "Sequence", "If", "Prtc",
    "Prts", "Prti", "While", "Assign", "Negate", "Not", "Multiply", "Divide", "Mod",
    "Add", "Subtract", "Less", "LessEqual", "Greater", "GreaterEqual", "Equal",
    "NotEqual", "And", "Or"};

static tok_s tok;
static FILE *source_fp, *dest_fp;

Tree *paren_expr();

void error(int err_line, int err_col, const char *fmt, ... ) {
    va_list ap;
    char buf[1000];

    va_start(ap, fmt);
    vsprintf(buf, fmt, ap);
    va_end(ap);
    printf("(%d, %d) error: %s\n", err_line, err_col, buf);
    exit(1);
}

char *read_line(int *len) {
    static char *text = NULL;
    static int textmax = 0;

    for (*len = 0; ; (*len)++) {
        int ch = fgetc(source_fp);
        if (ch == EOF || ch == '\n') {
            if (*len == 0)
                return NULL;
            break;
        }
        if (*len + 1 >= textmax) {
            textmax = (textmax == 0 ? 128 : textmax * 2);
            text = realloc(text, textmax);
        }
        text[*len] = ch;
    }
    text[*len] = '\0';
    return text;
}

char *rtrim(char *text, int *len) {         // remove trailing spaces
    for (; *len > 0 && isspace(text[*len - 1]); --(*len))
        ;

    text[*len] = '\0';
    return text;
}

TokenType get_enum(const char *name) {      // return internal version of name
    for (size_t i = 0; i < NELEMS(atr); i++) {
        if (strcmp(atr[i].enum_text, name) == 0)
            return atr[i].tok;
    }
    error(0, 0, "Unknown token %s\n", name);
    return 0;
}

tok_s gettok() {
    int len;
    tok_s tok;
    char *yytext = read_line(&len);
    yytext = rtrim(yytext, &len);

    // [ ]*{lineno}[ ]+{colno}[ ]+token[ ]+optional

    // get line and column
    tok.err_ln  = atoi(strtok(yytext, " "));
    tok.err_col = atoi(strtok(NULL, " "));

    // get the token name
    char *name = strtok(NULL, " ");
    tok.tok = get_enum(name);

    // if there is extra data, get it
    char *p = name + strlen(name);
    if (p != &yytext[len]) {
        for (++p; isspace(*p); ++p)
            ;
        tok.text = strdup(p);
    }
    return tok;
}

Tree *make_node(NodeType node_type, Tree *left, Tree *right) {
    Tree *t = calloc(sizeof(Tree), 1);
    t->node_type = node_type;
    t->left = left;
    t->right = right;
    return t;
}

Tree *make_leaf(NodeType node_type, char *value) {
    Tree *t = calloc(sizeof(Tree), 1);
    t->node_type = node_type;
    t->value = strdup(value);
    return t;
}

void expect(const char msg[], TokenType s) {
    if (tok.tok == s) {
        tok = gettok();
        return;
    }
    error(tok.err_ln, tok.err_col, "%s: Expecting '%s', found '%s'\n", msg, atr[s].text, atr[tok.tok].text);
}

Tree *expr(int p) {
    Tree *x = NULL, *node;
    TokenType op;

    switch (tok.tok) {
        case tk_Lparen:
            x = paren_expr();
            break;
        case tk_Sub: case tk_Add:
            op = tok.tok;
            tok = gettok();
            node = expr(atr[tk_Negate].precedence);
            x = (op == tk_Sub) ? make_node(nd_Negate, node, NULL) : node;
            break;
        case tk_Not:
            tok = gettok();
            x = make_node(nd_Not, expr(atr[tk_Not].precedence), NULL);
            break;
        case tk_Ident:
            x = make_leaf(nd_Ident, tok.text);
            tok = gettok();
            break;
        case tk_Integer:
            x = make_leaf(nd_Integer, tok.text);
            tok = gettok();
            break;
        default:
            error(tok.err_ln, tok.err_col, "Expecting a primary, found: %s\n", atr[tok.tok].text);
    }

    while (atr[tok.tok].is_binary && atr[tok.tok].precedence >= p) {
        TokenType op = tok.tok;

        tok = gettok();

        int q = atr[op].precedence;
        if (!atr[op].right_associative)
            q++;

        node = expr(q);
        x = make_node(atr[op].node_type, x, node);
    }
    return x;
}

Tree *paren_expr() {
    expect("paren_expr", tk_Lparen);
    Tree *t = expr(0);
    expect("paren_expr", tk_Rparen);
    return t;
}

Tree *stmt() {
    Tree *t = NULL, *v, *e, *s, *s2;

    switch (tok.tok) {
        case tk_If:
            tok = gettok();
            e = paren_expr();
            s = stmt();
            s2 = NULL;
            if (tok.tok == tk_Else) {
                tok = gettok();
                s2 = stmt();
            }
            t = make_node(nd_If, e, make_node(nd_If, s, s2));
            break;
        case tk_Putc:
            tok = gettok();
            e = paren_expr();
            t = make_node(nd_Prtc, e, NULL);
            expect("Putc", tk_Semi);
            break;
        case tk_Print: /* print '(' expr {',' expr} ')' */
            tok = gettok();
            for (expect("Print", tk_Lparen); ; expect("Print", tk_Comma)) {
                if (tok.tok == tk_String) {
                    e = make_node(nd_Prts, make_leaf(nd_String, tok.text), NULL);
                    tok = gettok();
                } else
                    e = make_node(nd_Prti, expr(0), NULL);

                t = make_node(nd_Sequence, t, e);

                if (tok.tok != tk_Comma)
                    break;
            }
            expect("Print", tk_Rparen);
            expect("Print", tk_Semi);
            break;
        case tk_Semi:
            tok = gettok();
            break;
        case tk_Ident:
            v = make_leaf(nd_Ident, tok.text);
            tok = gettok();
            expect("assign", tk_Assign);
            e = expr(0);
            t = make_node(nd_Assign, v, e);
            expect("assign", tk_Semi);
            break;
        case tk_While:
            tok = gettok();
            e = paren_expr();
            s = stmt();
            t = make_node(nd_While, e, s);
            break;
        case tk_Lbrace:         /* {stmt} */
            for (expect("Lbrace", tk_Lbrace); tok.tok != tk_Rbrace && tok.tok != tk_EOI;)
                t = make_node(nd_Sequence, t, stmt());
            expect("Lbrace", tk_Rbrace);
            break;
        case tk_EOI:
            break;
        default: error(tok.err_ln, tok.err_col, "expecting start of statement, found '%s'\n", atr[tok.tok].text);
    }
    return t;
}

Tree *parse() {
    Tree *t = NULL;

    tok = gettok();
    do {
        t = make_node(nd_Sequence, t, stmt());
    } while (t != NULL && tok.tok != tk_EOI);
    return t;
}

void prt_ast(Tree *t) {
    if (t == NULL)
        printf(";\n");
    else {
        printf("%-14s ", Display_nodes[t->node_type]);
        if (t->node_type == nd_Ident || t->node_type == nd_Integer || t->node_type == nd_String) {
            printf("%s\n", t->value);
        } else {
            printf("\n");
            prt_ast(t->left);
            prt_ast(t->right);
        }
    }
}

void init_io(FILE **fp, FILE *std, const char mode[], const char fn[]) {
    if (fn[0] == '\0')
        *fp = std;
    else if ((*fp = fopen(fn, mode)) == NULL)
        error(0, 0, "Can't open %s\n", fn);
}

int main(int argc, char *argv[]) {
    init_io(&source_fp, stdin,  "r",  argc > 1 ? argv[1] : "");
    init_io(&dest_fp,   stdout, "wb", argc > 2 ? argv[2] : "");
    prt_ast(parse());
}
```


<b>

```txt

Sequence
Sequence
Sequence
Sequence
Sequence
;
Assign
Identifier     count
Integer        1
Assign
Identifier     n
Integer        1
Assign
Identifier     limit
Integer        100
While
Less
Identifier     n
Identifier     limit
Sequence
Sequence
Sequence
Sequence
Sequence
;
Assign
Identifier     k
Integer        3
Assign
Identifier     p
Integer        1
Assign
Identifier     n
Add
Identifier     n
Integer        2
While
And
LessEqual
Multiply
Identifier     k
Identifier     k
Identifier     n
Identifier     p
Sequence
Sequence
;
Assign
Identifier     p
NotEqual
Multiply
Divide
Identifier     n
Identifier     k
Identifier     k
Identifier     n
Assign
Identifier     k
Add
Identifier     k
Integer        2
If
Identifier     p
If
Sequence
Sequence
;
Sequence
Sequence
;
Prti
Identifier     n
;
Prts
String         " is prime\n"
;
Assign
Identifier     count
Add
Identifier     count
Integer        1
;
Sequence
Sequence
Sequence
;
Prts
String         "Total primes found: "
;
Prti
Identifier     count
;
Prts
String         "\n"
;

```

</b>


## COBOL

Code by Steve Williams. Tested with GnuCOBOL 2.2.


```cobol
        >
SOURCE FORMAT IS FREE
identification division.
*> this code is dedicated to the public domain
*> (GnuCOBOL) 2.3-dev.0
*> for extra credit, generate this program directly from the EBNF
program-id. parser.
environment division.
configuration section.
repository.  function all intrinsic.
input-output section.
file-control.
    select input-file assign using input-name
        status is input-status
        organization is line sequential.
data division.
file section.
fd  input-file global.
01  input-record global.
    03  input-line pic zzzz9.
    03  input-column pic zzzzzz9.
    03  filler pic x(3).
    03  input-token pic x(16).
    03  input-value pic x(48).

working-storage section.
01  program-name pic x(32) value spaces global.
01  input-name pic x(32) value spaces global.
01  input-status pic xx global.

01  line-no pic 999 value 0.
01  col-no pic 99 value 0.

01  error-record global.
    03  error-line-no pic zzzz9.
    03  error-col-no pic zzzzzz9.
    03  filler pic x value space.
    03  error-message pic x(64) value spaces.

01  token global.
    03  token-type pic x(16).
    03  token-line pic 999.
    03  token-column pic 99.
    03  token-value pic x(48).

01  parse-stack global.
    03  p pic 999 value 0.
    03  p-lim pic 999 value 200.
    03  p-zero pic 999 value 0.
    03  parse-entry occurs 200.
        05  parse-name pic x(24).
        05  parse-token pic x(16).
        05  parse-left pic 999.
        05  parse-right pic 999.
        05  parse-work pic 999.
        05  parse-work1 pic 999.

01  abstract-syntax-tree global.
    03  t pic 999 value 0.
    03  t1 pic 999.
    03  t-lim pic 999 value 998.
    03  filler occurs 998.
        05  leaf.
            07  leaf-type pic x(14).
            07  leaf-value pic x(48).
        05  node redefines leaf.
            07  node-type pic x(14).
            07  node-left pic 999.
            07  node-right pic 999.

01  indent pic x(200) value all '|   ' global.

procedure division chaining program-name.
start-parser.
    if program-name <> spaces
        string program-name delimited by space '.lex' into input-name
        open input input-file
        if input-status <> '00'
            string 'in parser ' trim(input-name) ' open status ' input-status
                into error-message
            call 'reporterror'
        end-if
    end-if
    call 'gettoken'
    call 'stmt_list'
    if input-name <> spaces
        close input-file
    end-if

    call 'printast' using t

    >>d perform dump-ast

    stop run
    .
dump-ast.
    display '
### ====
' upon syserr
    display 'ast:' upon syserr
    display 't=' t upon syserr
    perform varying t1 from 1 by 1 until t1 > t
        if leaf-type(t1) = 'Identifier' or 'Integer' or 'String'
            display t1 space trim(leaf-type(t1)) space trim(leaf-value(t1)) upon syserr
        else
            display t1 space node-left(t1) space node-right(t1) space trim(node-type(t1))
                upon syserr
        end-if
    end-perform
    .

identification division.
program-id. stmt_list common recursive.
data division.
procedure division.
start-stmt_list.
    call 'push' using module-id
    move p-zero to parse-left(p)
    perform forever
        call 'stmt'
        move return-code to parse-right(p)
        call 'makenode' using 'Sequence' parse-left(p) parse-right(p)
        move return-code to parse-left(p)
        if parse-right(p) = 0
        or token-type = 'End_of_input'
            exit perform
        end-if
    end-perform
    call 'pop'
    .
end program stmt_list.

identification division.
program-id. stmt common recursive.
procedure division.
start-stmt.
    call 'push' using module-id
    move p-zero to parse-left(p)
    evaluate token-type
    when 'Semicolon'
        call 'gettoken'
    when 'Identifier'
        *>Identifier '=' expr ';'
        call 'makeleaf' using 'Identifier' token-value
        move return-code to parse-left(p)
        call 'gettoken'
        call 'expect' using 'Op_assign'
        call 'expr'
        move return-code to parse-right(p)
        call 'expect' using 'Semicolon'
        call 'makenode' using 'Assign' parse-left(p) parse-right(p)
        move return-code to parse-left(p)
    when 'Keyword_while'
        *>'while' paren_expr '{' stmt '}'
        call 'gettoken'
        call 'paren_expr'
        move return-code to parse-work(p)
        call 'stmt'
        move return-code to parse-right(p)
        call 'makenode' using 'While' parse-work(p) parse-right(p)
        move return-code to parse-left(p)
    when 'Keyword_if'
        *>'if' paren_expr stmt ['else' stmt]
        call 'gettoken'
        call 'paren_expr'
        move return-code to parse-left(p)
        call 'stmt'
        move return-code to parse-work(p)
        move p-zero to parse-work1(p)
        if token-type = 'Keyword_else'
            call 'gettoken'
            call 'stmt'
            move return-code to parse-work1(p)
        end-if
        call 'makenode' using 'If' parse-work(p) parse-work1(p)
        move return-code to parse-right(p)
        call 'makenode' using 'If' parse-left(p) parse-right(p)
        move return-code to parse-left(p)
    when 'Keyword_print'
        *>'print' '(' prt_list ')' ';'
        call 'gettoken'
        call 'expect' using 'LeftParen'
        call 'prt_list'
        move return-code to parse-left(p)
        call 'expect' using 'RightParen'
        call 'expect' using 'Semicolon'
    when 'Keyword_putc'
        *>'putc' paren_expr ';'
        call 'gettoken'
        call 'paren_expr'
        move return-code to parse-left(p)
        call 'makenode' using 'Prtc' parse-left(p) p-zero
        move return-code to parse-left(p)
        call 'expect' using 'Semicolon'
    when 'LeftBrace'
        *>'{' stmt '}'
        call 'gettoken'
        move p-zero to parse-left(p)
        perform until token-type = 'RightBrace' or 'End_of_input'
            call 'stmt'
            move return-code to parse-right(p)
            call 'makenode' using 'Sequence' parse-left(p) parse-right(p)
            move return-code to parse-left(p)
        end-perform
        if token-type <> 'End_of_input'
            call 'gettoken'
        end-if
    when other
        move 0 to parse-left(p)
    end-evaluate
    move parse-left(p) to return-code
    call 'pop'
    .
end program stmt.

identification division.
program-id. paren_expr common recursive.
procedure division.
start-paren_expr.
    *>'(' expr ')' ;
    call 'push' using module-id
    call 'expect' using 'LeftParen'
    call 'expr'
    call 'expect' using 'RightParen'
    call 'pop'
    .
end program paren_expr.

identification division.
program-id. prt_list common.
procedure division.
start-prt_list.
    *>(string | expr) {',' (String | expr)} ;
    call 'push' using module-id
    move p-zero to parse-work(p)
    perform prt_entry
    perform until token-type <> 'Comma'
        call 'gettoken'
        perform prt_entry
    end-perform
    call 'pop'
    exit program
    .
prt_entry.
    if token-type = 'String'
        call 'makeleaf' using token-type token-value
        move return-code to parse-left(p)
        call 'makenode' using 'Prts' parse-left(p) p-zero
        call 'gettoken'
    else
        call 'expr'
        move return-code to parse-left(p)
        call 'makenode' using 'Prti' parse-left(p) p-zero
    end-if
    move return-code to parse-right(p)
    call 'makenode' using 'Sequence' parse-work(p) parse-right(p)
    move return-code to parse-work(p)
    .
end program prt_list.

identification division.
program-id. expr common recursive.
procedure division.
start-expr.
    *>and_expr {'||' and_expr} ;
    call 'push' using module-id
    call 'and_expr'
    move return-code to parse-left(p)
    perform forever
       if token-type <> 'Op_or'
           exit perform
       end-if
       call 'gettoken'
       call 'and_expr'
       move return-code to parse-right(p)
       call 'makenode' using 'Or' parse-left(p) parse-right(p)
       move return-code to parse-left(p)
    end-perform
    move parse-left(p) to return-code
    call 'pop'
    .
end program expr.

identification division.
program-id. and_expr common recursive.
procedure division.
start-and_expr.
    *>equality_expr {'&&' equality_expr} ;
    call 'push' using module-id
    call 'equality_expr'
    move return-code to parse-left(p)
    perform forever
        if token-type <> 'Op_and'
            exit perform
        end-if
        call 'gettoken'
        call 'equality_expr'
        move return-code to parse-right(p)
        call 'makenode' using 'And' parse-left(p) parse-right(p)
        move return-code to parse-left(p)
    end-perform
    call 'pop'
    .
end program and_expr.

identification division.
program-id. equality_expr common recursive.
procedure division.
start-equality_expr.
    *>relational_expr [('==' | '!=') relational_expr] ;
    call 'push' using module-id
    call 'relational_expr'
    move return-code to parse-left(p)
    evaluate token-type
    when 'Op_equal'
        move 'Equal' to parse-token(p)
    when 'Op_notequal'
        move 'NotEqual' to parse-token(p)
    end-evaluate
    if parse-token(p) <> spaces
        call 'gettoken'
        call 'relational_expr'
        move return-code to parse-right(p)
        call 'makenode' using parse-token(p) parse-left(p) parse-right(p)
        move return-code to parse-left(p)
    end-if
    call 'pop'
    .
end program equality_expr.

identification division.
program-id. relational_expr common recursive.
procedure division.
start-relational_expr.
    *>addition_expr [('<' | '<=' | '>' | '>=') addition_expr] ;
    call 'push' using module-id
    call 'addition_expr'
    move return-code to parse-left(p)
    evaluate token-type
    when 'Op_less'
        move 'Less' to parse-token(p)
    when 'Op_lessequal'
        move 'LessEqual' to parse-token(p)
    when 'Op_greater'
        move 'Greater' to parse-token(p)
    when 'Op_greaterequal'
        move 'GreaterEqual' to parse-token(p)
    end-evaluate
    if parse-token(p) <> spaces
        call 'gettoken'
        call 'addition_expr'
        move return-code to parse-right(p)
        call 'makenode' using parse-token(p) parse-left(p) parse-right(p)
        move return-code to parse-left(p)
    end-if
    call 'pop'
    .
end program relational_expr.

identification division.
program-id. addition_expr common recursive.
procedure division.
start-addition_expr.
    *>multiplication_expr {('+' | '-') multiplication_expr} ;
    call 'push' using module-id
    call 'multiplication_expr'
    move return-code to parse-left(p)
    perform forever
        evaluate token-type
        when 'Op_add'
            move 'Add' to parse-token(p)
        when 'Op_subtract'
            move 'Subtract' to parse-token(p)
        when other
            exit perform
        end-evaluate
        call 'gettoken'
        call 'multiplication_expr'
        move return-code to parse-right(p)
        call 'makenode' using parse-token(p) parse-left(p) parse-right(p)
        move return-code to parse-left(p)
    end-perform
    call 'pop'
    .
end program addition_expr.

identification division.
program-id.  multiplication_expr common recursive.
procedure division.
start-multiplication_expr.
    *>primary {('*' | '/' | '%') primary } ;
    call 'push' using module-id
    call 'primary'
    move return-code to parse-left(p)
    perform forever
        evaluate token-type
        when 'Op_multiply'
            move 'Multiply' to parse-token(p)
        when 'Op_divide'
            move 'Divide' to parse-token(p)
        when 'Op_mod'
            move 'Mod' to parse-token(p)
        when other
            exit perform
        end-evaluate
        call 'gettoken'
        call 'primary'
        move return-code to parse-right(p)
        call 'makenode' using parse-token(p) parse-left(p) parse-right(p)
        move return-code to parse-left(p)
    end-perform
    call 'pop'
    .
end program multiplication_expr.

identification division.
program-id. primary common recursive.
procedure division.
start-primary.
    *>  Identifier
    *>| Integer
    *>| 'LeftParen' expr 'RightParen'
    *>| ('+' | '-' | '!') primary
    *>;
    call 'push' using module-id
    evaluate token-type
    when 'Identifier'
        call 'makeleaf' using 'Identifier' token-value
        call 'gettoken'
    when 'Integer'
        call 'makeleaf' using 'Integer' token-value
        call 'gettoken'
    when 'LeftParen'
        call 'gettoken'
        call 'expr'
        call 'expect' using 'RightParen'
        move t to return-code
    when 'Op_add'
        call 'gettoken'
        call 'primary'
    when 'Op_subtract'
        call 'gettoken'
        call 'primary'
        move return-code to parse-left(p)
        call 'makenode' using 'Negate' parse-left(p) p-zero
    when 'Op_not'
        call 'gettoken'
        call 'primary'
        move return-code to parse-left(p)
        call 'makenode' using 'Not' parse-left(p) p-zero
    when other
        move 0 to return-code
    end-evaluate
    call 'pop'
    .
end program primary.

program-id. reporterror common.
procedure division.
start-reporterror.
report-error.
    move token-line to error-line-no
    move token-column to error-col-no
    display error-record upon syserr
    stop run with error status -1
    .
end program reporterror.

identification division.
program-id. gettoken common.
procedure division.
start-gettoken.
    if program-name = spaces
        move '00' to input-status
        accept input-record on exception move '10' to input-status end-accept
    else
        read input-file
    end-if

    evaluate input-status
    when '00'
        move input-token to token-type
        move input-value to token-value
        move numval(input-line) to token-line
        move numval(input-column) to token-column
        >>d display indent(1:min(4 * p,length(indent))) 'new token: ' token-type upon syserr
    when '10'
        string 'in parser ' trim(input-name) ' unexpected end of input'
            into error-message
        call 'reporterror'
    when other
        string 'in parser ' trim(input-name) ' unexpected input-status ' input-status
            into error-message
        call 'reporterror'
    end-evaluate
    .
end program gettoken.

identification division.
program-id. expect common.
data division.
linkage section.
01  what any length.
procedure division using what.
start-expect.
    if token-type <> what
        string 'in parser expected ' what ' found ' token-type into error-message
        call 'reporterror'
    end-if
    >>d display indent(1:min(4 * p,length(indent))) 'match: ' token-type upon syserr
    call 'gettoken'
    .
end program expect.

identification division.
program-id. push common.
data division.
linkage section.
01  what any length.
procedure division using what.
start-push.
    >>d display indent(1:min(4 * p,length(indent))) 'push ' what upon syserr
    if p >= p-lim
        move 'in parser stack overflow' to error-message
        call 'reporterror'
    end-if
    add 1 to p
    initialize parse-entry(p)
    move what to parse-name(p)
    .
end program push.

identification division.
program-id. pop common.
procedure division.
start-pop.
    if p < 1
        move 'in parser stack underflow' to error-message
        call 'reporterror'
    end-if
    >>d display indent(1:4 * p - 4) 'pop ' parse-name(p) upon syserr
    subtract 1 from p
    .
end program pop.

identification division.
program-id. makenode common.
data division.
linkage section.
01  parm-type any length.
01  parm-left pic 999.
01  parm-right pic 999.
procedure division using parm-type parm-left parm-right.
start-makenode.
    if t >= t-lim
        string 'in parser makenode tree index t exceeds ' t-lim into error-message
        call 'reporterror'
    end-if
    add 1 to t
    move parm-type to node-type(t)
    move parm-left to node-left(t)
    move parm-right to node-right(t)
    move t to return-code
    .
end program makenode.

identification division.
program-id. makeleaf common.
data division.
linkage section.
01  parm-type any length.
01  parm-value pic x(48).
procedure division using parm-type parm-value.
start-makeleaf.
    if t >= t-lim
        string 'in parser makeleaf tree index t exceeds ' t-lim into error-message
        call 'reporterror'
    end-if
    add 1 to t
    move parm-type to leaf-type(t)
    move parm-value to leaf-value(t)
    move t to return-code
    .
end program makeleaf.

identification division.
program-id. printast recursive.
data division.
linkage section.
01  n pic 999.
procedure division using n.
start-printast.
    if n = 0
        display ';'
        exit program
    end-if
    evaluate leaf-type(n)
    when 'Identifier'
    when 'Integer'
    when 'String'
        display leaf-type(n) trim(leaf-value(n))
    when other
        display node-type(n)
        call 'printast' using node-left(n)
        call 'printast' using node-right(n)
    end-evaluate
    .
end program printast.
end program parser.
```


```txt
prompt$ ./lexer <testcases/Primes | ./parser
Sequence
Sequence
Sequence
Sequence
Sequence
;
Assign
Identifier    count
Integer       1
Assign
Identifier    n
Integer       1
Assign
Identifier    limit
Integer       100
While
Less
Identifier    n
Identifier    limit
Sequence
Sequence
Sequence
Sequence
Sequence
;
Assign
Identifier    k
Integer       3
Assign
Identifier    p
Integer       1
Assign
Identifier    n
Add
Identifier    n
Integer       2
While
And
LessEqual
Multiply
Identifier    k
Identifier    k
Identifier    n
Identifier    p
Sequence
Sequence
;
Assign
Identifier    p
NotEqual
Multiply
Divide
Identifier    n
Identifier    k
Identifier    k
Identifier    n
Assign
Identifier    k
Add
Identifier    k
Integer       2
If
Identifier    p
If
Sequence
Sequence
;
Sequence
Sequence
;
Prti
Identifier    n
;
Prts
String        " is prime\n"
;
Assign
Identifier    count
Add
Identifier    count
Integer       1
;
Sequence
Sequence
Sequence
;
Prts
String        "Total primes found: "
;
Prti
Identifier    count
;
Prts
String        "\n"
;
```



## Forth

Tested with Gforth 0.7.3.

```Forth
CREATE BUF 0 ,              \ single-character look-ahead buffer
: PEEK   BUF @ 0= IF KEY BUF ! THEN BUF @ ;
: GETC   PEEK  0 BUF ! ;
: SPACE?   DUP BL = SWAP  9 14 WITHIN  OR ;
: >SPACE   BEGIN PEEK SPACE? WHILE GETC DROP REPEAT ;
: DIGIT?   48 58 WITHIN ;
: GETINT   >SPACE  0
   BEGIN  PEEK DIGIT?
   WHILE  GETC [CHAR] 0 -  SWAP 10 * +
   REPEAT ;
: GETNAM   >SPACE PAD 1+
   BEGIN PEEK SPACE? INVERT
   WHILE GETC OVER C! CHAR+
   REPEAT  PAD TUCK - 1-  PAD C! ;
: GETSTR   >SPACE PAD  1+ GETC DROP   \ skip leading "
   BEGIN GETC  DUP [CHAR] " <>
   WHILE OVER C! CHAR+
   REPEAT  DROP  PAD TUCK - 1-  PAD C! ;
: INTERN   HERE SWAP  DUP C@ 1+ BOUNDS DO I C@ C, LOOP  ALIGN ;

CREATE #TK 0 ,
: TK:   CREATE #TK @ ,  1 #TK +!  DOES> @ ;
TK: End_of_input      TK: Keyword_if        TK: Keyword_else
TK: Keyword_while     TK: Keyword_print     TK: Keyword_putc
TK: String            TK: Integer           TK: Identifier
TK: LeftParen         TK: RightParen
TK: LeftBrace         TK: RightBrace
TK: Semicolon         TK: Comma
TK: Op_assign         TK: Op_not
: (BINARY?)   [ #TK @ ] literal >= ;
TK: Op_subtract       TK: Op_add
TK: Op_mod            TK: Op_multiply       TK: Op_divide
TK: Op_equal          TK: Op_notequal
TK: Op_less           TK: Op_lessequal
TK: Op_greater        TK: Op_greaterequal
TK: Op_and            TK: Op_or
CREATE TOKEN  0 , 0 , 0 , 0 ,
: TOKEN-TYPE   TOKEN 2 CELLS + @ ;
: TOKEN-VALUE   TOKEN 3 CELLS + @ ;
: GETTOK   GETINT GETINT TOKEN 2!
           GETNAM FIND DROP EXECUTE
	   DUP Integer    = IF GETINT ELSE
	   DUP String     = IF GETSTR INTERN ELSE
	   DUP Identifier = IF GETNAM INTERN ELSE
	   0 THEN THEN THEN
	   TOKEN 3 CELLS + !  TOKEN 2 CELLS + ! ;
: BINARY?   TOKEN-TYPE (BINARY?) ;

CREATE PREC #TK @ CELLS ALLOT  PREC #TK @ CELLS -1 FILL
: PREC!   CELLS PREC + ! ;
14 Op_not          PREC!  13 Op_multiply     PREC!
13 Op_divide       PREC!  13 Op_mod          PREC!
12 Op_add          PREC!  12 Op_subtract     PREC!
10 Op_less         PREC!  10 Op_greater      PREC!
10 Op_lessequal    PREC!  10 Op_greaterequal PREC!
 9 Op_equal        PREC!   9 Op_notequal     PREC!
 5 Op_and          PREC!   4 Op_or           PREC!
: PREC@   CELLS PREC + @ ;

\ Each AST Node is a sequence of cells in data space consisting
\ of the execution token of a printing word, followed by that
\ node's data.  Each printing word receives the address of the
\ node's data, and is responsible for printing that data
\ appropriately.

DEFER .NODE
: .NULL   DROP ." ;" CR ;
CREATE $NULL  ' .NULL ,
: .IDENTIFIER   ." Identifier " @ COUNT TYPE CR ;
: $IDENTIFIER ( a-addr --)  HERE SWAP  ['] .IDENTIFIER , , ;
: .INTEGER   ." Integer " @ . CR ;
: $INTEGER ( n --)  HERE SWAP  ['] .INTEGER , , ;
: "TYPE"   [CHAR] " EMIT  TYPE  [CHAR] " EMIT ;
: .STRING   ." String " @ COUNT "TYPE" CR ;
: $STRING ( a-addr --)  HERE SWAP  ['] .STRING , , ;
: .LEAF   DUP  @ COUNT TYPE CR  CELL+ @ .NODE  0 .NULL ;
: LEAF   CREATE HERE CELL+ ,  BL WORD INTERN .
          DOES> HERE >R ['] .LEAF ,  @ , ,  R> ;
LEAF $PRTC Prtc    LEAF $PRTS Prts       LEAF $PRTI Prti
LEAF $NOT Not      LEAF $NEGATE Negate
: .BINARY   DUP  @ COUNT TYPE CR
            CELL+ DUP @ .NODE  CELL+ @ .NODE ;
: BINARY   CREATE HERE CELL+ ,  BL WORD INTERN .
           DOES> HERE >R ['] .BINARY ,  @ ,  SWAP 2,  R> ;
BINARY $SEQUENCE Sequence   BINARY $ASSIGN Assign
BINARY $WHILE While         BINARY $IF If
BINARY $SUBTRACT Subtract   BINARY $ADD Add
BINARY $MOD Mod             BINARY $MULTIPLY Multiply
BINARY $DIVIDE Divide
BINARY $LESS Less           BINARY $LESSEQUAL LessEqual
BINARY $GREATER Greater     BINARY $GREATEREQUAL GreaterEqual
BINARY $EQUAL Equal         BINARY $NOTEQUAL NotEqual
BINARY $AND And             BINARY $OR Or

: TOK-CONS ( x* -- node-xt) TOKEN-TYPE  CASE
   Op_subtract     OF ['] $SUBTRACT     ENDOF
   Op_add          OF ['] $ADD          ENDOF
   op_mod          OF ['] $MOD          ENDOF
   op_multiply     OF ['] $MULTIPLY     ENDOF
   Op_divide       OF ['] $DIVIDE       ENDOF
   Op_equal        OF ['] $EQUAL        ENDOF
   Op_notequal     OF ['] $NOTEQUAL     ENDOF
   Op_less         OF ['] $LESS         ENDOF
   Op_lessequal    OF ['] $LESSEQUAL    ENDOF
   Op_greater      OF ['] $GREATER      ENDOF
   Op_greaterequal OF ['] $GREATEREQUAL ENDOF
   Op_and          OF ['] $AND          ENDOF
   Op_or           OF ['] $OR           ENDOF
   ENDCASE ;

: (.NODE)   DUP CELL+ SWAP @ EXECUTE ;
' (.NODE) IS .NODE

: .- ( n --)  0 <# #S #> TYPE ;
: EXPECT ( tk --)  DUP TOKEN-TYPE <>
   IF CR ." stdin:" TOKEN 2@ SWAP .- ." :" .-
     ." : unexpected token, expecting " . CR BYE
   THEN  DROP GETTOK ;
: '('   LeftParen EXPECT ;
: ')'   RightParen EXPECT ;
: '}'   RightBrace EXPECT ;
: ';'   Semicolon EXPECT ;
: ','   Comma EXPECT ;
: '='   Op_assign EXPECT ;

DEFER *EXPR  DEFER EXPR  DEFER STMT
: PAREN-EXPR   '(' EXPR ')' ;
: PRIMARY
   TOKEN-TYPE LeftParen   = IF PAREN-EXPR              EXIT THEN
   TOKEN-TYPE Op_add      = IF GETTOK 12 *EXPR         EXIT THEN
   TOKEN-TYPE Op_subtract = IF GETTOK 14 *EXPR $NEGATE EXIT THEN
   TOKEN-TYPE Op_not      = IF GETTOK 14 *EXPR $NOT    EXIT THEN
   TOKEN-TYPE Identifier  = IF TOKEN-VALUE $IDENTIFIER      ELSE
   TOKEN-TYPE Integer     = IF TOKEN-VALUE $INTEGER    THEN THEN
   GETTOK ;
: (*EXPR) ( n -- node)
   PRIMARY ( n node)
   BEGIN OVER TOKEN-TYPE PREC@ SWAP OVER <=  BINARY?  AND
   WHILE ( n node prec) 1+ TOK-CONS SWAP GETTOK *EXPR SWAP EXECUTE
   REPEAT ( n node prec) DROP NIP ( node) ;
: (EXPR)   0 *EXPR ;
: -)?   TOKEN-TYPE RightParen <> ;
: -}?   TOKEN-TYPE RightBrace <> ;
: (STMT)
   TOKEN-TYPE Semicolon = IF GETTOK STMT EXIT THEN
   TOKEN-TYPE Keyword_while =
     IF GETTOK  PAREN-EXPR STMT $WHILE  EXIT THEN
   TOKEN-TYPE Keyword_if =
     IF GETTOK  PAREN-EXPR STMT
       TOKEN-TYPE Keyword_else = IF GETTOK STMT ELSE $NULL THEN
       $IF $IF EXIT
     THEN
   TOKEN-TYPE Keyword_putc =
     IF GETTOK  PAREN-EXPR ';' $PRTC  EXIT THEN
   TOKEN-TYPE Keyword_print =
     IF GETTOK  '(' $NULL
        BEGIN TOKEN-TYPE String =
           IF TOKEN-VALUE $STRING $PRTS  GETTOK
           ELSE EXPR $PRTI THEN  $SEQUENCE  -)?
        WHILE ',' REPEAT  ')' ';'  EXIT THEN
   TOKEN-TYPE Identifier =
     IF TOKEN-VALUE $IDENTIFIER GETTOK '=' EXPR ';' $ASSIGN
        EXIT THEN
   TOKEN-TYPE LeftBrace =
     IF $NULL GETTOK BEGIN -}? WHILE STMT $SEQUENCE REPEAT
        '}' EXIT THEN
   TOKEN-TYPE End_of_input = IF EXIT THEN  EXPR ;
' (*EXPR) IS *EXPR  ' (EXPR) IS EXPR  ' (STMT) IS STMT

: -EOI?   TOKEN-TYPE End_of_input <> ;
: PARSE   $NULL GETTOK BEGIN -EOI? WHILE STMT $SEQUENCE REPEAT ;
PARSE  .NODE
```


<b>

```txt

Sequence
Sequence
;
Assign
Identifier count
Integer 1
While
Less
Identifier count
Integer 10
Sequence
Sequence
;
Sequence
Sequence
Sequence
;
Prts
String "count is: "
;
Prti
Identifier count
;
Prts
String "\n"
;
Assign
Identifier count
Add
Identifier count
Integer 1

```

</b>



## Go

```go
package main

import (
    "bufio"
    "fmt"
    "log"
    "os"
    "strconv"
    "strings"
)

type TokenType int

const (
    tkEOI TokenType = iota
    tkMul
    tkDiv
    tkMod
    tkAdd
    tkSub
    tkNegate
    tkNot
    tkLss
    tkLeq
    tkGtr
    tkGeq
    tkEql
    tkNeq
    tkAssign
    tkAnd
    tkOr
    tkIf
    tkElse
    tkWhile
    tkPrint
    tkPutc
    tkLparen
    tkRparen
    tkLbrace
    tkRbrace
    tkSemi
    tkComma
    tkIdent
    tkInteger
    tkString
)

type NodeType int

const (
    ndIdent NodeType = iota
    ndString
    ndInteger
    ndSequence
    ndIf
    ndPrtc
    ndPrts
    ndPrti
    ndWhile
    ndAssign
    ndNegate
    ndNot
    ndMul
    ndDiv
    ndMod
    ndAdd
    ndSub
    ndLss
    ndLeq
    ndGtr
    ndGeq
    ndEql
    ndNeq
    ndAnd
    ndOr
)

type tokS struct {
    tok    TokenType
    errLn  int
    errCol int
    text   string // ident or string literal or integer value
}

type Tree struct {
    nodeType NodeType
    left     *Tree
    right    *Tree
    value    string
}

// dependency: Ordered by tok, must remain in same order as TokenType consts
type atr struct {
    text             string
    enumText         string
    tok              TokenType
    rightAssociative bool
    isBinary         bool
    isUnary          bool
    precedence       int
    nodeType         NodeType
}

var atrs = []atr{
    {"EOI", "End_of_input", tkEOI, false, false, false, -1, -1},
    {"*", "Op_multiply", tkMul, false, true, false, 13, ndMul},
    {"/", "Op_divide", tkDiv, false, true, false, 13, ndDiv},
    {"%", "Op_mod", tkMod, false, true, false, 13, ndMod},
    {"+", "Op_add", tkAdd, false, true, false, 12, ndAdd},
    {"-", "Op_subtract", tkSub, false, true, false, 12, ndSub},
    {"-", "Op_negate", tkNegate, false, false, true, 14, ndNegate},
    {"!", "Op_not", tkNot, false, false, true, 14, ndNot},
    {"<", "Op_less", tkLss, false, true, false, 10, ndLss},
    {"<=", "Op_lessequal", tkLeq, false, true, false, 10, ndLeq},
    {">", "Op_greater", tkGtr, false, true, false, 10, ndGtr},
    {">=", "Op_greaterequal", tkGeq, false, true, false, 10, ndGeq},
    {"==", "Op_equal", tkEql, false, true, false, 9, ndEql},
    {"!=", "Op_notequal", tkNeq, false, true, false, 9, ndNeq},
    {"=", "Op_assign", tkAssign, false, false, false, -1, ndAssign},
    {"&&", "Op_and", tkAnd, false, true, false, 5, ndAnd},
    {"||", "Op_or", tkOr, false, true, false, 4, ndOr},
    {"if", "Keyword_if", tkIf, false, false, false, -1, ndIf},
    {"else", "Keyword_else", tkElse, false, false, false, -1, -1},
    {"while", "Keyword_while", tkWhile, false, false, false, -1, ndWhile},
    {"print", "Keyword_print", tkPrint, false, false, false, -1, -1},
    {"putc", "Keyword_putc", tkPutc, false, false, false, -1, -1},
    {"(", "LeftParen", tkLparen, false, false, false, -1, -1},
    {")", "RightParen", tkRparen, false, false, false, -1, -1},
    {"{", "LeftBrace", tkLbrace, false, false, false, -1, -1},
    {"}", "RightBrace", tkRbrace, false, false, false, -1, -1},
    {";", "Semicolon", tkSemi, false, false, false, -1, -1},
    {",", "Comma", tkComma, false, false, false, -1, -1},
    {"Ident", "Identifier", tkIdent, false, false, false, -1, ndIdent},
    {"Integer literal", "Integer", tkInteger, false, false, false, -1, ndInteger},
    {"String literal", "String", tkString, false, false, false, -1, ndString},
}

var displayNodes = []string{
    "Identifier", "String", "Integer", "Sequence", "If", "Prtc", "Prts", "Prti",
    "While", "Assign", "Negate", "Not", "Multiply", "Divide", "Mod", "Add",
    "Subtract", "Less", "LessEqual", "Greater", "GreaterEqual", "Equal",
    "NotEqual", "And", "Or",
}

var (
    err     error
    token   tokS
    scanner *bufio.Scanner
)

func reportError(errLine, errCol int, msg string) {
    log.Fatalf("(%d, %d) error : %s\n", errLine, errCol, msg)
}

func check(err error) {
    if err != nil {
        log.Fatal(err)
    }
}

func getEum(name string) TokenType { // return internal version of name#
    for _, atr := range atrs {
        if atr.enumText == name {
            return atr.tok
        }
    }
    reportError(0, 0, fmt.Sprintf("Unknown token %s\n", name))
    return tkEOI
}

func getTok() tokS {
    tok := tokS{}
    if scanner.Scan() {
        line := strings.TrimRight(scanner.Text(), " \t")
        fields := strings.Fields(line)
        // [ ]*{lineno}[ ]+{colno}[ ]+token[ ]+optional
        tok.errLn, err = strconv.Atoi(fields[0])
        check(err)
        tok.errCol, err = strconv.Atoi(fields[1])
        check(err)
        tok.tok = getEum(fields[2])
        le := len(fields)
        if le == 4 {
            tok.text = fields[3]
        } else if le > 4 {
            idx := strings.Index(line, `"`)
            tok.text = line[idx:]
        }
    }
    check(scanner.Err())
    return tok
}

func makeNode(nodeType NodeType, left *Tree, right *Tree) *Tree {
    return &Tree{nodeType, left, right, ""}
}

func makeLeaf(nodeType NodeType, value string) *Tree {
    return &Tree{nodeType, nil, nil, value}
}

func expect(msg string, s TokenType) {
    if token.tok == s {
        token = getTok()
        return
    }
    reportError(token.errLn, token.errCol,
        fmt.Sprintf("%s: Expecting '%s', found '%s'\n", msg, atrs[s].text, atrs[token.tok].text))
}

func expr(p int) *Tree {
    var x, node *Tree
    switch token.tok {
    case tkLparen:
        x = parenExpr()
    case tkSub, tkAdd:
        op := token.tok
        token = getTok()
        node = expr(atrs[tkNegate].precedence)
        if op == tkSub {
            x = makeNode(ndNegate, node, nil)
        } else {
            x = node
        }
    case tkNot:
        token = getTok()
        x = makeNode(ndNot, expr(atrs[tkNot].precedence), nil)
    case tkIdent:
        x = makeLeaf(ndIdent, token.text)
        token = getTok()
    case tkInteger:
        x = makeLeaf(ndInteger, token.text)
        token = getTok()
    default:
        reportError(token.errLn, token.errCol,
            fmt.Sprintf("Expecting a primary, found: %s\n", atrs[token.tok].text))
    }

    for atrs[token.tok].isBinary && atrs[token.tok].precedence >= p {
        op := token.tok
        token = getTok()
        q := atrs[op].precedence
        if !atrs[op].rightAssociative {
            q++
        }
        node = expr(q)
        x = makeNode(atrs[op].nodeType, x, node)
    }
    return x
}

func parenExpr() *Tree {
    expect("parenExpr", tkLparen)
    t := expr(0)
    expect("parenExpr", tkRparen)
    return t
}

func stmt() *Tree {
    var t, v, e, s, s2 *Tree
    switch token.tok {
    case tkIf:
        token = getTok()
        e = parenExpr()
        s = stmt()
        s2 = nil
        if token.tok == tkElse {
            token = getTok()
            s2 = stmt()
        }
        t = makeNode(ndIf, e, makeNode(ndIf, s, s2))
    case tkPutc:
        token = getTok()
        e = parenExpr()
        t = makeNode(ndPrtc, e, nil)
        expect("Putc", tkSemi)
    case tkPrint: // print '(' expr {',' expr} ')'
        token = getTok()
        for expect("Print", tkLparen); ; expect("Print", tkComma) {
            if token.tok == tkString {
                e = makeNode(ndPrts, makeLeaf(ndString, token.text), nil)
                token = getTok()
            } else {
                e = makeNode(ndPrti, expr(0), nil)
            }
            t = makeNode(ndSequence, t, e)
            if token.tok != tkComma {
                break
            }
        }
        expect("Print", tkRparen)
        expect("Print", tkSemi)
    case tkSemi:
        token = getTok()
    case tkIdent:
        v = makeLeaf(ndIdent, token.text)
        token = getTok()
        expect("assign", tkAssign)
        e = expr(0)
        t = makeNode(ndAssign, v, e)
        expect("assign", tkSemi)
    case tkWhile:
        token = getTok()
        e = parenExpr()
        s = stmt()
        t = makeNode(ndWhile, e, s)
    case tkLbrace: // {stmt}
        for expect("Lbrace", tkLbrace); token.tok != tkRbrace && token.tok != tkEOI; {
            t = makeNode(ndSequence, t, stmt())
        }
        expect("Lbrace", tkRbrace)
    case tkEOI:
        // do nothing
    default:
        reportError(token.errLn, token.errCol,
            fmt.Sprintf("expecting start of statement, found '%s'\n", atrs[token.tok].text))
    }
    return t
}

func parse() *Tree {
    var t *Tree
    token = getTok()
    for {
        t = makeNode(ndSequence, t, stmt())
        if t == nil || token.tok == tkEOI {
            break
        }
    }
    return t
}

func prtAst(t *Tree) {
    if t == nil {
        fmt.Print(";\n")
    } else {
        fmt.Printf("%-14s ", displayNodes[t.nodeType])
        if t.nodeType == ndIdent || t.nodeType == ndInteger || t.nodeType == ndString {
            fmt.Printf("%s\n", t.value)
        } else {
            fmt.Println()
            prtAst(t.left)
            prtAst(t.right)
        }
    }
}

func main() {
    source, err := os.Open("source.txt")
    check(err)
    defer source.Close()
    scanner = bufio.NewScanner(source)
    prtAst(parse())
}
```


Prime Numbers example:

```txt

Sequence
Sequence
Sequence
Sequence
Sequence
;
Assign
Identifier     count
Integer        1
Assign
Identifier     n
Integer        1
Assign
Identifier     limit
Integer        100
While
Less
Identifier     n
Identifier     limit
Sequence
Sequence
Sequence
Sequence
Sequence
;
Assign
Identifier     k
Integer        3
Assign
Identifier     p
Integer        1
Assign
Identifier     n
Add
Identifier     n
Integer        2
While
And
LessEqual
Multiply
Identifier     k
Identifier     k
Identifier     n
Identifier     p
Sequence
Sequence
;
Assign
Identifier     p
NotEqual
Multiply
Divide
Identifier     n
Identifier     k
Identifier     k
Identifier     n
Assign
Identifier     k
Add
Identifier     k
Integer        2
If
Identifier     p
If
Sequence
Sequence
;
Sequence
Sequence
;
Prti
Identifier     n
;
Prts
String         " is prime\n"
;
Assign
Identifier     count
Add
Identifier     count
Integer        1
;
Sequence
Sequence
Sequence
;
Prts
String         "Total primes found: "
;
Prti
Identifier     count
;
Prts
String         "\n"
;

```


## Java

Usage: java Parser infile [>outfile]
```java

import java.io.File;
import java.io.FileNotFoundException;
import java.util.Scanner;
import java.util.StringTokenizer;
import java.util.List;
import java.util.ArrayList;
import java.util.Map;
import java.util.HashMap;

class Parser {
	private List<Token> source;
	private Token token;
	private int position;

	static class Node {
		public NodeType nt;
		public Node left, right;
		public String value;

		Node() {
			this.nt = null;
			this.left = null;
			this.right = null;
			this.value = null;
		}
		Node(NodeType node_type, Node left, Node right, String value) {
			this.nt = node_type;
			this.left = left;
			this.right = right;
			this.value = value;
		}
		public static Node make_node(NodeType nodetype, Node left, Node right) {
			return new Node(nodetype, left, right, "");
		}
		public static Node make_node(NodeType nodetype, Node left) {
			return new Node(nodetype, left, null, "");
		}
		public static Node make_leaf(NodeType nodetype, String value) {
			return new Node(nodetype, null, null, value);
		}
	}

	static class Token {
		public TokenType tokentype;
		public String value;
		public int line;
		public int pos;

		Token(TokenType token, String value, int line, int pos) {
			this.tokentype = token; this.value = value; this.line = line; this.pos = pos;
		}
		@Override
		public String toString() {
			return String.format("%5d  %5d %-15s %s", this.line, this.pos, this.tokentype, this.value);
		}
	}

	static enum TokenType {
		End_of_input(false, false, false, -1, NodeType.nd_None),
		Op_multiply(false, true, false, 13, NodeType.nd_Mul),
		Op_divide(false, true, false, 13, NodeType.nd_Div),
		Op_mod(false, true, false, 13, NodeType.nd_Mod),
		Op_add(false, true, false, 12, NodeType.nd_Add),
		Op_subtract(false, true, false, 12, NodeType.nd_Sub),
		Op_negate(false, false, true, 14, NodeType.nd_Negate),
		Op_not(false, false, true, 14, NodeType.nd_Not),
		Op_less(false, true, false, 10, NodeType.nd_Lss),
		Op_lessequal(false, true, false, 10, NodeType.nd_Leq),
		Op_greater(false, true, false, 10, NodeType.nd_Gtr),
		Op_greaterequal(false, true, false, 10, NodeType.nd_Geq),
		Op_equal(false, true, true, 9, NodeType.nd_Eql),
		Op_notequal(false, true, false, 9, NodeType.nd_Neq),
		Op_assign(false, false, false, -1, NodeType.nd_Assign),
		Op_and(false, true, false, 5, NodeType.nd_And),
		Op_or(false, true, false, 4, NodeType.nd_Or),
		Keyword_if(false, false, false, -1, NodeType.nd_If),
		Keyword_else(false, false, false, -1, NodeType.nd_None),
		Keyword_while(false, false, false, -1, NodeType.nd_While),
		Keyword_print(false, false, false, -1, NodeType.nd_None),
		Keyword_putc(false, false, false, -1, NodeType.nd_None),
		LeftParen(false, false, false, -1, NodeType.nd_None),
		RightParen(false, false, false, -1, NodeType.nd_None),
		LeftBrace(false, false, false, -1, NodeType.nd_None),
		RightBrace(false, false, false, -1, NodeType.nd_None),
		Semicolon(false, false, false, -1, NodeType.nd_None),
		Comma(false, false, false, -1, NodeType.nd_None),
		Identifier(false, false, false, -1, NodeType.nd_Ident),
		Integer(false, false, false, -1, NodeType.nd_Integer),
		String(false, false, false, -1, NodeType.nd_String);

		private final int precedence;
		private final boolean right_assoc;
		private final boolean is_binary;
		private final boolean is_unary;
		private final NodeType node_type;

		TokenType(boolean right_assoc, boolean is_binary, boolean is_unary, int precedence, NodeType node) {
			this.right_assoc = right_assoc;
			this.is_binary = is_binary;
			this.is_unary = is_unary;
			this.precedence = precedence;
			this.node_type = node;
		}
		boolean isRightAssoc() { return this.right_assoc; }
		boolean isBinary() { return this.is_binary; }
		boolean isUnary() { return this.is_unary; }
		int getPrecedence() { return this.precedence; }
		NodeType getNodeType() { return this.node_type; }
	}
	static enum NodeType {
		nd_None(""), nd_Ident("Identifier"), nd_String("String"), nd_Integer("Integer"), nd_Sequence("Sequence"), nd_If("If"),
		nd_Prtc("Prtc"), nd_Prts("Prts"), nd_Prti("Prti"), nd_While("While"),
		nd_Assign("Assign"), nd_Negate("Negate"), nd_Not("Not"), nd_Mul("Multiply"), nd_Div("Divide"), nd_Mod("Mod"), nd_Add("Add"),
		nd_Sub("Subtract"), nd_Lss("Less"), nd_Leq("LessEqual"),
		nd_Gtr("Greater"), nd_Geq("GreaterEqual"), nd_Eql("Equal"), nd_Neq("NotEqual"), nd_And("And"), nd_Or("Or");

		private final String name;

		NodeType(String name) {
			this.name = name;
		}

		@Override
		public String toString() { return this.name; }
	}
	static void error(int line, int pos, String msg) {
		if (line > 0 && pos > 0) {
			System.out.printf("%s in line %d, pos %d\n", msg, line, pos);
		} else {
			System.out.println(msg);
		}
		System.exit(1);
	}
	Parser(List<Token> source) {
		this.source = source;
		this.token = null;
		this.position = 0;
	}
	Token getNextToken() {
		this.token = this.source.get(this.position++);
		return this.token;
	}
	Node expr(int p) {
		Node result = null, node;
		TokenType op;
		int q;

		if (this.token.tokentype == TokenType.LeftParen) {
			result = paren_expr();
		} else if (this.token.tokentype == TokenType.Op_add || this.token.tokentype == TokenType.Op_subtract) {
			op = (this.token.tokentype == TokenType.Op_subtract) ? TokenType.Op_negate : TokenType.Op_add;
			getNextToken();
			node = expr(TokenType.Op_negate.getPrecedence());
			result = (op == TokenType.Op_negate) ? Node.make_node(NodeType.nd_Negate, node) : node;
		} else if (this.token.tokentype == TokenType.Op_not) {
			getNextToken();
			result = Node.make_node(NodeType.nd_Not, expr(TokenType.Op_not.getPrecedence()));
		} else if (this.token.tokentype == TokenType.Identifier) {
			result = Node.make_leaf(NodeType.nd_Ident, this.token.value);
			getNextToken();
		} else if (this.token.tokentype == TokenType.Integer) {
			result = Node.make_leaf(NodeType.nd_Integer, this.token.value);
			getNextToken();
		} else {
			error(this.token.line, this.token.pos, "Expecting a primary, found: " + this.token.tokentype);
		}

		while (this.token.tokentype.isBinary() && this.token.tokentype.getPrecedence() >= p) {
			op = this.token.tokentype;
			getNextToken();
			q = op.getPrecedence();
			if (!op.isRightAssoc()) {
				q++;
			}
			node = expr(q);
			result = Node.make_node(op.getNodeType(), result, node);
		}
		return result;
	}
	Node paren_expr() {
		expect("paren_expr", TokenType.LeftParen);
		Node node = expr(0);
		expect("paren_expr", TokenType.RightParen);
		return node;
	}
	void expect(String msg, TokenType s) {
		if (this.token.tokentype == s) {
			getNextToken();
			return;
		}
		error(this.token.line, this.token.pos, msg + ": Expecting '" + s + "', found: '" + this.token.tokentype + "'");
	}
	Node stmt() {
		Node s, s2, t = null, e, v;
		if (this.token.tokentype == TokenType.Keyword_if) {
			getNextToken();
			e = paren_expr();
			s = stmt();
			s2 = null;
			if (this.token.tokentype == TokenType.Keyword_else) {
				getNextToken();
				s2 = stmt();
			}
			t = Node.make_node(NodeType.nd_If, e, Node.make_node(NodeType.nd_If, s, s2));
		} else if (this.token.tokentype == TokenType.Keyword_putc) {
			getNextToken();
			e = paren_expr();
			t = Node.make_node(NodeType.nd_Prtc, e);
			expect("Putc", TokenType.Semicolon);
		} else if (this.token.tokentype == TokenType.Keyword_print) {
			getNextToken();
			expect("Print", TokenType.LeftParen);
			while (true) {
				if (this.token.tokentype == TokenType.String) {
					e = Node.make_node(NodeType.nd_Prts, Node.make_leaf(NodeType.nd_String, this.token.value));
					getNextToken();
				} else {
					e = Node.make_node(NodeType.nd_Prti, expr(0), null);
				}
				t = Node.make_node(NodeType.nd_Sequence, t, e);
				if (this.token.tokentype != TokenType.Comma) {
					break;
				}
				getNextToken();
			}
			expect("Print", TokenType.RightParen);
			expect("Print", TokenType.Semicolon);
		} else if (this.token.tokentype == TokenType.Semicolon) {
			getNextToken();
		} else if (this.token.tokentype == TokenType.Identifier) {
			v = Node.make_leaf(NodeType.nd_Ident, this.token.value);
			getNextToken();
			expect("assign", TokenType.Op_assign);
			e = expr(0);
			t = Node.make_node(NodeType.nd_Assign, v, e);
			expect("assign", TokenType.Semicolon);
		} else if (this.token.tokentype == TokenType.Keyword_while) {
			getNextToken();
			e = paren_expr();
			s = stmt();
			t = Node.make_node(NodeType.nd_While, e, s);
		} else if (this.token.tokentype == TokenType.LeftBrace) {
			getNextToken();
			while (this.token.tokentype != TokenType.RightBrace && this.token.tokentype != TokenType.End_of_input) {
				t = Node.make_node(NodeType.nd_Sequence, t, stmt());
			}
			expect("LBrace", TokenType.RightBrace);
		} else if (this.token.tokentype == TokenType.End_of_input) {
		} else {
			error(this.token.line, this.token.pos, "Expecting start of statement, found: " + this.token.tokentype);
		}
		return t;
	}
	Node parse() {
		Node t = null;
		getNextToken();
		while (this.token.tokentype != TokenType.End_of_input) {
			t = Node.make_node(NodeType.nd_Sequence, t, stmt());
		}
		return t;
	}
	void printAST(Node t) {
		int i = 0;
		if (t == null) {
			System.out.println(";");
		} else {
			System.out.printf("%-14s", t.nt);
			if (t.nt == NodeType.nd_Ident || t.nt == NodeType.nd_Integer || t.nt == NodeType.nd_String) {
				System.out.println(" " + t.value);
			} else {
				System.out.println();
				printAST(t.left);
				printAST(t.right);
			}
		}
	}
	public static void main(String[] args) {
		if (args.length > 0) {
			try {
				String value, token;
				int line, pos;
				Token t;
				boolean found;
				List<Token> list = new ArrayList<>();
				Map<String, TokenType> str_to_tokens = new HashMap<>();

				str_to_tokens.put("End_of_input", TokenType.End_of_input);
				str_to_tokens.put("Op_multiply", TokenType.Op_multiply);
				str_to_tokens.put("Op_divide", TokenType.Op_divide);
				str_to_tokens.put("Op_mod", TokenType.Op_mod);
				str_to_tokens.put("Op_add", TokenType.Op_add);
				str_to_tokens.put("Op_subtract", TokenType.Op_subtract);
				str_to_tokens.put("Op_negate", TokenType.Op_negate);
				str_to_tokens.put("Op_not", TokenType.Op_not);
				str_to_tokens.put("Op_less", TokenType.Op_less);
				str_to_tokens.put("Op_lessequal", TokenType.Op_lessequal);
				str_to_tokens.put("Op_greater", TokenType.Op_greater);
				str_to_tokens.put("Op_greaterequal", TokenType.Op_greaterequal);
				str_to_tokens.put("Op_equal", TokenType.Op_equal);
				str_to_tokens.put("Op_notequal", TokenType.Op_notequal);
				str_to_tokens.put("Op_assign", TokenType.Op_assign);
				str_to_tokens.put("Op_and", TokenType.Op_and);
				str_to_tokens.put("Op_or", TokenType.Op_or);
				str_to_tokens.put("Keyword_if", TokenType.Keyword_if);
				str_to_tokens.put("Keyword_else", TokenType.Keyword_else);
				str_to_tokens.put("Keyword_while", TokenType.Keyword_while);
				str_to_tokens.put("Keyword_print", TokenType.Keyword_print);
				str_to_tokens.put("Keyword_putc", TokenType.Keyword_putc);
				str_to_tokens.put("LeftParen", TokenType.LeftParen);
				str_to_tokens.put("RightParen", TokenType.RightParen);
				str_to_tokens.put("LeftBrace", TokenType.LeftBrace);
				str_to_tokens.put("RightBrace", TokenType.RightBrace);
				str_to_tokens.put("Semicolon", TokenType.Semicolon);
				str_to_tokens.put("Comma", TokenType.Comma);
				str_to_tokens.put("Identifier", TokenType.Identifier);
				str_to_tokens.put("Integer", TokenType.Integer);
				str_to_tokens.put("String", TokenType.String);

				Scanner s = new Scanner(new File(args[0]));
				String source = " ";
				while (s.hasNext()) {
					String str = s.nextLine();
					StringTokenizer st = new StringTokenizer(str);
					line = Integer.parseInt(st.nextToken());
					pos = Integer.parseInt(st.nextToken());
					token = st.nextToken();
					value = "";
					while (st.hasMoreTokens()) {
						value += st.nextToken() + " ";
					}
					found = false;
					if (str_to_tokens.containsKey(token)) {
						found = true;
						list.add(new Token(str_to_tokens.get(token), value, line, pos));
					}
					if (found == false) {
						throw new Exception("Token not found: '" + token + "'");
					}
				}
				Parser p = new Parser(list);
				p.printAST(p.parse());
			} catch (FileNotFoundException e) {
				error(-1, -1, "Exception: " + e.getMessage());
			} catch (Exception e) {
				error(-1, -1, "Exception: " + e.getMessage());
			}
		} else {
			error(-1, -1, "No args");
		}
	}
}

```



## Julia

Julia tends to discourage large numbers of global variables, so this direct port from the Python reference implementation moves the globals into a function wrapper.
```julia
struct ASTnode
    nodetype::Int
    left::Union{Nothing, ASTnode}
    right::Union{Nothing, ASTnode}
    value::Union{Nothing, Int, String}
end

function syntaxanalyzer(inputfile)
    tkEOI, tkMul, tkDiv, tkMod, tkAdd, tkSub, tkNegate, tkNot, tkLss, tkLeq, tkGtr, tkGeq,
    tkEql, tkNeq, tkAssign, tkAnd, tkOr, tkIf, tkElse, tkWhile, tkPrint, tkPutc, tkLparen, tkRparen,
    tkLbrace, tkRbrace, tkSemi, tkComma, tkIdent, tkInteger, tkString = collect(1:31)

    ndIdent, ndString, ndInteger, ndSequence, ndIf, ndPrtc, ndPrts, ndPrti, ndWhile,
    ndAssign, ndNegate, ndNot, ndMul, ndDiv, ndMod, ndAdd, ndSub, ndLss, ndLeq,
    ndGtr, ndGeq, ndEql, ndNeq, ndAnd, ndOr = collect(1:25)

    TK_NAME, TK_RIGHT_ASSOC, TK_IS_BINARY, TK_IS_UNARY, TK_PRECEDENCE, TK_NODE = collect(1:6) # label Token columns
    Tokens = [
    ["EOI"             , false, false, false, -1, -1       ],
    ["*"               , false, true,  false, 13, ndMul    ],
    ["/"               , false, true,  false, 13, ndDiv    ],
    ["%"               , false, true,  false, 13, ndMod    ],
    ["+"               , false, true,  false, 12, ndAdd    ],
    ["-"               , false, true,  false, 12, ndSub    ],
    ["-"               , false, false, true,  14, ndNegate ],
    ["!"               , false, false, true,  14, ndNot    ],
    ["<"               , false, true,  false, 10, ndLss    ],
    ["<="              , false, true,  false, 10, ndLeq    ],
    [">"               , false, true,  false, 10, ndGtr    ],
    [">="              , false, true,  false, 10, ndGeq    ],
    ["=="              , false, true,  false,  9, ndEql    ],
    ["!="              , false, true,  false,  9, ndNeq    ],
    ["="               , false, false, false, -1, ndAssign ],
    ["&&"              , false, true,  false,  5, ndAnd    ],
    ["||"              , false, true,  false,  4, ndOr     ],
    ["if"              , false, false, false, -1, ndIf     ],
    ["else"            , false, false, false, -1, -1       ],
    ["while"           , false, false, false, -1, ndWhile  ],
    ["print"           , false, false, false, -1, -1       ],
    ["putc"            , false, false, false, -1, -1       ],
    ["("               , false, false, false, -1, -1       ],
    [")"               , false, false, false, -1, -1       ],
    ["{"               , false, false, false, -1, -1       ],
    ["}"               , false, false, false, -1, -1       ],
    [";"               , false, false, false, -1, -1       ],
    [","               , false, false, false, -1, -1       ],
    ["Ident"           , false, false, false, -1, ndIdent  ],
    ["Integer literal" , false, false, false, -1, ndInteger],
    ["String literal"  , false, false, false, -1, ndString ]]

    allsyms = Dict(
        "End_of_input" => tkEOI, "Op_multiply" => tkMul, "Op_divide" => tkDiv,
        "Op_mod" => tkMod, "Op_add" => tkAdd, "Op_subtract" => tkSub,
        "Op_negate" => tkNegate, "Op_not" => tkNot, "Op_less" => tkLss,
        "Op_lessequal" => tkLeq, "Op_greater" => tkGtr, "Op_greaterequal" => tkGeq,
        "Op_equal" => tkEql, "Op_notequal" => tkNeq, "Op_assign" => tkAssign,
        "Op_and" => tkAnd, "Op_or" => tkOr, "Keyword_if" => tkIf, "Keyword_else" => tkElse,
        "Keyword_while" => tkWhile, "Keyword_print" => tkPrint, "Keyword_putc" => tkPutc,
        "LeftParen" => tkLparen, "RightParen" => tkRparen, "LeftBrace" => tkLbrace,
        "RightBrace" => tkRbrace, "Semicolon" => tkSemi, "Comma" => tkComma,
        "Identifier" => tkIdent, "Integer" => tkInteger, "String" => tkString)

    displaynodes = ["Identifier", "String", "Integer", "Sequence", "If", "Prtc", "Prts", "Prti", "While",
                     "Assign", "Negate", "Not", "Multiply", "Divide", "Mod", "Add", "Subtract", "Less",
                     "LessEqual", "Greater", "GreaterEqual", "Equal", "NotEqual", "And", "Or"]

    errline, errcol, tok, toktext = fill("", 4)
    error(msg) = throw("Error in syntax: $msg.")
    nilnode = ASTnode(0, nothing, nothing, nothing)
    tokother = ""

    function gettok()
        s = readline(inputfile)
        if length(s) == 0
            error("empty line")
        end
        linelist = split(strip(s), r"\s+", limit = 4)
        # line col Ident varname
        # 0    1   2     3
        errline, errcol, toktext = linelist[1:3]
        if !haskey(allsyms, toktext)
            error("Unknown token $toktext")
        end
        tok = allsyms[toktext]
        tokother = (tok in [tkInteger, tkIdent, tkString]) ? linelist[4] : ""
     end

    makenode(oper, left, right = nilnode) = ASTnode(oper, left, right, nothing)
    makeleaf(oper, n::Int) = ASTnode(oper, nothing, nothing, n)
    makeleaf(oper, n) = ASTnode(oper, nothing, nothing, string(n))
    expect(msg, s) = if tok != s error("msg: Expecting $(Tokens[s][TK_NAME]), found $(Tokens[tok][TK_NAME])") else gettok() end

    function expr(p)
        x = nilnode
        if tok == tkLparen
            x = parenexpr()
        elseif tok in [tkSub, tkAdd]
            op = tok == tkSub ? tkNegate : tkAdd
            gettok()
            node = expr(Tokens[tkNegate][TK_PRECEDENCE])
            x = (op == tkNegate) ? makenode(ndNegate, node) : node
        elseif tok == tkNot
            gettok()
            x = makenode(ndNot, expr(Tokens[tkNot][TK_PRECEDENCE]))
        elseif tok == tkIdent
            x = makeleaf(ndIdent, tokother)
            gettok()
        elseif tok == tkInteger
            x = makeleaf(ndInteger, tokother)
            gettok()
        else
            error("Expecting a primary, found: $(Tokens[tok][TK_NAME])")
        end
        while Tokens[tok][TK_IS_BINARY] && (Tokens[tok][TK_PRECEDENCE] >= p)
            op = tok
            gettok()
            q = Tokens[op][TK_PRECEDENCE]
            if !Tokens[op][TK_RIGHT_ASSOC]
                q += 1
            end
            node = expr(q)
            x = makenode(Tokens[op][TK_NODE], x, node)
        end
        x
    end

    parenexpr() = (expect("paren_expr", tkLparen); node = expr(0); expect("paren_expr", tkRparen); node)

    function stmt()
        t = nilnode
        if tok == tkIf
            gettok()
            e = parenexpr()
            s = stmt()
            s2 = nilnode
            if tok == tkElse
                gettok()
                s2 = stmt()
            end
            t = makenode(ndIf, e, makenode(ndIf, s, s2))
        elseif tok == tkPutc
            gettok()
            e = parenexpr()
            t = makenode(ndPrtc, e)
            expect("Putc", tkSemi)
        elseif tok == tkPrint
            gettok()
            expect("Print", tkLparen)
            while true
                if tok == tkString
                    e = makenode(ndPrts, makeleaf(ndString, tokother))
                    gettok()
                else
                    e = makenode(ndPrti, expr(0))
                end
                t = makenode(ndSequence, t, e)
                if tok != tkComma
                    break
                end
                gettok()
            end
            expect("Print", tkRparen)
            expect("Print", tkSemi)
        elseif tok == tkSemi
            gettok()
        elseif tok == tkIdent
            v = makeleaf(ndIdent, tokother)
            gettok()
            expect("assign", tkAssign)
            e = expr(0)
            t = makenode(ndAssign, v, e)
            expect("assign", tkSemi)
        elseif tok == tkWhile
            gettok()
            e = parenexpr()
            s = stmt()
            t = makenode(ndWhile, e, s)
        elseif tok == tkLbrace
            gettok()
            while (tok != tkRbrace) && (tok != tkEOI)
                t = makenode(ndSequence, t, stmt())
            end
            expect("Lbrace", tkRbrace)
        elseif tok != tkEOI
            error("Expecting start of statement, found: $(Tokens[tok][TK_NAME])")
        end
        return t
    end

    function parse()
        t = nilnode
        gettok()
        while true
            t = makenode(ndSequence, t, stmt())
            if (tok == tkEOI) || (t == nilnode)
                break
            end
        end
        t
    end

    function prtASTnode(t)
        if t == nothing
            return
        elseif t == nilnode
            println(";")
        elseif t.nodetype in [ndIdent, ndInteger, ndString]
            println(rpad(displaynodes[t.nodetype], 14), t.value)
        else
            println(rpad(displaynodes[t.nodetype], 14))
        end
        prtASTnode(t.left)
        prtASTnode(t.right)
    end

    # runs the function
    prtASTnode(parse())
end

testtxt = """
    1      1 Identifier      count
    1      7 Op_assign
    1      9 Integer             1
    1     10 Semicolon
    2      1 Keyword_while
    2      7 LeftParen
    2      8 Identifier      count
    2     14 Op_less
    2     16 Integer            10
    2     18 RightParen
    2     20 LeftBrace
    3      5 Keyword_print
    3     10 LeftParen
    3     11 String          \"count is: \"
    3     23 Comma
    3     25 Identifier      count
    3     30 Comma
    3     32 String          \"\\n\"
    3     36 RightParen
    3     37 Semicolon
    4      5 Identifier      count
    4     11 Op_assign
    4     13 Identifier      count
    4     19 Op_add
    4     21 Integer             1
    4     22 Semicolon
    5      1 RightBrace
    6      1 End_of_input           """

syntaxanalyzer(IOBuffer(testtxt))  # for isolated testing

# syntaxanalyzer(length(ARGS) > 1 ? ARGS[1] : stdin) # for use as in the Python code

```



## M2000 Interpreter

This program written without functions. Subs use the current stack of values (a feature from interpreter) to return arrays. Subs run on same scope as the module or function which called. We use Local to make local variables (erased at return). Sub prt_ast() called first time without passing parameter, because parameter already exist in stack of values. Interpreter when call a module, a function, a subroutine always pass values to stack of values. Functions called in an expression, always have own stack of values. Modules call other modules passing the same stack of values. Threads are parts of modules, with same scope in module where belong, but have own stack and static variables, and they rub in time intervals.

A (1,2,3) is an auto array or tuple. We can assign a tuple in a variable, in a item in another tuple. A tuple is a reference type, but here we don't use a second pointer (we say references variables which references to other variables - reference or value type-, so we say pointer the reference who hold an object alive. We can read 2nd item (expected string) from alfa, a pointer to array, using array$(alfa,1) or alfa#val$(1). The second variation can be used multiple times if a tuple has another tulple so alfa#val(2)#val$(1) return a string from 3rd item, which expect a tuple from 2nd item. The other variation array$(array(alfa,2),1) for the same result.



```M2000 Interpreter

Module syntax_analyzer(b$){
	enum tokens {
		Op_add, Op_subtract, Op_not=5, Op_multiply=10, Op_divide, Op_mod,
		Op_negate,  Op_less, Op_lessequal, Op_greater, Op_greaterequal,
		Op_equal, Op_notequal, Op_and, Op_or, Op_assign=100, Keyword_if=110,
		Keyword_else, Keyword_while, Keyword_print, Keyword_putc, LeftParen, RightParen,
		LeftBrace, RightBrace, Semicolon, Comma, Identifier, Integer, String, End_of_input
	}

	Inventory precedence=Op_multiply:=13, Op_divide:=13, Op_mod:=13, Op_add:=12, Op_subtract:=12
	Append  precedence, Op_negate:=14, Op_not:=14, Op_less:=10, Op_lessequal:=10, Op_greater:=10
	Append  precedence, Op_greaterequal:=10, Op_equal:=9, Op_notequal:=9, Op_assign:=-1, Op_and:=5
	Append  precedence, Op_or:=4

	Inventory symbols=Op_multiply:="Multiply", Op_divide:="Divide", Op_mod:="Mod", Op_add:="Add"
	Append  symbols, Op_negate:="Negate", Op_not:="Not", Op_less:="Less", Op_subtract:="Subtract"
	Append  symbols, Op_lessequal:="LessEqual", Op_greater:="Greater", Op_greaterequal:="GreaterEqual"
	Append  symbols, Op_equal:="Equal", Op_notequal:="NotEqual",  Op_and:="And", Op_or:="Or"

	def lineNo, ColumnNo, m, line$, a, lim, cur=-1
	const nl$=chr$(13)+chr$(10), Ansi=3
	Dim lex$()
	lex$()=piece$(b$,chr$(13)+chr$(10))
	lim=dimension(lex$(),1)-1
	op=End_of_input
	flush
	k=0
	Try {
		push (,)   ' Null
		getone(&op)
		repeat
		stmt(&op)
		shift 2  ' swap two top items
		push ("Sequence", array, array)
		k++
		until op=End_of_Input
	}
	er$=error$
	if er$<>"" then print er$ : flush: break
	Print "Ast"
	Document Output$
	prt_ast()
	clipboard Output$
	Save.Doc Output$, "parse.t", Ansi
	document parse$
	Load.Doc parse$,"parse.t", Ansi
	Report parse$

	sub prt_ast(t)
		if len(t)<1 then
			Output$=";"+nl$
		else.if len(t)=3 then
			Output$=t#val$(0) +nl$
			prt_ast(t#val(1)) : prt_ast(t#val(2))
		else
			Output$=t#val$(0) +nl$
		end if
	end sub
	sub expr(p)   ' only a number
		local x=(,), prev=op
		if  op>=Identifier then
			x=(line$,)
			getone(&op)
		else.if op=LeftParen then
			paren_exp()
			x=array
		else.if op<10 then
			getone(&op)
			expr(precedence(int(Op_negate)))
			read local y
			if prev=Op_add then
				x=y
			else
				if prev=Op_subtract then prev=Op_negate
				x=(symbols(prev), y,(,))
			End if
		else
			 {error "??? "+eval$(op)}
		end if
		local prec
		while exist(precedence, int(op))
			prev=op : prec=eval(precedence)
			if prec<14 and prec>=p else exit
			getone(&op)
			expr(prec+1)  ' all operators are left associative (use prec for right a.)
			x=(symbols(int(prev)), x, array)
		End While
		Push x
	end sub
	sub paren_exp()
		expected(LeftParen)
		getone(&op)
		expr(0)
		expected(RightParen)
		getone(&op)
	end sub
	sub stmt(&op)
		local t=(,)
		if op=Identifier then
			t=(line$)
			getone(&op)
			expected(Op_assign)
			getone(&op)
			expr(0)
			read local rightnode
			Push ("Assign",t,rightnode)
			expected(Semicolon)
			getone(&op)
		else.if op=Semicolon then
			getone(&op)
			Push (";",)
		else.if op=Keyword_print then
			getone(&op)
			expected(LeftParen)
			repeat
				getone(&op)
				if op=String then
					Push ("Prts",(line$,),(,))
					getone(&op)
				else
					expr(0)
					Push ("Prti", array,(,))
				end if
				t=("Sequence", t, array)
			until op<>Comma
			expected(RightParen)
			getone(&op)
			expected(Semicolon)
			getone(&op)
			push t
		else.if op=Keyword_while then
			getone(&op)
			paren_exp()
			stmt(&op)
			shift 2
			Push ("While",array, array)
		else.if op=Keyword_if then
			getone(&op)
			paren_exp()
			stmt(&op)
			local s2=(,)
			if op=Keyword_else then
				getone(&op)
				stmt(&op)
				read s2
			end if
			shift 2
			Push ("If",array ,("If",array,s2))
		else.if op=Keyword_putc then
			getone(&op)
			paren_exp()
			Push ("Prtc",array,t)
			expected(Semicolon)
			getone(&op)
		else.if op=LeftBrace then
			Brace()
		else
			error "Unkown Op"
		end if
	end sub
	Sub Brace()
			getone(&op)
			while op<>RightBrace and op<>End_of_input
				stmt(&op)
				t=("Sequence", t, array)
			end while
			expected(RightBrace)
			getone(&op)
			push t
	End Sub
	Sub expected(what)
		if not op=what then {Error "Expected "+eval$(what)+str$(LineNo)+","+Str$(ColumnNo)}
	End Sub
	sub getone(&op)
		op=End_of_input
		while cur<lim
		cur++
		line$=trim$(lex$(cur))
		if line$<>"" then exit
		end while
		if cur=lim then exit sub
		LineNo=Val(line$,"int",m)
		line$=mid$(line$, m)
		ColumnNo=Val(line$,"int",m)
		line$=trim$(mid$(line$, m))
		Rem : Print LineNo, ColumnNo
		m=instr(line$," ")
		if m>0 then op=Eval("."+leftpart$(line$, " ")) else op=Eval("."+line$)
	end sub
}

syntax_analyzer {
         1         1 LeftBrace
         5         5 Identifier left_edge
         5        17 Op_assign
         5        19 Op_subtract
         5        20 Integer 420
         5        23 Semicolon
         6         5 Identifier right_edge
         6        17 Op_assign
         6        20 Integer 300
         6        23 Semicolon
         7         5 Identifier top_edge
         7        17 Op_assign
         7        20 Integer 300
         7        23 Semicolon
         8         5 Identifier bottom_edge
         8        17 Op_assign
         8        19 Op_subtract
         8        20 Integer 300
         8        23 Semicolon
         9         5 Identifier x_step
         9        17 Op_assign
         9        22 Integer 7
         9        23 Semicolon
        10         5 Identifier y_step
        10        17 Op_assign
        10        21 Integer 15
        10        23 Semicolon
        12         5 Identifier max_iter
        12        17 Op_assign
        12        20 Integer 200
        12        23 Semicolon
        14         5 Identifier y0
        14         8 Op_assign
        14        10 Identifier top_edge
        14        18 Semicolon
        15         5 Keyword_while
        15        11 LeftParen
        15        12 Identifier y0
        15        15 Op_greater
        15        17 Identifier bottom_edge
        15        28 RightParen
        15        30 LeftBrace
        16         9 Identifier x0
        16        12 Op_assign
        16        14 Identifier left_edge
        16        23 Semicolon
        17         9 Keyword_while
        17        15 LeftParen
        17        16 Identifier x0
        17        19 Op_less
        17        21 Identifier right_edge
        17        31 RightParen
        17        33 LeftBrace
        18        13 Identifier y
        18        15 Op_assign
        18        17 Integer 0
        18        18 Semicolon
        19        13 Identifier x
        19        15 Op_assign
        19        17 Integer 0
        19        18 Semicolon
        20        13 Identifier the_char
        20        22 Op_assign
        20        24 Integer 32
        20        27 Semicolon
        21        13 Identifier i
        21        15 Op_assign
        21        17 Integer 0
        21        18 Semicolon
        22        13 Keyword_while
        22        19 LeftParen
        22        20 Identifier i
        22        22 Op_less
        22        24 Identifier max_iter
        22        32 RightParen
        22        34 LeftBrace
        23        17 Identifier x_x
        23        21 Op_assign
        23        23 LeftParen
        23        24 Identifier x
        23        26 Op_multiply
        23        28 Identifier x
        23        29 RightParen
        23        31 Op_divide
        23        33 Integer 200
        23        36 Semicolon
        24        17 Identifier y_y
        24        21 Op_assign
        24        23 LeftParen
        24        24 Identifier y
        24        26 Op_multiply
        24        28 Identifier y
        24        29 RightParen
        24        31 Op_divide
        24        33 Integer 200
        24        36 Semicolon
        25        17 Keyword_if
        25        20 LeftParen
        25        21 Identifier x_x
        25        25 Op_add
        25        27 Identifier y_y
        25        31 Op_greater
        25        33 Integer 800
        25        37 RightParen
        25        39 LeftBrace
        26        21 Identifier the_char
        26        30 Op_assign
        26        32 Integer 48
        26        36 Op_add
        26        38 Identifier i
        26        39 Semicolon
        27        21 Keyword_if
        27        24 LeftParen
        27        25 Identifier i
        27        27 Op_greater
        27        29 Integer 9
        27        30 RightParen
        27        32 LeftBrace
        28        25 Identifier the_char
        28        34 Op_assign
        28        36 Integer 64
        28        39 Semicolon
        29        21 RightBrace
        30        21 Identifier i
        30        23 Op_assign
        30        25 Identifier max_iter
        30        33 Semicolon
        31        17 RightBrace
        32        17 Identifier y
        32        19 Op_assign
        32        21 Identifier x
        32        23 Op_multiply
        32        25 Identifier y
        32        27 Op_divide
        32        29 Integer 100
        32        33 Op_add
        32        35 Identifier y0
        32        37 Semicolon
        33        17 Identifier x
        33        19 Op_assign
        33        21 Identifier x_x
        33        25 Op_subtract
        33        27 Identifier y_y
        33        31 Op_add
        33        33 Identifier x0
        33        35 Semicolon
        34        17 Identifier i
        34        19 Op_assign
        34        21 Identifier i
        34        23 Op_add
        34        25 Integer 1
        34        26 Semicolon
        35        13 RightBrace
        36        13 Keyword_putc
        36        17 LeftParen
        36        18 Identifier the_char
        36        26 RightParen
        36        27 Semicolon
        37        13 Identifier x0
        37        16 Op_assign
        37        18 Identifier x0
        37        21 Op_add
        37        23 Identifier x_step
        37        29 Semicolon
        38         9 RightBrace
        39         9 Keyword_putc
        39        13 LeftParen
        39        14 Integer 10
        39        18 RightParen
        39        19 Semicolon
        40         9 Identifier y0
        40        12 Op_assign
        40        14 Identifier y0
        40        17 Op_subtract
        40        19 Identifier y_step
        40        25 Semicolon
        41         5 RightBrace
        42         1 RightBrace
        43         1 End_of_Input
}

```

```txt
Sequence
;
Sequence
Sequence
Sequence
Sequence
Sequence
Sequence
Sequence
Sequence
Sequence
;
Assign
Identifier left_edge
Negate
Integer 420
;
Assign
Identifier right_edge
Integer 300
Assign
Identifier top_edge
Integer 300
Assign
Identifier bottom_edge
Negate
Integer 300
;
Assign
Identifier x_step
Integer 7
Assign
Identifier y_step
Integer 15
Assign
Identifier max_iter
Integer 200
Assign
Identifier y0
Identifier top_edge
While
Greater
Identifier y0
Identifier bottom_edge
Sequence
Sequence
Sequence
Sequence
;
Assign
Identifier x0
Identifier left_edge
While
Less
Identifier x0
Identifier right_edge
Sequence
Sequence
Sequence
Sequence
Sequence
Sequence
Sequence
;
Assign
Identifier y
Integer 0
Assign
Identifier x
Integer 0
Assign
Identifier the_char
Integer 32
Assign
Identifier i
Integer 0
While
Less
Identifier i
Identifier max_iter
Sequence
Sequence
Sequence
Sequence
Sequence
Sequence
;
Assign
Identifier x_x
Divide
Multiply
Identifier x
Identifier x
Integer 200
Assign
Identifier y_y
Divide
Multiply
Identifier y
Identifier y
Integer 200
If
Greater
Add
Identifier x_x
Identifier y_y
Integer 800
if
Sequence
Sequence
Sequence
;
Assign
Identifier the_char
Add
Integer 48
Identifier i
If
Greater
Identifier i
Integer 9
if
Sequence
;
Assign
Identifier the_char
Integer 64
;
Assign
Identifier i
Identifier max_iter
;
Assign
Identifier y
Add
Divide
Multiply
Identifier x
Identifier y
Integer 100
Identifier y0
Assign
Identifier x
Add
Subtract
Identifier x_x
Identifier y_y
Identifier x0
Assign
Identifier i
Add
Identifier i
Integer 1
Putc
Identifier the_char
;
Assign
Identifier x0
Add
Identifier x0
Identifier x_step
Putc
Integer 10
;
Assign
Identifier y0
Subtract
Identifier y0
Identifier y_step

</pre >


## Perl

Tested on perl v5.26.1

```Perl
#!/usr/bin/perl

use strict;   # parse.pl - inputs lex, outputs flattened ast
use warnings; # http://www.rosettacode.org/wiki/Compiler/syntax_analyzer

my $h = qr/\G\s*\d+\s+\d+\s+/;  # header of each line

sub error { die "*** Expected @_ at " . (/\G(.*\n)/ ?
  $1 =~ s/^\s*(\d+)\s+(\d+)\s+/line $1 character $2 got /r : "EOF\n") }

sub want { /$h \Q$_[1]\E.*\n/gcx ? shift : error "'$_[1]'" }

local $_ = join '', <>;
print want stmtlist(), 'End_of_input';

sub stmtlist
  {
  /(?=$h (RightBrace|End_of_input))/gcx and return ";\n";
  my ($stmt, $stmtlist) = (stmt(), stmtlist());
  $stmtlist eq ";\n" ? $stmt : "Sequence\n$stmt$stmtlist";
  }

sub stmt
  {
  /$h Semicolon\n/gcx ? ";\n" :
    /$h Identifier \s+ (\w+) \n/gcx ? want("Assign\nIdentifier\t$1\n",
      'Op_assign') . want expr(0), 'Semicolon' :
    /$h Keyword_while \n/gcx ? "While\n" . parenexp() . stmt() :
    /$h Keyword_if \n/gcx ?  "If\n" . parenexp() . "If\n" . stmt() .
      (/$h Keyword_else \n/gcx ? stmt() : ";\n") :
    /$h Keyword_print \n/gcx ? want('', 'LeftParen') .
      want want(printlist(), 'RightParen'), 'Semicolon' :
    /$h Keyword_putc \n/gcx ? want "Prtc\n" . parenexp() . ";\n", 'Semicolon' :
    /$h LeftBrace \n/gcx ? want stmtlist(), 'RightBrace' :
    error 'A STMT';
  }

sub parenexp { want('', 'LeftParen') . want expr(0), 'RightParen' } # (expr)

sub printlist
  {
  my $ast = /$h String \s+ (".*") \n/gcx ?
    "Prts\nString\t\t$1\n;\n" : "Prti\n" . expr(0) . ";\n";
  /$h Comma \n/gcx ? "Sequence\n$ast" . printlist() : $ast;
  }

sub expr               # (sort of EBNF) expr = operand { operator expr }
  {
  my $ast =                                        # operand
    /$h Integer \s+ (\d+) \n/gcx ? "Integer\t\t$1\n" :
    /$h Identifier \s+ (\w+) \n/gcx ? "Identifier\t$1\n" :
    /$h LeftParen \n/gcx ? want expr(0), 'RightParen' :
    /$h Op_(negate|subtract) \n/gcx ? "Negate\n" . expr(8) . ";\n" :
    /$h Op_not \n/gcx ? "Not\n" . expr(8) . ";\n" :
    /$h Op_add \n/gcx ? expr(8) :
    error "A PRIMARY";
  $ast =                                           # { operator expr }
    $_[0] <= 7 && /$h Op_multiply \n/gcx ? "Multiply\n$ast" . expr(8) :
    $_[0] <= 7 && /$h Op_divide \n/gcx ? "Divide\n$ast" . expr(8) :
    $_[0] <= 7 && /$h Op_mod \n/gcx ? "Mod\n$ast" . expr(8) :
    $_[0] <= 6 && /$h Op_add \n/gcx ? "Add\n$ast" . expr(7) :
    $_[0] <= 6 && /$h Op_subtract \n/gcx ? "Subtract\n$ast" . expr(7) :
    $_[0] == 5 && /(?=$h Op_(less|greater)(equal)? \n)/gcx ? error 'NO ASSOC' :
    $_[0] <= 5 && /$h Op_lessequal \n/gcx ? "LessEqual\n$ast" . expr(5) :
    $_[0] <= 5 && /$h Op_less \n/gcx ? "Less\n$ast" . expr(5) :
    $_[0] <= 5 && /$h Op_greater \n/gcx ? "Greater\n$ast" . expr(5) :
    $_[0] <= 5 && /$h Op_greaterequal \n/gcx ?  "GreaterEqual\n$ast" . expr(5) :
    $_[0] == 3 && /(?=$h Op_(not)?equal \n)/gcx ? error 'NO ASSOC' :
    $_[0] <= 3 && /$h Op_equal \n/gcx ? "Equal\n$ast" . expr(3) :
    $_[0] <= 3 && /$h Op_notequal \n/gcx ? "NotEqual\n$ast" . expr(3) :
    $_[0] <= 1 && /$h Op_and \n/gcx ? "And\n$ast" . expr(2) :
    $_[0] <= 0 && /$h Op_or \n/gcx ? "Or\n$ast" . expr(1) :
    return $ast while 1;
  }
```


<b>

```txt
Sequence
Assign
Identifier      count
Integer         1
While
Less
Identifier      count
Integer         10
Sequence
Sequence
Prts
String          "count is: "
;
Sequence
Prti
Identifier      count
;
Prts
String          "\n"
;
Assign
Identifier      count
Add
Identifier      count
Integer         1

```

</b>


## Phix

Reusing lex.e (and core.e) from the [[Compiler/lexical_analyzer#Phix|Lexical Analyzer task]], and again written as a reusable module.

```Phix
--
-- demo\\rosetta\\Compiler\\parse.e
--
### ==========================

--
--  The reusable part of parse.exw
--

include lex.e

sequence tok

procedure errd(sequence msg, sequence args={})
    {tok_line,tok_col} = tok
    error(msg,args)
end procedure

global sequence toks
integer next_tok = 1

function get_tok()
    sequence tok = toks[next_tok]
    next_tok += 1
    return tok
end function

procedure expect(string msg, integer s)
integer tk = tok[3]
    if tk!=s then
        errd("%s: Expecting '%s', found '%s'\n", {msg, tkNames[s], tkNames[tk]})
    end if
    tok = get_tok()
end procedure

function expr(integer p)
object x = NULL, node
integer op = tok[3]

    switch op do
        case tk_LeftParen:
            tok = get_tok()
            x = expr(0)
            expect("expr",tk_RightParen)
        case tk_sub:
        case tk_add:
            tok = get_tok()
            node = expr(precedences[tk_neg]);
            x = iff(op==tk_sub?{tk_neg, node, NULL}:node)
        case tk_not:
            tok = get_tok();
            x = {tk_not, expr(precedences[tk_not]), NULL}
        case tk_Identifier:
            x = {tk_Identifier, tok[4]}
            tok = get_tok();
        case tk_Integer:
            x = {tk_Integer, tok[4]}
            tok = get_tok();
        default:
            errd("Expecting a primary, found: %s\n", tkNames[op])
    end switch

    op = tok[3]
    while narys[op]=BINARY
      and precedences[op]>=p do
        tok = get_tok()
        x = {op, x, expr(precedences[op]+1)}
        op = tok[3]
    end while
    return x;
end function

function paren_expr(string msg)
    expect(msg, tk_LeftParen);
    object t = expr(0)
    expect(msg, tk_RightParen);
    return t
end function

function stmt()
object t = NULL, e, s

    switch tok[3] do
        case tk_if:
            tok = get_tok();
            object condition = paren_expr("If-cond");
            object ifblock = stmt();
            object elseblock = NULL;
            if tok[3] == tk_else then
                tok = get_tok();
                elseblock = stmt();
            end if
            t = {tk_if, condition, {tk_if, ifblock, elseblock}}
        case tk_putc:
            tok = get_tok();
            e = paren_expr("Prtc")
            t = {tk_putc, e, NULL}
            expect("Putc", tk_Semicolon);
        case tk_print:
            tok = get_tok();
            expect("Print",tk_LeftParen)
            while 1 do
                if tok[3] == tk_String then
                    e = {tk_Prints, {tk_String, tok[4]}, NULL}
                    tok = get_tok();
                else
                    e = {tk_Printi, expr(0), NULL}
                end if
                t = {tk_Sequence, t, e}
                if tok[3]!=tk_Comma then exit end if
                expect("Print", tk_Comma)
            end while
            expect("Print", tk_RightParen);
            expect("Print", tk_Semicolon);
        case tk_Semicolon:
            tok = get_tok();
        case tk_Identifier:
            object v
            v = {tk_Identifier, tok[4]}
            tok = get_tok();
            expect("assign", tk_assign);
            e = expr(0);
            t = {tk_assign, v, e}
            expect("assign", tk_Semicolon);
        case tk_while:
            tok = get_tok();
            e = paren_expr("while");
            s = stmt();
            t = {tk_while, e, s}
        case tk_LeftBrace:      /* {stmt} */
            expect("LeftBrace", tk_LeftBrace)
            while not find(tok[3],{tk_RightBrace,tk_EOI}) do
                t = {tk_Sequence, t, stmt()}
            end while
            expect("LeftBrace", tk_RightBrace);
            break;
        case tk_EOI:
            break;
        default:
            errd("expecting start of statement, found '%s'\n", tkNames[tok[3]]);
    end switch
    return t
end function

global function parse()
object t = NULL
    tok = get_tok()
    while 1 do
        object s = stmt()
        if s=NULL then exit end if
        t = {tk_Sequence, t, s}
    end while
    return t
end function
```

And a simple test driver for the specific task:

```Phix
--
-- demo\\rosetta\\Compiler\\parse.exw
--
### ============================

--
include parse.e

procedure print_ast(object t)
    if t == NULL then
        printf(output_file,";\n")
    else
        integer ttype = t[1]
        printf(output_file,tkNames[ttype])
        if ttype=tk_Identifier then
            printf(output_file," %s\n",t[2])
        elsif ttype=tk_Integer then
            printf(output_file," %d\n",t[2])
        elsif ttype=tk_String then
            printf(output_file," %s\n",enquote(t[2]))
        else
            printf(output_file,"\n")
            print_ast(t[2])
            print_ast(t[3])
        end if
    end if
end procedure

procedure main(sequence cl)
    open_files(cl)
    toks = lex()
    object t = parse()
    print_ast(t)
    close_files()
end procedure

--main(command_line())
main({0,0,"test3.c"})       -- not parseable!
--main({0,0,"primes.c"})    -- as Algol, C, Python (apart from spacing)
--main({0,0,"count.c"})     -- as AWK              (       ""         )
```

```txt

Line 5 column 40:
Print: Expecting 'LeftParen', found 'Op_subtract'

```



## Python

Tested with Python 2.7 and 3.x

```Python
from __future__ import print_function
import sys, shlex, operator

tk_EOI, tk_Mul, tk_Div, tk_Mod, tk_Add, tk_Sub, tk_Negate, tk_Not, tk_Lss, tk_Leq, tk_Gtr, \
tk_Geq, tk_Eql, tk_Neq, tk_Assign, tk_And, tk_Or, tk_If, tk_Else, tk_While, tk_Print,      \
tk_Putc, tk_Lparen, tk_Rparen, tk_Lbrace, tk_Rbrace, tk_Semi, tk_Comma, tk_Ident,          \
tk_Integer, tk_String = range(31)

nd_Ident, nd_String, nd_Integer, nd_Sequence, nd_If, nd_Prtc, nd_Prts, nd_Prti, nd_While, \
nd_Assign, nd_Negate, nd_Not, nd_Mul, nd_Div, nd_Mod, nd_Add, nd_Sub, nd_Lss, nd_Leq,     \
nd_Gtr, nd_Geq, nd_Eql, nd_Neq, nd_And, nd_Or = range(25)

# must have same order as above
Tokens = [
    ["EOI"             , False, False, False, -1, -1        ],
    ["*"               , False, True,  False, 13, nd_Mul    ],
    ["/"               , False, True,  False, 13, nd_Div    ],
    ["%"               , False, True,  False, 13, nd_Mod    ],
    ["+"               , False, True,  False, 12, nd_Add    ],
    ["-"               , False, True,  False, 12, nd_Sub    ],
    ["-"               , False, False, True,  14, nd_Negate ],
    ["!"               , False, False, True,  14, nd_Not    ],
    ["<"               , False, True,  False, 10, nd_Lss    ],
    ["<="              , False, True,  False, 10, nd_Leq    ],
    [">"               , False, True,  False, 10, nd_Gtr    ],
    [">="              , False, True,  False, 10, nd_Geq    ],
    ["=="              , False, True,  False,  9, nd_Eql    ],
    ["!="              , False, True,  False,  9, nd_Neq    ],
    ["="               , False, False, False, -1, nd_Assign ],
    ["&&"              , False, True,  False,  5, nd_And    ],
    ["||"              , False, True,  False,  4, nd_Or     ],
    ["if"              , False, False, False, -1, nd_If     ],
    ["else"            , False, False, False, -1, -1        ],
    ["while"           , False, False, False, -1, nd_While  ],
    ["print"           , False, False, False, -1, -1        ],
    ["putc"            , False, False, False, -1, -1        ],
    ["("               , False, False, False, -1, -1        ],
    [")"               , False, False, False, -1, -1        ],
    ["{"               , False, False, False, -1, -1        ],
    ["}"               , False, False, False, -1, -1        ],
    [";"               , False, False, False, -1, -1        ],
    [","               , False, False, False, -1, -1        ],
    ["Ident"           , False, False, False, -1, nd_Ident  ],
    ["Integer literal" , False, False, False, -1, nd_Integer],
    ["String literal"  , False, False, False, -1, nd_String ]
    ]

all_syms = {"End_of_input"   : tk_EOI,     "Op_multiply"    : tk_Mul,
            "Op_divide"      : tk_Div,     "Op_mod"         : tk_Mod,
            "Op_add"         : tk_Add,     "Op_subtract"    : tk_Sub,
            "Op_negate"      : tk_Negate,  "Op_not"         : tk_Not,
            "Op_less"        : tk_Lss,     "Op_lessequal"   : tk_Leq,
            "Op_greater"     : tk_Gtr,     "Op_greaterequal": tk_Geq,
            "Op_equal"       : tk_Eql,     "Op_notequal"    : tk_Neq,
            "Op_assign"      : tk_Assign,  "Op_and"         : tk_And,
            "Op_or"          : tk_Or,      "Keyword_if"     : tk_If,
            "Keyword_else"   : tk_Else,    "Keyword_while"  : tk_While,
            "Keyword_print"  : tk_Print,   "Keyword_putc"   : tk_Putc,
            "LeftParen"      : tk_Lparen,  "RightParen"     : tk_Rparen,
            "LeftBrace"      : tk_Lbrace,  "RightBrace"     : tk_Rbrace,
            "Semicolon"      : tk_Semi,    "Comma"          : tk_Comma,
            "Identifier"     : tk_Ident,   "Integer"        : tk_Integer,
            "String"         : tk_String}

Display_nodes = ["Identifier", "String", "Integer", "Sequence", "If", "Prtc", "Prts",
    "Prti", "While", "Assign", "Negate", "Not", "Multiply", "Divide", "Mod", "Add",
    "Subtract", "Less", "LessEqual", "Greater", "GreaterEqual", "Equal", "NotEqual",
    "And", "Or"]

TK_NAME         = 0
TK_RIGHT_ASSOC  = 1
TK_IS_BINARY    = 2
TK_IS_UNARY     = 3
TK_PRECEDENCE   = 4
TK_NODE         = 5

input_file = None
err_line   = None
err_col    = None
tok        = None
tok_text   = None

#*** show error and exit
def error(msg):
    print("(%d, %d) %s" % (int(err_line), int(err_col), msg))
    exit(1)

#***
def gettok():
    global err_line, err_col, tok, tok_text, tok_other
    line = input_file.readline()
    if len(line) == 0:
        error("empty line")

    line_list = shlex.split(line, False, False)
    # line col Ident var_name
    # 0    1   2     3

    err_line = line_list[0]
    err_col  = line_list[1]
    tok_text = line_list[2]

    tok = all_syms.get(tok_text)
    if tok == None:
        error("Unknown token %s" % (tok_text))

    tok_other = None
    if tok in [tk_Integer, tk_Ident, tk_String]:
        tok_other = line_list[3]

class Node:
    def __init__(self, node_type, left = None, right = None, value = None):
        self.node_type  = node_type
        self.left  = left
        self.right = right
        self.value = value

#***
def make_node(oper, left, right = None):
    return Node(oper, left, right)

#***
def make_leaf(oper, n):
    return Node(oper, value = n)

#***
def expect(msg, s):
    if tok == s:
        gettok()
        return
    error("%s: Expecting '%s', found '%s'" % (msg, Tokens[s][TK_NAME], Tokens[tok][TK_NAME]))

#***
def expr(p):
    x = None

    if tok == tk_Lparen:
        x = paren_expr()
    elif tok in [tk_Sub, tk_Add]:
        op = (tk_Negate if tok == tk_Sub else tk_Add)
        gettok()
        node = expr(Tokens[tk_Negate][TK_PRECEDENCE])
        x = (make_node(nd_Negate, node) if op == tk_Negate else node)
    elif tok == tk_Not:
        gettok()
        x = make_node(nd_Not, expr(Tokens[tk_Not][TK_PRECEDENCE]))
    elif tok == tk_Ident:
        x = make_leaf(nd_Ident, tok_other)
        gettok()
    elif tok == tk_Integer:
        x = make_leaf(nd_Integer, tok_other)
        gettok()
    else:
        error("Expecting a primary, found: %s" % (Tokens[tok][TK_NAME]))

    while Tokens[tok][TK_IS_BINARY] and Tokens[tok][TK_PRECEDENCE] >= p:
        op = tok
        gettok()
        q = Tokens[op][TK_PRECEDENCE]
        if not Tokens[op][TK_RIGHT_ASSOC]:
            q += 1

        node = expr(q)
        x = make_node(Tokens[op][TK_NODE], x, node)

    return x

#***
def paren_expr():
    expect("paren_expr", tk_Lparen)
    node = expr(0)
    expect("paren_expr", tk_Rparen)
    return node

#***
def stmt():
    t = None

    if tok == tk_If:
        gettok()
        e = paren_expr()
        s = stmt()
        s2 = None
        if tok == tk_Else:
            gettok()
            s2 = stmt()
        t = make_node(nd_If, e, make_node(nd_If, s, s2))
    elif tok == tk_Putc:
        gettok()
        e = paren_expr()
        t = make_node(nd_Prtc, e)
        expect("Putc", tk_Semi)
    elif tok == tk_Print:
        gettok()
        expect("Print", tk_Lparen)
        while True:
            if tok == tk_String:
                e = make_node(nd_Prts, make_leaf(nd_String, tok_other))
                gettok()
            else:
                e = make_node(nd_Prti, expr(0))

            t = make_node(nd_Sequence, t, e)
            if tok != tk_Comma:
                break
            gettok()
        expect("Print", tk_Rparen)
        expect("Print", tk_Semi)
    elif tok == tk_Semi:
        gettok()
    elif tok == tk_Ident:
        v = make_leaf(nd_Ident, tok_other)
        gettok()
        expect("assign", tk_Assign)
        e = expr(0)
        t = make_node(nd_Assign, v, e)
        expect("assign", tk_Semi)
    elif tok == tk_While:
        gettok()
        e = paren_expr()
        s = stmt()
        t = make_node(nd_While, e, s)
    elif tok == tk_Lbrace:
        gettok()
        while tok != tk_Rbrace and tok != tk_EOI:
            t = make_node(nd_Sequence, t, stmt())
        expect("Lbrace", tk_Rbrace)
    elif tok == tk_EOI:
        pass
    else:
        error("Expecting start of statement, found: %s" % (Tokens[tok][TK_NAME]))

    return t

#***
def parse():
    t = None
    gettok()
    while True:
        t = make_node(nd_Sequence, t, stmt())
        if tok == tk_EOI or t == None:
            break
    return t

def prt_ast(t):
    if t == None:
        print(";")
    else:
        print("%-14s" % (Display_nodes[t.node_type]), end='')
        if t.node_type in [nd_Ident, nd_Integer]:
            print("%s" % (t.value))
        elif t.node_type == nd_String:
            print("%s" %(t.value))
        else:
            print("")
            prt_ast(t.left)
            prt_ast(t.right)

#*** main driver
input_file = sys.stdin
if len(sys.argv) > 1:
    try:
        input_file = open(sys.argv[1], "r", 4096)
    except IOError as e:
        error(0, 0, "Can't open %s" % sys.argv[1])
t = parse()
prt_ast(t)
```


<b>

```txt

Sequence
Sequence
Sequence
Sequence
Sequence
;
Assign
Identifier    count
Integer       1
Assign
Identifier    n
Integer       1
Assign
Identifier    limit
Integer       100
While
Less
Identifier    n
Identifier    limit
Sequence
Sequence
Sequence
Sequence
Sequence
;
Assign
Identifier    k
Integer       3
Assign
Identifier    p
Integer       1
Assign
Identifier    n
Add
Identifier    n
Integer       2
While
And
LessEqual
Multiply
Identifier    k
Identifier    k
Identifier    n
Identifier    p
Sequence
Sequence
;
Assign
Identifier    p
NotEqual
Multiply
Divide
Identifier    n
Identifier    k
Identifier    k
Identifier    n
Assign
Identifier    k
Add
Identifier    k
Integer       2
If
Identifier    p
If
Sequence
Sequence
;
Sequence
Sequence
;
Prti
Identifier    n
;
Prts
String        " is prime\n"
;
Assign
Identifier    count
Add
Identifier    count
Integer       1
;
Sequence
Sequence
Sequence
;
Prts
String        "Total primes found: "
;
Prti
Identifier    count
;
Prts
String        "\n"
;

```

</b>


## Scheme


Code implements a recursive descent parser based on the given grammar.  Tested against all programs in [[Compiler/Sample programs]].


```scheme

(import (scheme base)
        (scheme process-context)
        (scheme write))

(define *names* (list (cons 'Op_add 'Add)
                      (cons 'Op_subtract 'Subtract)
                      (cons 'Op_multiply 'Multiply)
                      (cons 'Op_divide 'Divide)
                      (cons 'Op_mod 'Mod)
                      (cons 'Op_not 'Not)
                      (cons 'Op_equal 'Equal)
                      (cons 'Op_notequal 'NotEqual)
                      (cons 'Op_or 'Or)
                      (cons 'Op_and 'And)
                      (cons 'Op_less 'Less)
                      (cons 'Op_lessequal 'LessEqual)
                      (cons 'Op_greater 'Greater)
                      (cons 'Op_greaterequal 'GreaterEqual)))

(define (retrieve-name type)
  (let ((res (assq type *names*)))
    (if res
      (cdr res)
      (error "Unknown type name"))))

;; takes a vector of tokens
(define (parse tokens) ; read statements, until hit end of tokens
  (define posn 0)
  (define (peek-token)
    (vector-ref tokens posn))
  (define (get-token)
    (set! posn (+ 1 posn))
    (vector-ref tokens (- posn 1)))
  (define (match type)
    (if (eq? (car (vector-ref tokens posn)) type)
      (set! posn (+ 1 posn))
      (error "Could not match token type" type)))
  ; make it easier to read token parts
  (define type car)
  (define value cadr)
  ;
  ;; left associative read of one or more items with given separators
  (define (read-one-or-more reader separators)
    (let loop ((lft (reader)))
      (let ((next (peek-token)))
        (if (memq (type next) separators)
          (begin (match (type next))
                 (loop (list (retrieve-name (type next)) lft (reader))))
          lft))))
  ;
  ;; read one or two items with given separator
  (define (read-one-or-two reader separators)
    (let* ((lft (reader))
           (next (peek-token)))
      (if (memq (type next) separators)
        (begin (match (type next))
               (list (retrieve-name (type next)) lft (reader)))
        lft)))
  ;
  (define (read-primary)
    (let ((next (get-token)))
      (case (type next)
        ((Identifier Integer)
         next)
        ((LeftParen)
         (let ((v (read-expr)))
           (match 'RightParen)
           v))
        ((Op_add) ; + sign is ignored
         (read-primary))
        ((Op_not)
         (list 'Not (read-primary) '()))
        ((Op_subtract)
         (list 'Negate (read-primary) '()))
        (else
          (error "Unknown primary type")))))
  ;
  (define (read-multiplication-expr) ; *
    (read-one-or-more read-primary '(Op_multiply Op_divide Op_mod)))
  ;
  (define (read-addition-expr) ; *
    (read-one-or-more read-multiplication-expr '(Op_add Op_subtract)))
  ;
  (define (read-relational-expr) ; ?
    (read-one-or-two read-addition-expr
                     '(Op_less Op_lessequal Op_greater Op_greaterequal)))
  ;
  (define (read-equality-expr) ; ?
    (read-one-or-two read-relational-expr '(Op_equal Op_notequal)))
  ;
  (define (read-and-expr) ; *
    (read-one-or-more read-equality-expr '(Op_and)))
  ;
  (define (read-expr) ; *
    (read-one-or-more read-and-expr '(Op_or)))
  ;
  (define (read-prt-list)
    (define (read-print-part)
      (if (eq? (type (peek-token)) 'String)
        (list 'Prts (get-token) '())
        (list 'Prti (read-expr) '())))
    ;
    (do ((tok (read-print-part) (read-print-part))
         (rec '() (list 'Sequence rec tok)))
      ((not (eq? (type (peek-token)) 'Comma))
       (list 'Sequence rec tok))
      (match 'Comma)))
  ;
  (define (read-paren-expr)
    (match 'LeftParen)
    (let ((v (read-expr)))
      (match 'RightParen)
      v))
  ;
  (define (read-stmt)
    (case (type (peek-token))
      ((SemiColon)
       '())
      ((Identifier)
       (let ((id (get-token)))
         (match 'Op_assign)
         (let ((ex (read-expr)))
           (match 'Semicolon)
           (list 'Assign id ex))))
      ((Keyword_while)
       (match 'Keyword_while)
       (let* ((expr (read-paren-expr))
              (stmt (read-stmt)))
         (list 'While expr stmt)))
      ((Keyword_if)
       (match 'Keyword_if)
       (let* ((expr (read-paren-expr))
              (then-part (read-stmt))
              (else-part (if (eq? (type (peek-token)) 'Keyword_else)
                           (begin (match 'Keyword_else)
                                  (read-stmt))
                           '())))
         (list 'If expr (list 'If then-part else-part))))
      ((Keyword_print)
       (match 'Keyword_print)
       (match 'LeftParen)
       (let ((v (read-prt-list)))
         (match 'RightParen)
         (match 'Semicolon)
         v))
      ((Keyword_putc)
       (match 'Keyword_putc)
       (let ((v (read-paren-expr)))
         (match 'Semicolon)
         (list 'Putc v '())))
      ((LeftBrace)
       (match 'LeftBrace)
       (let ((v (read-stmts)))
         (match 'RightBrace)
         v))
      (else
        (error "Unknown token type for statement" (type (peek-token))))))
  ;
  (define (read-stmts)
    (do ((sequence (list 'Sequence '() (read-stmt))
                   (list 'Sequence sequence (read-stmt))))
      ((memq (type (peek-token)) '(End_of_input RightBrace))
       sequence)))
  ;
  (let ((res (read-stmts)))
    (match 'End_of_input)
    res))

;; reads tokens from file, parses and returns the AST
(define (parse-file filename)
  (define (tokenise line)
    (let ((port (open-input-string line)))
      (read port) ; discard line
      (read port) ; discard col
      (let* ((type (read port)) ; read type as symbol
             (val (read port))) ; check for optional value
        (if (eof-object? val)
          (list type)
          (list type val)))))
  ;
  (with-input-from-file
    filename
    (lambda ()
      (do ((line (read-line) (read-line))
           (toks '() (cons (tokenise line) toks)))
        ((eof-object? line)
         (parse (list->vector (reverse toks))))))))

;; Output the AST in flattened format
(define (display-ast ast)
  (cond ((null? ast)
         (display ";\n"))
        ((= 2 (length ast))
         (display (car ast))
         (display #\tab)
         (write (cadr ast)) ; use write to preserve " " on String
         (newline))
        (else
          (display (car ast)) (newline)
          (display-ast (cadr ast))
          (display-ast (cadr (cdr ast))))))

;; read from filename passed on command line
(if (= 2 (length (command-line)))
  (display-ast (parse-file (cadr (command-line))))
  (display "Error: provide program filename\n"))

```

