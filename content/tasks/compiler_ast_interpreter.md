+++
title = "Compiler/AST interpreter"
description = ""
date = 2019-05-18T14:05:10Z
aliases = []
[extra]
id = 21167
[taxonomies]
categories = ["task"]
tags = []
languages = [
  "algol_w",
  "c",
  "cobol",
  "forth",
  "go",
  "java",
  "julia",
  "perl",
  "phix",
  "python",
  "scheme",
  "zkl",
]
+++

## Task

{{task}}AST interpreter

An AST interpreter interprets an [https://en.wikipedia.org/wiki/Abstract_syntax_tree Abstract Syntax Tree (AST)]
produced by a [[Compiler/syntax_analyzer|Syntax Analyzer]].

Take the AST output from the Syntax analyzer [[Compiler/syntax_analyzer|task]], and interpret it as appropriate.
Refer to the [[Compiler/syntax_analyzer|Syntax analyzer task]] for details of the AST.

;Loading the AST from the syntax analyzer is as simple as (pseudo code):


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


; The interpreter algorithm is relatively simple:


```python
interp(x)
    if x == NULL return NULL
    elif x.node_type == Integer return x.value converted to an integer
    elif x.node_type == Ident   return the current value of variable x.value
    elif x.node_type == String  return x.value
    elif x.node_type == Assign
                    globals[x.left.value] = interp(x.right)
                    return NULL
    elif x.node_type is a binary operator return interp(x.left) operator interp(x.right)
    elif x.node_type is a unary operator, return return operator interp(x.left)
    elif x.node_type ==  If
                    if (interp(x.left)) then interp(x.right.left)
                    else interp(x.right.right)
                    return NULL
    elif x.node_type == While
                    while (interp(x.left)) do interp(x.right)
                    return NULL
    elif x.node_type == Prtc
                    print interp(x.left) as a character, no newline
                    return NULL
    elif x.node_type == Prti
                    print interp(x.left) as an integer, no newline
                    return NULL
    elif x.node_type == Prts
                    print interp(x.left) as a string, respecting newlines ("\n")
                    return NULL
    elif x.node_type == Sequence
                    interp(x.left)
                    interp(x.right)
                    return NULL
    else
        error("unknown node type")
```


Notes:

Because of the simple nature of our tiny language, Semantic analysis is not needed.

Your interpreter should use C like division semantics, for both division and modulus.  For division of positive operands, only the non-fractional portion of the result should be returned.  In other words, the result should be truncated towards 0.

This means, for instance, that 3 / 2 should result in 1.

For division when one of the operands is negative, the result should be truncated towards 0.

This means, for instance, that 3 / -2 should result in -1.

; Test program

{| class="wikitable"
|-
! prime.t
! lex &lt;prime.t  	&#124; parse &#124; interp
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

3 is prime
5 is prime
7 is prime
11 is prime
13 is prime
17 is prime
19 is prime
23 is prime
29 is prime
31 is prime
37 is prime
41 is prime
43 is prime
47 is prime
53 is prime
59 is prime
61 is prime
67 is prime
71 is prime
73 is prime
79 is prime
83 is prime
89 is prime
97 is prime
101 is prime
Total primes found: 26

```
</b>

|}

; Additional examples

Your solution should pass all the test cases above and the additional tests found '''[[Compiler/Sample_programs|Here]]'''.

The C and Python versions can be considered reference implementations.

;Related Tasks

* [[Compiler/lexical_analyzer|Lexical Analyzer task]]
* [[Compiler/syntax_analyzer|Syntax Analyzer task]]
* [[Compiler/code_generator|Code Generator task]]
* [[Compiler/virtual_machine_interpreter|Virtual Machine Interpreter task]]

<hr>
__TOC__


## ALGOL W


```algolw
begin % AST interpreter %
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
    integer    MAX_NODE_TYPE;
    % string literals and identifiers - uses a linked list - a hash table might be better... %
    string(1)   array text ( 0 :: 4095 );
    integer     textNext, TEXT_MAX;
    record textElement ( integer start, length; reference(textElement) next );
    reference(textElement) idList, stList;
    % memory - identifiers hold indexes to locations here %
    integer array data ( 1 :: 4096 );

    % returns a new node with left and right branches %
    reference(node) procedure opNode ( integer value opType; reference(node) value opLeft, opRight ) ; begin
        node( opType, opLeft, opRight, 0 )
    end opNode ;

    % returns a new operand node %
    reference(node) procedure operandNode ( integer value opType, opValue ) ; begin
        node( opType, null, null, opValue )
    end operandNode ;

    % reports an error and stops %
    procedure rtError( string(80) value message ); begin
        integer errorPos;
        write( s_w := 0, "**** Runtime error " );
        errorPos := 0;
        while errorPos < 80 and message( errorPos // 1 ) not = "." do begin
            writeon( s_w := 0, message( errorPos // 1 ) );
            errorPos := errorPos + 1
        end while_not_at_end_of_message ;
        writeon( s_w := 0, "." );
        assert( false )
    end rtError ;

    % reads a node from standard input %
    reference(node) procedure readNode ; begin
        reference(node) resultNode;

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
            if lPos > 255 then rtError( "Unterminated String in node file." );
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
                if textNext + sLen > TEXT_MAX then rtError( "Text space exhausted." )
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
            n := 0;
            while line( lPos // 1 ) not = " " do begin
                n    := ( n * 10 ) + ( decode( line( lPos // 1 ) ) - decode( "0" ) );
                lPos := lPos + 1
            end while_not_end_of_integer ;
            n
        end readInteger ;

        string(256) line;
        string(16)  name;
        integer     lPos, tPos, ndType;
        tPos := lPos := 0;
        readcard( line );
        % get the node type name %
        while line( lPos // 1 ) = " " do lPos := lPos + 1;
        name := "";
        while lPos < 256 and line( lPos // 1 ) not = " " do begin
            name( tPos // 1 ) := line( lPos // 1 );
            lPos := lPos + 1;
            tPos := tPos + 1
        end  while_more_name ;
        % determine the node type %
        ndType         := 1;
        resultNode     := null;
        if name not = ";" then begin
            % not a null node %
            while ndType <= MAX_NODE_TYPE and name not = ndName( ndType ) do ndType := ndType + 1;
            if ndType > MAX_NODE_TYPE then rtError( "Malformed node." );
            % handle the additional parameter for identifier/string/integer, or sub-nodes for operator nodes %
            if ndType = nInteger or ndType = nIdentifier or ndType = nString then begin
                while line( lPos // 1 ) = " " do lPos := lPos + 1;
                if      ndType = nInteger    then resultNode := operandNode( ndType, readInteger )
                else if ndType = nIdentifier then resultNode := operandNode( ndType, readString( idList, " "  ) )
                else  % ndType = nString     %    resultNode := operandNode( ndType, readString( stList, """" ) )
                end
            else begin
                % operator node %
                reference(node) leftNode;
                leftNode   := readNode;
                resultNode := opNode( ndType, leftNode, readNode )
            end
        end if_non_null_node ;
        resultNode
    end readNode ;

    % interprets the specified node and returns the value %
    integer procedure eval ( reference(node) value n ) ; begin
        integer v;

        % prints a string from text, escape sequences are interpreted %
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
            if txPos = null then rtError( "INTERNAL ERROR: text not found." )
            else begin
                % found the text - output it, handling escape sequences %
                integer cPos;
                cPos := 1; % start from 1 to skip over the leading " %
                while cPos < length(txPos) do begin
                    string(1) ch;
                    ch := text( start(txPos) + cPos );
                    if ch not = "\" then writeon( s_w := 0, ch )
                    else begin
                        % escaped character %
                        cPos := cPos + 1;
                        if      cPos > length(txPos) then rtError( "String terminates with ""\""." )
                        else begin
                            ch := text( start(txPos) + cPos );
                            if ch = "n" then % newline % write()
                                        else writeon( s_w := 0, ch )
                        end
                    end;
                    cPos := cPos + 1
                end while_not_end_of_string
            end
        end writeOnText ;

        % returns 1 if val is true, 0 otherwise %
        integer procedure booleanResult ( logical value val ) ; begin
            if val then 1 else 0
        end booleanResult ;

        v := 0;

        if      n = null                 then v := 0
        else if type(n) = nIdentifier    then v := data( iValue(n) )
        else if type(n) = nString        then v := iValue(n)
        else if type(n) = nInteger       then v := iValue(n)
        else if type(n) = nSequence      then begin
            % sequence - evaluate and discard the left branch and return the right branch %
            v := eval(  left(n) );
            v := eval( right(n) )
            end
        else if type(n) = nIf            then % if-else         % begin
            if eval( left(n) ) not = 0 then v := eval(  left(right(n)) )
                                       else v := eval( right(right(n)) );
            v := 0
            end
        else if type(n) = nPrtc          then % print character % writeon( s_w := 0, code( eval( left(n) ) ) )
        else if type(n) = nPrts          then % print string    % writeOnText( stList, eval( left(n) ) )
        else if type(n) = nPrti          then % print integer   % writeon( s_w := 0, i_w := 1, eval( left(n) ) )
        else if type(n) = nWhile         then % while-loop      % begin
            while eval( left(n) ) not = 0 do v := eval( right(n) );
            v := 0
            end
        else if type(n) = nAssign        then % assignment      % data( iValue(left(n)) ) := eval( right(n) )
        else if type(n) = nNegate        then % unary -         % v := - eval( left(n) )
        else if type(n) = nNot           then % unary not       % v := booleanResult( eval( left(n) ) = 0 )
        else if type(n) = nMultiply      then % multiply        % v := eval( left(n) ) * eval( right(n) )
        else if type(n) = nDivide        then % division        % begin
            integer lv, rv;
            lv := eval(  left(n) );
            rv := eval( right(n) );
            if rv = 0 then rtError( "Division by 0." )
            else v := lv div rv
            end
        else if type(n) = nMod           then % modulo          % begin
            integer lv, rv;
            lv := eval(  left(n) );
            rv := eval( right(n) );
            if rv = 0 then rtError( "Right operand of % is 0." )
            else v := lv rem rv
            end
        else if type(n) = nAdd           then % addition        % v := eval( left(n) ) + eval( right(n) )
        else if type(n) = nSubtract      then % subtraction     % v := eval( left(n) ) - eval( right(n) )
        else if type(n) = nLess          then % less-than       % v := booleanResult( eval( left(n) ) <     eval( right(n) ) )
        else if type(n) = nLessEqual     then % less or equal   % v := booleanResult( eval( left(n) ) <=    eval( right(n) ) )
        else if type(n) = nGreater       then % greater-than    % v := booleanResult( eval( left(n) ) >     eval( right(n) ) )
        else if type(n) = nGreaterEqual  then % greater or eq   % v := booleanResult( eval( left(n) ) >=    eval( right(n) ) )
        else if type(n) = nEqual         then % test equal      % v := booleanResult( eval( left(n) ) =     eval( right(n) ) )
        else if type(n) = nNotEqual      then % not-equal       % v := booleanResult( eval( left(n) ) not = eval( right(n) ) )
        else if type(n) = nAnd           then % boolean "and"   % begin
            v := eval( left(n) );
            if v not = 0 then v := eval( right(n) )
            end
        else if type(n) = nOr            then % boolean "or"    % begin
            v := eval( left(n) );
            if v = 0 then v := eval( right(n) );
            end
        else % unknown node % begin
            rtError( "Unknown node type in eval." )
        end;
        v
    end eval ;

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
    MAX_NODE_TYPE    := 25; TEXT_MAX := 4095; textNext := 0;
    stList := idList := null;

    % parse the output from the syntax analyser and intetrpret parse tree %
    eval( readNode )
end.
```

```txt

3 is prime
5 is prime
7 is prime
11 is prime
...
83 is prime
89 is prime
97 is prime
101 is prime
Total primes found: 26

```



## C

Tested with gcc 4.81 and later, compiles warning free with -Wall -Wextra

```cpp
#include <iostream>
#include <stdio.h>
#include <string.h>
#include <stdarg.h>
#include <ctype.h>

#define da_dim(name, type)  type *name = NULL;          \
                            int _qy_ ## name ## _p = 0;  \
                            int _qy_ ## name ## _max = 0
#define da_rewind(name)     _qy_ ## name ## _p = 0
#define da_redim(name)      do {if (_qy_ ## name ## _p >= _qy_ ## name ## _max) \
                                name = realloc(name, (_qy_ ## name ## _max += 32) * sizeof(name[0]));} while (0)
#define da_append(name, x)  do {da_redim(name); name[_qy_ ## name ## _p++] = x;} while (0)
#define da_len(name)        _qy_ ## name ## _p
#define da_add(name)        do {da_redim(name); _qy_ ## name ## _p++;} while (0)

typedef enum {
    nd_Ident, nd_String, nd_Integer, nd_Sequence, nd_If, nd_Prtc, nd_Prts, nd_Prti, nd_While,
    nd_Assign, nd_Negate, nd_Not, nd_Mul, nd_Div, nd_Mod, nd_Add, nd_Sub, nd_Lss, nd_Leq,
    nd_Gtr, nd_Geq, nd_Eql, nd_Neq, nd_And, nd_Or
} NodeType;

typedef struct Tree Tree;
struct Tree {
    NodeType node_type;
    Tree *left;
    Tree *right;
    int value;
};

// dependency: Ordered by NodeType, must remain in same order as NodeType enum

struct {
    char       *enum_text;
    NodeType   node_type;
} atr[] = {
    {"Identifier"  , nd_Ident,  },  {"String"      , nd_String,  },
    {"Integer"     , nd_Integer,},  {"Sequence"    , nd_Sequence,},
    {"If"          , nd_If,     },  {"Prtc"        , nd_Prtc,    },
    {"Prts"        , nd_Prts,   },  {"Prti"        , nd_Prti,    },
    {"While"       , nd_While,  },  {"Assign"      , nd_Assign,  },
    {"Negate"      , nd_Negate, },  {"Not"         , nd_Not,     },
    {"Multiply"    , nd_Mul,    },  {"Divide"      , nd_Div,     },
    {"Mod"         , nd_Mod,    },  {"Add"         , nd_Add,     },
    {"Subtract"    , nd_Sub,    },  {"Less"        , nd_Lss,     },
    {"LessEqual"   , nd_Leq,    },  {"Greater"     , nd_Gtr,     },
    {"GreaterEqual", nd_Geq,    },  {"Equal"       , nd_Eql,     },
    {"NotEqual"    , nd_Neq,    },  {"And"         , nd_And,     },
    {"Or"          , nd_Or,     },
};

FILE *source_fp;
da_dim(string_pool, const char *);
da_dim(global_names, const char *);
da_dim(global_values, int);

void error(const char *fmt, ... ) {
    va_list ap;
    char buf[1000];

    va_start(ap, fmt);
    vsprintf(buf, fmt, ap);
    printf("error: %s\n", buf);
    exit(1);
}

Tree *make_node(NodeType node_type, Tree *left, Tree *right) {
    Tree *t = calloc(sizeof(Tree), 1);
    t->node_type = node_type;
    t->left = left;
    t->right = right;
    return t;
}

Tree *make_leaf(NodeType node_type, int value) {
    Tree *t = calloc(sizeof(Tree), 1);
    t->node_type = node_type;
    t->value = value;
    return t;
}

int interp(Tree *x) {           /* interpret the parse tree */
    if (!x) return 0;
    switch(x->node_type) {
        case nd_Integer:  return x->value;
        case nd_Ident:    return global_values[x->value];
        case nd_String:   return x->value;

        case nd_Assign:   return global_values[x->left->value] = interp(x->right);
        case nd_Add:      return interp(x->left) +  interp(x->right);
        case nd_Sub:      return interp(x->left) -  interp(x->right);
        case nd_Mul:      return interp(x->left) *  interp(x->right);
        case nd_Div:      return interp(x->left) /  interp(x->right);
        case nd_Mod:      return interp(x->left) %  interp(x->right);
        case nd_Lss:      return interp(x->left) <  interp(x->right);
        case nd_Gtr:      return interp(x->left) >  interp(x->right);
        case nd_Leq:      return interp(x->left) <= interp(x->right);
        case nd_Eql:      return interp(x->left) == interp(x->right);
        case nd_Neq:      return interp(x->left) != interp(x->right);
        case nd_And:      return interp(x->left) && interp(x->right);
        case nd_Or:       return interp(x->left) || interp(x->right);
        case nd_Negate:   return -interp(x->left);
        case nd_Not:      return !interp(x->left);

        case nd_If:       if (interp(x->left))
                            interp(x->right->left);
                          else
                            interp(x->right->right);
                          return 0;

        case nd_While:    while (interp(x->left))
                            interp(x->right);
                          return 0;

        case nd_Prtc:     printf("%c", interp(x->left));
                          return 0;
        case nd_Prti:     printf("%d", interp(x->left));
                          return 0;
        case nd_Prts:     printf("%s", string_pool[interp(x->left)]);
                          return 0;

        case nd_Sequence: interp(x->left);
                          interp(x->right);
                          return 0;

        default:          error("interp: unknown tree type %d\n", x->node_type);
    }
    return 0;
}

void init_in(const char fn[]) {
    if (fn[0] == '\0')
        source_fp = stdin;
    else {
        source_fp = fopen(fn, "r");
        if (source_fp == NULL)
            error("Can't open %s\n", fn);
    }
}

NodeType get_enum_value(const char name[]) {
    for (size_t i = 0; i < sizeof(atr) / sizeof(atr[0]); i++) {
        if (strcmp(atr[i].enum_text, name) == 0) {
            return atr[i].node_type;
        }
    }
    error("Unknown token %s\n", name);
    return -1;
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

int fetch_string_offset(char *st) {
    int len = strlen(st);
    st[len - 1] = '\0';
    ++st;
    char *p, *q;
    p = q = st;

    while ((*p++ = *q++) != '\0') {
        if (q[-1] == '\\') {
            if (q[0] == 'n') {
                p[-1] = '\n';
                ++q;
            } else if (q[0] == '\\') {
                ++q;
            }
        }
    }

    for (int i = 0; i < da_len(string_pool); ++i) {
        if (strcmp(st, string_pool[i]) == 0) {
            return i;
        }
    }
    da_add(string_pool);
    int n = da_len(string_pool) - 1;
    string_pool[n] = strdup(st);
    return da_len(string_pool) - 1;
}

int fetch_var_offset(const char *name) {
    for (int i = 0; i < da_len(global_names); ++i) {
        if (strcmp(name, global_names[i]) == 0)
            return i;
    }
    da_add(global_names);
    int n = da_len(global_names) - 1;
    global_names[n] = strdup(name);
    da_append(global_values, 0);
    return n;
}

Tree *load_ast() {
    int len;
    char *yytext = read_line(&len);
    yytext = rtrim(yytext, &len);

    // get first token
    char *tok = strtok(yytext, " ");

    if (tok[0] == ';') {
        return NULL;
    }
    NodeType node_type = get_enum_value(tok);

    // if there is extra data, get it
    char *p = tok + strlen(tok);
    if (p != &yytext[len]) {
        int n;
        for (++p; isspace(*p); ++p)
            ;
        switch (node_type) {
            case nd_Ident:      n = fetch_var_offset(p);    break;
            case nd_Integer:    n = strtol(p, NULL, 0);     break;
            case nd_String:     n = fetch_string_offset(p); break;
            default:            error("Unknown node type: %s\n", p);
        }
        return make_leaf(node_type, n);
    }

    Tree *left  = load_ast();
    Tree *right = load_ast();
    return make_node(node_type, left, right);
}

int main(int argc, char *argv[]) {
    init_in(argc > 1 ? argv[1] : "");

    Tree *x = load_ast();
    interp(x);

    return 0;
}
```


<b>

```txt

lex prime.t | parse | interp
3 is prime
5 is prime
7 is prime
11 is prime
13 is prime
17 is prime
19 is prime
23 is prime
29 is prime
31 is prime
37 is prime
41 is prime
43 is prime
47 is prime
53 is prime
59 is prime
61 is prime
67 is prime
71 is prime
73 is prime
79 is prime
83 is prime
89 is prime
97 is prime
101 is prime
Total primes found: 26

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
program-id. astinterpreter.
environment division.
configuration section.
repository. function all intrinsic.
data division.
working-storage section.
01  program-name pic x(32) value spaces global.
01  input-name pic x(32) value spaces global.
01  input-status pic xx global.

01  ast-record global.
    03  ast-type pic x(14).
    03  ast-value pic x(48).
    03  filler redefines ast-value.
        05  asl-left pic 999.
        05  asl-right pic 999.

01  error-record pic x(64) value spaces global.

01  loadstack global.
    03  l pic 99 value 0.
    03  l-lim pic 99 value 64.
    03  load-entry occurs 64.
        05  l-node pic x(14).
        05  l-left pic 999.
        05  l-right pic 999.
        05  l-link pic 999.

01  abstract-syntax-tree global.
    03  t pic 999 value 0.
    03  t1 pic 999.
    03  n1 pic 999.
    03  t-lim pic 999 value 998.
    03  filler occurs 998.
        05  leaf.
            07  leaf-type pic x(14).
            07  leaf-value pic x(48).
        05  node redefines leaf.
            07  node-type pic x(14).
            07  node-left pic 999.
            07  node-right pic 999.


01  interpreterstack global.
    03  stack1 pic 99 value 2.
    03  stack2 pic 99 value 1.
    03  stack-lim pic 99 value 32.
    03  stack-entry occurs 32.
         05  stack-source pic 99.
         05  stack usage binary-int.

01  variables global.
    03  v pic 99.
    03  v-max pic 99 value 0.
    03  v-lim pic 99 value 16.
    03  filler occurs 16.
        05  variable-value binary-int.
        05  variable-name pic x(48).

01  strings global.
    03  s pic 99.
    03  s-max pic 99 value 0.
    03  s-lim pic 99 value 16.
    03  filler occurs 16 value spaces.
        05  string-value pic x(48).

01  string-fields global.
    03  string-length pic 99.
    03  string1 pic 99.
    03  length1 pic 99.
    03  count1 pic 99.

01  display-fields global.
    03  display-number pic -(9)9.
    03  display-pending pic x value 'n'.
    03  character-value.
        05  character-number usage binary-char.

procedure division chaining program-name.
start-astinterpreter.
    call 'loadast'
    if program-name <> spaces
        call 'readinput' *> close the input-file
    end-if
    >>d perform print-ast
    call 'runast' using t
    if display-pending = 'y'
        display space
    end-if
    stop run
    .
print-ast.
    call 'printast' using t
    display 'ast:' upon syserr
    display 't=' t
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
program-id. runast common recursive.
data division.
working-storage section.
01  word-length constant as length of binary-int.
linkage section.
01  n pic 999.
procedure division using n.
start-runast.
    if n = 0
        exit program
    end-if
    evaluate node-type(n)
    when 'Integer'
        perform push-stack
        move numval(leaf-value(n)) to stack(stack1)
    when 'Identifier'
        perform get-variable-index
        perform push-stack
        move v to stack-source(stack1)
        move variable-value(v) to stack(stack1)
    when 'String'
        perform get-string-index
        perform push-stack
        move s to stack-source(stack1)
    when 'Assign'
        call 'runast' using node-left(n)
        call 'runast' using node-right(n)
        move stack-source(stack2) to v
        move stack(stack1) to variable-value(v)
        perform pop-stack
        perform pop-stack
    when 'If'
        call 'runast' using node-left(n)
        move node-right(n) to n1
        if stack(stack1) <> 0
            call 'runast' using node-left(n1)
        else
            call 'runast' using node-right(n1)
        end-if
        perform pop-stack
    when 'While'
        call 'runast' using node-left(n)
        perform until stack(stack1) = 0
            perform pop-stack
            call 'runast' using node-right(n)
            call 'runast' using node-left(n)
        end-perform
        perform pop-stack
    when 'Add'
        perform get-values
        add stack(stack1) to stack(stack2)
        perform pop-stack
    when 'Subtract'
        perform get-values
        subtract stack(stack1) from stack(stack2)
        perform pop-stack
    when 'Multiply'
        perform get-values
        multiply stack(stack1) by stack(stack2)
        perform pop-stack
    when 'Divide'
        perform get-values
        divide stack(stack1) into stack(stack2)
        perform pop-stack
    when 'Mod'
        perform get-values
        move mod(stack(stack2),stack(stack1)) to stack(stack2)
        perform pop-stack
    when 'Less'
        perform get-values
        if stack(stack2) < stack(stack1)
            move 1 to stack(stack2)
        else
            move 0 to stack(stack2)
        end-if
        perform pop-stack
    when 'Greater'
        perform get-values
        if stack(stack2) > stack(stack1)
            move 1 to stack(stack2)
        else
            move 0 to stack(stack2)
        end-if
        perform pop-stack
    when 'LessEqual'
        perform get-values
        if stack(stack2) <= stack(stack1)
            move 1 to stack(stack2)
        else
            move 0 to stack(stack2)
        end-if
        perform pop-stack
    when 'GreaterEqual'
        perform get-values
        if stack(stack2) >= stack(stack1)
            move 1 to stack(stack2)
        else
            move 0 to stack(stack2)
        end-if
        perform pop-stack
    when 'Equal'
        perform get-values
        if stack(stack2) = stack(stack1)
            move 1 to stack(stack2)
        else
            move 0 to stack(stack2)
        end-if
        perform pop-stack
    when 'NotEqual'
        perform get-values
        if stack(stack2) <> stack(stack1)
            move 1 to stack(stack2)
        else
            move 0 to stack(stack2)
        end-if
        perform pop-stack
    when 'And'
        perform get-values
        call "CBL_AND" using stack(stack1) stack(stack2) by value word-length
        perform pop-stack
    when 'Or'
        perform get-values
        call "CBL_OR" using stack(stack1) stack(stack2) by value word-length
        perform pop-stack
    when 'Not'
        call 'runast' using node-left(n)
        if stack(stack1) = 0
            move 1 to stack(stack1)
        else
            move 0 to stack(stack1)
        end-if
    when 'Negate'
        call 'runast' using node-left(n)
        compute stack(stack1) = - stack(stack1)
    when 'Prtc'
        call 'runast' using node-left(n)
        move stack(stack1) to character-number
        display character-value with no advancing
        move 'y' to display-pending
        perform pop-stack
    when 'Prti'
        call 'runast' using node-left(n)
        move stack(stack1) to display-number
        display trim(display-number) with no advancing
        move 'y' to display-pending
        perform pop-stack
    when 'Prts'
        call 'runast' using node-left(n)
        move stack-source(stack1) to s
        move length(trim(string-value(s))) to string-length
        move 2 to string1
        compute length1 = string-length - 2
        perform until string1 >= string-length
            move 0 to count1
            inspect string-value(s)(string1:length1)
                tallying count1 for characters before initial '\'   *> ' (workaround Rosetta Code highlighter problem)
            evaluate true
            when string-value(s)(string1 + count1 + 1:1) = 'n' *> \n
                display string-value(s)(string1:count1)
                move 'n' to display-pending
                compute string1 = string1 + 2 + count1
                compute length1 = length1 - 2 - count1
            when string-value(s)(string1 + count1 + 1:1) = '\' *> \\ '
                display string-value(s)(string1:count1 + 1) with no advancing
                move 'y' to display-pending
                compute string1 = string1 + 2 + count1
                compute length1 = length1 - 2 - count1
            when other
                display string-value(s)(string1:count1) with no advancing
                move 'y' to display-pending
                add count1 to string1
                subtract count1 from length1
            end-evaluate
        end-perform
        perform pop-stack
    when 'Sequence'
        call 'runast' using node-left(n)
        call 'runast' using node-right(n)
    when other
        string 'in astinterpreter unknown node type ' node-type(n) into error-record
        call 'reporterror'
    end-evaluate
    exit program
    .
push-stack.
    if stack1 >= s-lim
        string 'in astinterpreter at ' n ' stack overflow' into error-record
        call 'reporterror'
    end-if
    add 1 to stack1 stack2
    initialize stack-entry(stack1)
    .
pop-stack.
    if stack1 < 2
        string 'in astinterpreter at ' n ' stack underflow ' into error-record
        call 'reporterror'
    end-if
    subtract 1 from stack1 stack2
    .
get-variable-index.
    perform varying v from 1 by 1 until v > v-max
    or variable-name(v) = leaf-value(n)
        continue
    end-perform
    if v > v-max
        if v-max = v-lim
            string 'in astinterpreter number of variables exceeds ' v-lim into error-record
            call 'reporterror'
        end-if
        move v to v-max
        move leaf-value(n) to variable-name(v)
        move 0 to variable-value(v)
    end-if
    .
get-string-index.
    perform varying s from 1 by 1 until s > s-max
    or string-value(s) = leaf-value(n)
        continue
    end-perform
    if s > s-max
        if s-max = s-lim
            string 'in astinterpreter number of strings exceeds ' s-lim into error-record
            call 'reporterror'
        end-if
        move s to s-max
        move leaf-value(n) to string-value(s)
    end-if
    .
get-values.
    call 'runast' using node-left(n)
    call 'runast' using node-right(n)
    .
end program runast.

identification division.
program-id. loadast common recursive.
procedure division.
start-loadast.
    if l >= l-lim
        string 'in astinterpreter loadast l exceeds ' l-lim into error-record
        call 'reporterror'
    end-if
    add 1 to l
    call 'readinput'
    evaluate true
    when ast-record = ';'
    when input-status = '10'
        move 0 to return-code
    when ast-type = 'Identifier'
    when ast-type = 'Integer'
    when ast-type = 'String'
        call 'makeleaf' using ast-type ast-value
        move t to return-code
    when ast-type = 'Sequence'
        move ast-type to l-node(l)
        call 'loadast'
        move return-code to l-left(l)
        call 'loadast'
        move t to l-right(l)
        call 'makenode' using l-node(l) l-left(l) l-right(l)
        move t to return-code
    when other
        move ast-type to l-node(l)
        call 'loadast'
        move return-code to l-left(l)
        call 'loadast'
        move return-code to l-right(l)
        call 'makenode' using l-node(l) l-left(l) l-right(l)
        move t to return-code
    end-evaluate
    subtract 1 from l
    .
end program loadast.

identification division.
program-id. makenode common.
data division.
linkage section.
01  parm-type any length.
01  parm-l-left pic 999.
01  parm-l-right pic 999.
procedure division using parm-type parm-l-left parm-l-right.
start-makenode.
    if t >= t-lim
        string 'in astinterpreter makenode t exceeds ' t-lim into error-record
        call 'reporterror'
    end-if
    add 1 to t
    move parm-type to node-type(t)
    move parm-l-left to node-left(t)
    move parm-l-right to node-right(t)
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
    add 1 to t
    if t >= t-lim
        string 'in astinterpreter makeleaf t exceeds ' t-lim into error-record
        call 'reporterror'
    end-if
    move parm-type to leaf-type(t)
    move parm-value to leaf-value(t)
    .
end program makeleaf.

identification division.
program-id. printast common recursive.
data division.
linkage section.
01  n pic 999.
procedure division using n.
start-printast.
    if n = 0
        display ';' upon syserr
        exit program
    end-if
    display leaf-type(n) upon syserr
    evaluate leaf-type(n)
    when 'Identifier'
    when 'Integer'
    when 'String'
        display leaf-type(n) space trim(leaf-value(n)) upon syserr
    when other
        display node-type(n) upon syserr
        call 'printast' using node-left(n)
        call 'printast' using node-right(n)
    end-evaluate
    .
end program printast.

identification division.
program-id. readinput common.
environment division.
input-output section.
file-control.
    select input-file assign using input-name
        status is input-status
        organization is line sequential.
data division.
file section.
fd  input-file.
01  input-record pic x(64).
procedure division.
start-readinput.
    if program-name = spaces
        move '00' to input-status
        accept ast-record on exception move '10' to input-status end-accept
        exit program
    end-if
    if input-name = spaces
        string program-name delimited by space '.ast' into input-name
        open input input-file
        if input-status = '35'
            string 'in astinterpreter ' trim(input-name) ' not found' into error-record
            call 'reporterror'
        end-if
    end-if
    read input-file into ast-record
    evaluate input-status
    when '00'
        continue
    when '10'
        close input-file
    when other
        string 'in astinterpreter ' trim(input-name) ' unexpected input-status: ' input-status
            into error-record
        call 'reporterror'
    end-evaluate
    .
end program readinput.

program-id. reporterror common.
procedure division.
start-reporterror.
report-error.
    display error-record upon syserr
    stop run with error status -1
    .
end program reporterror.
end program astinterpreter.
```


```txt
prompt$ ./lexer <testcases/Primes | ./parser | ./astinterpreter
3 is prime
5 is prime
7 is prime
11 is prime
13 is prime
17 is prime
19 is prime
23 is prime
29 is prime
31 is prime
37 is prime
41 is prime
43 is prime
47 is prime
53 is prime
59 is prime
61 is prime
67 is prime
71 is prime
73 is prime
79 is prime
83 is prime
89 is prime
97 is prime
101 is prime
Total primes found: 26
```



## Forth

Tested with Gforth 0.7.3

```Forth
CREATE BUF 0 ,              \ single-character look-ahead buffer
: PEEK   BUF @ 0= IF KEY BUF ! THEN BUF @ ;
: GETC   PEEK  0 BUF ! ;
: SPACE?   DUP BL = SWAP  9 14 WITHIN  OR ;
: >SPACE   BEGIN PEEK SPACE? WHILE GETC DROP REPEAT ;
: DIGIT?   48 58 WITHIN ;
: GETINT   >SPACE  0
   BEGIN  PEEK DIGIT?
   WHILE  GETC [CHAR] 0 -  SWAP 10 * +  REPEAT ;
: GETNAM   >SPACE PAD 1+
   BEGIN PEEK SPACE? INVERT
   WHILE GETC OVER C! CHAR+
   REPEAT  PAD TUCK - 1-  PAD C! ;
: GETSTR ( -- c-addr u)
   HERE >R 0  >SPACE GETC DROP  \ skip leading "
   BEGIN GETC DUP [CHAR] " <> WHILE C, 1+ REPEAT
   DROP R> SWAP ;
: \TYPE   BEGIN DUP 0> WHILE
   OVER C@ [CHAR] \ = IF
     1- >R CHAR+ R>
     OVER C@ [CHAR] n = IF CR ELSE
     OVER C@ [CHAR] \ = IF [CHAR] \ EMIT THEN THEN
   ELSE OVER C@ EMIT THEN  1- >R CHAR+ R> REPEAT
   DROP DROP ;
: .   S>D SWAP OVER DABS <# #S ROT SIGN #> TYPE ;

: CONS ( v l -- l)  HERE >R SWAP , , R> ;
: HEAD ( l -- v)  @ ;
: TAIL ( l -- l)  CELL+ @ ;
CREATE GLOBALS 0 ,
: DECLARE ( c-addr -- a-addr)  HERE TUCK
   OVER C@ CHAR+  DUP ALLOT CMOVE  HERE SWAP 0 ,
   GLOBALS @ CONS  GLOBALS ! ;
: LOOKUP ( c-addr -- a-addr)  DUP COUNT  GLOBALS @ >R
   BEGIN R@ 0<>
   WHILE R@ HEAD COUNT  2OVER COMPARE 0=
     IF 2DROP DROP  R> HEAD DUP C@ CHAR+ + EXIT
     THEN  R> TAIL >R
   REPEAT
   2DROP RDROP  DECLARE ;

DEFER GETAST
: >Identifier   GETNAM LOOKUP  0 ;
: >Integer   GETINT  0 ;
: >String   GETSTR ;
: >;   0 0 ;
: NODE ( xt left right -- addr)  HERE >R , , , R> ;
CREATE BUF' 12 ALLOT
: PREPEND ( c-addr c -- c-addr)  BUF' 1+ C!
   COUNT DUP 1+ BUF' C!  BUF' 2 + SWAP CMOVE  BUF' ;
: HANDLER ( c-addr -- xt)  [CHAR] $ PREPEND  FIND
   0= IF ." No handler for AST node '" COUNT TYPE ." '" THEN ;
: READER ( c-addr -- xt t | f)
   [CHAR] > PREPEND  FIND  DUP 0= IF NIP THEN ;
: READ ( c-addr -- left right)  READER
   IF EXECUTE ELSE GETAST GETAST THEN ;
: (GETAST)   GETNAM  DUP HANDLER SWAP  READ  NODE ;
' (GETAST) IS GETAST

: INTERP   DUP 2@  ROT [ 2 CELLS ]L + @ EXECUTE ;
: $;   DROP DROP ;
: $Identifier ( l r -- a-addr)  DROP @ ;
: $Integer ( l r -- n)  DROP ;
: $String ( l r -- c-addr u)  ( noop) ;
: $Prtc ( l r --)  DROP INTERP EMIT ;
: $Prti ( l r --)  DROP INTERP . ;
: $Prts ( l r --)  DROP INTERP \TYPE ;
: $Not ( l r --)  DROP INTERP 0= ;
: $Negate ( l r --) DROP INTERP NEGATE ;
: $Sequence ( l r --) SWAP INTERP INTERP ;
: $Assign ( l r --)  SWAP CELL+ @ >R  INTERP  R> ! ;
: $While ( l r --)
   >R BEGIN DUP INTERP WHILE R@ INTERP REPEAT  RDROP DROP ;
: $If ( l r --)  SWAP INTERP 0<> IF CELL+ THEN @ INTERP ;
: $Subtract ( l r -- n) >R INTERP R> INTERP - ;
: $Add   >R INTERP R> INTERP + ;
: $Mod   >R INTERP R> INTERP MOD ;
: $Multiply   >R INTERP R> INTERP * ;
: $Divide   >R INTERP S>D R> INTERP SM/REM SWAP DROP ;
: $Less   >R INTERP R> INTERP < ;
: $LessEqual   >R INTERP R> INTERP <= ;
: $Greater   >R INTERP R> INTERP > ;
: $GreaterEqual   >R INTERP R> INTERP >= ;
: $Equal   >R INTERP R> INTERP = ;
: $NotEqual   >R INTERP R> INTERP <> ;
: $And   >R INTERP IF R> INTERP 0<> ELSE RDROP 0 THEN ;
: $Or   >R INTERP IF RDROP -1 ELSE R> INTERP 0<> THEN ;

GETAST INTERP

```

Passes all tests.


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

type Tree struct {
    nodeType NodeType
    left     *Tree
    right    *Tree
    value    int
}

// dependency: Ordered by NodeType, must remain in same order as NodeType enum
type atr struct {
    enumText string
    nodeType NodeType
}

var atrs = []atr{
    {"Identifier", ndIdent},
    {"String", ndString},
    {"Integer", ndInteger},
    {"Sequence", ndSequence},
    {"If", ndIf},
    {"Prtc", ndPrtc},
    {"Prts", ndPrts},
    {"Prti", ndPrti},
    {"While", ndWhile},
    {"Assign", ndAssign},
    {"Negate", ndNegate},
    {"Not", ndNot},
    {"Multiply", ndMul},
    {"Divide", ndDiv},
    {"Mod", ndMod},
    {"Add", ndAdd},
    {"Subtract", ndSub},
    {"Less", ndLss},
    {"LessEqual", ndLeq},
    {"Greater", ndGtr},
    {"GreaterEqual", ndGeq},
    {"Equal", ndEql},
    {"NotEqual", ndNeq},
    {"And", ndAnd},
    {"Or", ndOr},
}

var (
    stringPool   []string
    globalNames  []string
    globalValues = make(map[int]int)
)

var (
    err     error
    scanner *bufio.Scanner
)

func reportError(msg string) {
    log.Fatalf("error : %s\n", msg)
}

func check(err error) {
    if err != nil {
        log.Fatal(err)
    }
}

func btoi(b bool) int {
    if b {
        return 1
    }
    return 0
}

func itob(i int) bool {
    if i == 0 {
        return false
    }
    return true
}

func makeNode(nodeType NodeType, left *Tree, right *Tree) *Tree {
    return &Tree{nodeType, left, right, 0}
}

func makeLeaf(nodeType NodeType, value int) *Tree {
    return &Tree{nodeType, nil, nil, value}
}

func interp(x *Tree) int { // interpret the parse tree
    if x == nil {
        return 0
    }
    switch x.nodeType {
    case ndInteger:
        return x.value
    case ndIdent:
        return globalValues[x.value]
    case ndString:
        return x.value
    case ndAssign:
        n := interp(x.right)
        globalValues[x.left.value] = n
        return n
    case ndAdd:
        return interp(x.left) + interp(x.right)
    case ndSub:
        return interp(x.left) - interp(x.right)
    case ndMul:
        return interp(x.left) * interp(x.right)
    case ndDiv:
        return interp(x.left) / interp(x.right)
    case ndMod:
        return interp(x.left) % interp(x.right)
    case ndLss:
        return btoi(interp(x.left) < interp(x.right))
    case ndGtr:
        return btoi(interp(x.left) > interp(x.right))
    case ndLeq:
        return btoi(interp(x.left) <= interp(x.right))
    case ndEql:
        return btoi(interp(x.left) == interp(x.right))
    case ndNeq:
        return btoi(interp(x.left) != interp(x.right))
    case ndAnd:
        return btoi(itob(interp(x.left)) && itob(interp(x.right)))
    case ndOr:
        return btoi(itob(interp(x.left)) || itob(interp(x.right)))
    case ndNegate:
        return -interp(x.left)
    case ndNot:
        if interp(x.left) == 0 {
            return 1
        }
        return 0
    case ndIf:
        if interp(x.left) != 0 {
            interp(x.right.left)
        } else {
            interp(x.right.right)
        }
        return 0
    case ndWhile:
        for interp(x.left) != 0 {
            interp(x.right)
        }
        return 0
    case ndPrtc:
        fmt.Printf("%c", interp(x.left))
        return 0
    case ndPrti:
        fmt.Printf("%d", interp(x.left))
        return 0
    case ndPrts:
        fmt.Print(stringPool[interp(x.left)])
        return 0
    case ndSequence:
        interp(x.left)
        interp(x.right)
        return 0
    default:
        reportError(fmt.Sprintf("interp: unknown tree type %d\n", x.nodeType))
    }
    return 0
}

func getEnumValue(name string) NodeType {
    for _, atr := range atrs {
        if atr.enumText == name {
            return atr.nodeType
        }
    }
    reportError(fmt.Sprintf("Unknown token %s\n", name))
    return -1
}

func fetchStringOffset(s string) int {
    var d strings.Builder
    s = s[1 : len(s)-1]
    for i := 0; i < len(s); i++ {
        if s[i] == '\\' && (i+1) < len(s) {
            if s[i+1] == 'n' {
                d.WriteByte('\n')
                i++
            } else if s[i+1] == '\\' {
                d.WriteByte('\\')
                i++
            }
        } else {
            d.WriteByte(s[i])
        }
    }
    s = d.String()
    for i := 0; i < len(stringPool); i++ {
        if s == stringPool[i] {
            return i
        }
    }
    stringPool = append(stringPool, s)
    return len(stringPool) - 1
}

func fetchVarOffset(name string) int {
    for i := 0; i < len(globalNames); i++ {
        if globalNames[i] == name {
            return i
        }
    }
    globalNames = append(globalNames, name)
    return len(globalNames) - 1
}

func loadAst() *Tree {
    var nodeType NodeType
    var s string
    if scanner.Scan() {
        line := strings.TrimRight(scanner.Text(), " \t")
        tokens := strings.Fields(line)
        first := tokens[0]
        if first[0] == ';' {
            return nil
        }
        nodeType = getEnumValue(first)
        le := len(tokens)
        if le == 2 {
            s = tokens[1]
        } else if le > 2 {
            idx := strings.Index(line, `"`)
            s = line[idx:]
        }
    }
    check(scanner.Err())
    if s != "" {
        var n int
        switch nodeType {
        case ndIdent:
            n = fetchVarOffset(s)
        case ndInteger:
            n, err = strconv.Atoi(s)
            check(err)
        case ndString:
            n = fetchStringOffset(s)
        default:
            reportError(fmt.Sprintf("Unknown node type: %s\n", s))
        }
        return makeLeaf(nodeType, n)
    }
    left := loadAst()
    right := loadAst()
    return makeNode(nodeType, left, right)
}

func main() {
    ast, err := os.Open("ast.txt")
    check(err)
    defer ast.Close()
    scanner = bufio.NewScanner(ast)
    x := loadAst()
    interp(x)
}
```


Prime Numbers example:

```txt

3 is prime
5 is prime
7 is prime
11 is prime
13 is prime
17 is prime
19 is prime
23 is prime
29 is prime
31 is prime
37 is prime
41 is prime
43 is prime
47 is prime
53 is prime
59 is prime
61 is prime
67 is prime
71 is prime
73 is prime
79 is prime
83 is prime
89 is prime
97 is prime
101 is prime
Total primes found: 26

```


## Java


```java

import java.util.Scanner;
import java.io.File;
import java.util.List;
import java.util.ArrayList;
import java.util.Map;
import java.util.HashMap;

class Interpreter {
	static Map<String, Integer> globals = new HashMap<>();
	static Scanner s;
	static List<Node> list = new ArrayList<>();
	static Map<String, NodeType> str_to_nodes = new HashMap<>();

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
	static enum NodeType {
		nd_None(";"), nd_Ident("Identifier"), nd_String("String"), nd_Integer("Integer"),
		nd_Sequence("Sequence"), nd_If("If"),
		nd_Prtc("Prtc"), nd_Prts("Prts"), nd_Prti("Prti"), nd_While("While"),
		nd_Assign("Assign"), nd_Negate("Negate"), nd_Not("Not"), nd_Mul("Multiply"), nd_Div("Divide"),
		nd_Mod("Mod"), nd_Add("Add"),
		nd_Sub("Subtract"), nd_Lss("Less"), nd_Leq("LessEqual"),
		nd_Gtr("Greater"), nd_Geq("GreaterEqual"), nd_Eql("Equal"), nd_Neq("NotEqual"), nd_And("And"), nd_Or("Or");

		private final String name;

		NodeType(String name) {	this.name = name; }

		@Override
		public String toString() { return this.name; }
	}
	static String str(String s) {
		String result = "";
		int i = 0;
		s = s.replace("\"", "");
		while (i < s.length()) {
			if (s.charAt(i) == '\\' && i + 1 < s.length()) {
				if (s.charAt(i + 1) == 'n') {
					result += '\n';
					i += 2;
				} else if (s.charAt(i) == '\\') {
					result += '\\';
					i += 2;
				}
			} else {
				result += s.charAt(i);
				i++;
			}
		}
		return result;
	}
	static boolean itob(int i) {
		return i != 0;
	}
	static int btoi(boolean b) {
		return b ? 1 : 0;
	}
	static int fetch_var(String name) {
		int result;
		if (globals.containsKey(name)) {
			result = globals.get(name);
		} else {
			globals.put(name, 0);
			result = 0;
		}
		return result;
	}
	static Integer interpret(Node n) throws Exception {
		if (n == null) {
			return 0;
		}
		switch (n.nt) {
			case nd_Integer:
				return Integer.parseInt(n.value);
			case nd_Ident:
				return fetch_var(n.value);
			case nd_String:
				return 1;//n.value;
			case nd_Assign:
				globals.put(n.left.value, interpret(n.right));
				return 0;
			case nd_Add:
				return interpret(n.left) + interpret(n.right);
			case nd_Sub:
				return interpret(n.left) - interpret(n.right);
			case nd_Mul:
				return interpret(n.left) * interpret(n.right);
			case nd_Div:
				return interpret(n.left) / interpret(n.right);
			case nd_Mod:
				return interpret(n.left) % interpret(n.right);
			case nd_Lss:
				return btoi(interpret(n.left) < interpret(n.right));
			case nd_Leq:
				return btoi(interpret(n.left) <= interpret(n.right));
			case nd_Gtr:
				return btoi(interpret(n.left) > interpret(n.right));
			case nd_Geq:
				return btoi(interpret(n.left) >= interpret(n.right));
			case nd_Eql:
				return btoi(interpret(n.left) == interpret(n.right));
			case nd_Neq:
				return btoi(interpret(n.left) != interpret(n.right));
			case nd_And:
				return btoi(itob(interpret(n.left)) && itob(interpret(n.right)));
			case nd_Or:
				return btoi(itob(interpret(n.left)) || itob(interpret(n.right)));
			case nd_Not:
				if (interpret(n.left) == 0) {
					return 1;
				} else {
					return 0;
				}
			case nd_Negate:
				return -interpret(n.left);
			case nd_If:
				if (interpret(n.left) != 0) {
					interpret(n.right.left);
				} else {
					interpret(n.right.right);
				}
				return 0;
			case nd_While:
				while (interpret(n.left) != 0) {
					interpret(n.right);
				}
				return 0;
			case nd_Prtc:
				System.out.printf("%c", interpret(n.left));
				return 0;
			case nd_Prti:
				System.out.printf("%d", interpret(n.left));
				return 0;
			case nd_Prts:
				System.out.print(str(n.left.value));//interpret(n.left));
				return 0;
			case nd_Sequence:
				interpret(n.left);
				interpret(n.right);
				return 0;
			default:
				throw new Exception("Error: '" + n.nt + "' found, expecting operator");
		}
	}
	static Node load_ast() throws Exception {
		String command, value;
		String line;
		Node left, right;

		while (s.hasNext()) {
			line = s.nextLine();
			value = null;
			if (line.length() > 16) {
				command = line.substring(0, 15).trim();
				value = line.substring(15).trim();
			} else {
				command = line.trim();
			}
			if (command.equals(";")) {
				return null;
			}
			if (!str_to_nodes.containsKey(command)) {
				throw new Exception("Command not found: '" + command + "'");
			}
			if (value != null) {
				return Node.make_leaf(str_to_nodes.get(command), value);
			}
			left = load_ast(); right = load_ast();
			return Node.make_node(str_to_nodes.get(command), left, right);
		}
		return null; // for the compiler, not needed
	}
	public static void main(String[] args) {
		Node n;

		str_to_nodes.put(";", NodeType.nd_None);
		str_to_nodes.put("Sequence", NodeType.nd_Sequence);
		str_to_nodes.put("Identifier", NodeType.nd_Ident);
		str_to_nodes.put("String", NodeType.nd_String);
		str_to_nodes.put("Integer", NodeType.nd_Integer);
		str_to_nodes.put("If", NodeType.nd_If);
		str_to_nodes.put("While", NodeType.nd_While);
		str_to_nodes.put("Prtc", NodeType.nd_Prtc);
		str_to_nodes.put("Prts", NodeType.nd_Prts);
		str_to_nodes.put("Prti", NodeType.nd_Prti);
		str_to_nodes.put("Assign", NodeType.nd_Assign);
		str_to_nodes.put("Negate", NodeType.nd_Negate);
		str_to_nodes.put("Not", NodeType.nd_Not);
		str_to_nodes.put("Multiply", NodeType.nd_Mul);
		str_to_nodes.put("Divide", NodeType.nd_Div);
		str_to_nodes.put("Mod", NodeType.nd_Mod);
		str_to_nodes.put("Add", NodeType.nd_Add);
		str_to_nodes.put("Subtract", NodeType.nd_Sub);
		str_to_nodes.put("Less", NodeType.nd_Lss);
		str_to_nodes.put("LessEqual", NodeType.nd_Leq);
		str_to_nodes.put("Greater", NodeType.nd_Gtr);
		str_to_nodes.put("GreaterEqual", NodeType.nd_Geq);
		str_to_nodes.put("Equal", NodeType.nd_Eql);
		str_to_nodes.put("NotEqual", NodeType.nd_Neq);
		str_to_nodes.put("And", NodeType.nd_And);
		str_to_nodes.put("Or", NodeType.nd_Or);

		if (args.length > 0) {
			try {
				s = new Scanner(new File(args[0]));
				n = load_ast();
				interpret(n);
			} catch (Exception e) {
				System.out.println("Ex: "+e.getMessage());
			}
		}
	}
}


```


## Julia


```julia
struct Anode
    node_type::String
    left::Union{Nothing, Anode}
    right::Union{Nothing, Anode}
    value::Union{Nothing, String}
end

make_leaf(t, v) = Anode(t, nothing, nothing, v)
make_node(t, l, r) = Anode(t, l, r, nothing)

const OP2 = Dict("Multiply" => "*", "Divide" => "/", "Mod" => "%", "Add" => "+", "Subtract" => "-",
                 "Less" => "<", "Greater" => ">", "LessEqual" => "<=", "GreaterEqual" => ">=",
                 "Equal" => "==", "NotEqual" => "!=", "And" => "&&", "Or" => "||")
const OP1 = Dict("Not" => "!", "Minus" => "-")

tobool(i::Bool) = i
tobool(i::Int) = (i != 0)
tobool(s::String) = eval(Symbol(s)) != 0

const stac = Vector{Any}()

function call2(op, x, y)
    if op in ["And", "Or"]
        x, y = tobool(x), tobool(y)
    end
    eval(Meta.parse("push!(stac, $(x) $(OP2[op]) $(y))"))
    return Int(floor(pop!(stac)))
end

call1(op, x) = (if op in ["Not"] x = tobool(x) end; eval(Meta.parse("$(OP1[op]) $(x)")))
evalpn(op, x, y = nothing) = (haskey(OP2, op) ? call2(op, x, y) : call1(op, x))

function load_ast(io)
    line = strip(readline(io))
    line_list = filter(x -> x != nothing, match(r"(?:(\w+)\s+(\d+|\w+|\".*\")|(\w+|;))", line).captures)
    text = line_list[1]
    if text == ";"
        return nothing
    end
    node_type = text
    if length(line_list) > 1
        return make_leaf(line_list[1], line_list[2])
    end
    left = load_ast(io)
    right = load_ast(io)
    return make_node(line_list[1], left, right)
end

function interp(x)
    if x == nothing return nothing
    elseif x.node_type == "Integer" return parse(Int, x.value)
    elseif x.node_type == "Identifier" return "_" * x.value
    elseif x.node_type == "String" return replace(replace(x.value, "\"" => ""), "\\n" => "\n")
    elseif x.node_type == "Assign" s = "$(interp(x.left)) = $(interp(x.right))"; eval(Meta.parse(s)); return nothing
    elseif x.node_type in keys(OP2) return evalpn(x.node_type, interp(x.left), interp(x.right))
    elseif x.node_type in keys(OP1) return evalpn(x.node_type, interp(x.left))
    elseif x.node_type ==  "If" tobool(eval(interp(x.left))) ? interp(x.right.left) : interp(x.right.right); return nothing
    elseif x.node_type == "While" while tobool(eval(interp(x.left))) interp(x.right) end; return nothing
    elseif x.node_type == "Prtc" print(Char(eval(interp(x.left)))); return nothing
    elseif x.node_type == "Prti" s = interp(x.left); print((i = tryparse(Int, s)) == nothing ? eval(Symbol(s)) : i); return nothing
    elseif x.node_type == "Prts" print(eval(interp(x.left))); return nothing
    elseif x.node_type == "Sequence" interp(x.left); interp(x.right); return nothing
    else
        throw("unknown node type: $x")
    end
end

const testparsed = """
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
String        \" is prime\\n\"
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
String        \"Total primes found: \"
;
Prti
Identifier    count
;
Prts
String        \"\\n\"
;  """

const lio = IOBuffer(testparsed)

interp(load_ast(lio))

```
```txt

 3 is prime
 5 is prime
 7 is prime
 11 is prime
 13 is prime
 17 is prime
 19 is prime
 23 is prime
 29 is prime
 31 is prime
 37 is prime
 41 is prime
 43 is prime
 47 is prime
 53 is prime
 59 is prime
 61 is prime
 67 is prime
 71 is prime
 73 is prime
 79 is prime
 83 is prime
 89 is prime
 97 is prime
 101 is prime
 Total primes found: 26

```



## Perl

Tested with perl v5.26.1


```Perl
#!/usr/bin/perl

use strict;   # interpreter.pl - execute a flatAST
use warnings; # http://www.rosettacode.org/wiki/Compiler/AST_interpreter
use integer;

my %variables;

tree()->run;

sub tree
  {
  my $line = <> // die "incomplete tree\n";
  (local $_, my $arg) = $line =~ /^(\w+|;)\s+(.*)/ or die "bad input $line";
  /String/ ? bless [$arg =~ tr/""//dr =~ s/\\(.)/$1 eq 'n' ? "\n" : $1/ger], $_ :
    /Identifier|Integer/ ? bless [ $arg ], $_ :
    /;/ ? bless [], 'Null' :
    bless [ tree(), tree() ], $_;
  }

sub Add::run { $_[0][0]->run + $_[0][1]->run }
sub And::run { $_[0][0]->run && $_[0][1]->run }
sub Assign::run { $variables{$_[0][0][0]} = $_[0][1]->run }
sub Divide::run { $_[0][0]->run / $_[0][1]->run }
sub Equal::run { $_[0][0]->run == $_[0][1]->run ? 1 : 0 }
sub Greater::run { $_[0][0]->run > $_[0][1]->run ? 1 : 0 }
sub GreaterEqual::run { $_[0][0]->run >= $_[0][1]->run ? 1 : 0 }
sub Identifier::run { $variables{$_[0][0]} // 0 }
sub If::run { $_[0][0]->run ? $_[0][1][0]->run : $_[0][1][1]->run }
sub Integer::run { $_[0][0] }
sub Less::run { $_[0][0]->run < $_[0][1]->run ? 1 : 0 }
sub LessEqual::run { $_[0][0]->run <= $_[0][1]->run ? 1 : 0 }
sub Mod::run { $_[0][0]->run % $_[0][1]->run }
sub Multiply::run { $_[0][0]->run * $_[0][1]->run }
sub Negate::run { - $_[0][0]->run }
sub Not::run { $_[0][0]->run ? 0 : 1 }
sub NotEqual::run { $_[0][0]->run != $_[0][1]->run ? 1 : 0 }
sub Null::run {}
sub Or::run { $_[0][0]->run || $_[0][1]->run }
sub Prtc::run { print chr $_[0][0]->run }
sub Prti::run { print $_[0][0]->run }
sub Prts::run { print $_[0][0][0] }
sub Sequence::run { $_->run for $_[0]->@* }
sub Subtract::run { $_[0][0]->run - $_[0][1]->run }
sub While::run { $_[0][1]->run while $_[0][0]->run }
```

Passes all tests.


## Phix

Reusing parse.e from the [[Compiler/syntax_analyzer#Phix|Syntax Analyzer task]]

```Phix
--
-- demo\rosetta\Compiler\interp.exw
--
### ==========================


include parse.e

sequence vars = {},
         vals = {}

function var_idx(sequence inode)
    if inode[1]!=tk_Identifier then ?9/0 end if
    string ident = inode[2]
    integer n = find(ident,vars)
    if n=0 then
        vars = append(vars,ident)
        vals = append(vals,0)
        n = length(vars)
    end if
    return n
end function

function interp(object t)
    if t!=NULL then
        integer ntype = t[1]
        object t2 = t[2],
               t3 = iff(length(t)=3?t[3]:0)
        switch ntype do
            case tk_Sequence:       {} = interp(t2) {} = interp(t3)
            case tk_assign:         vals[var_idx(t2)] = interp(t3)
            case tk_Identifier:     return vals[var_idx(t)]
            case tk_Integer:        return t2
            case tk_String:         return t2
            case tk_lt:             return interp(t2) < interp(t3)
            case tk_add:            return interp(t2) + interp(t3)
            case tk_sub:            return interp(t2) - interp(t3)
            case tk_while:          while interp(t2) do {} = interp(t3) end while
            case tk_Prints:         puts(1,interp(t2))
            case tk_Printi:         printf(1,"%d",interp(t2))
            case tk_putc:           printf(1,"%c",interp(t2))
            case tk_and:            return interp(t2) and interp(t3)
            case tk_or:             return interp(t2) or interp(t3)
            case tk_le:             return interp(t2) <= interp(t3)
            case tk_ge:             return interp(t2) >= interp(t3)
            case tk_ne:             return interp(t2) != interp(t3)
            case tk_gt:             return interp(t2) > interp(t3)
            case tk_mul:            return interp(t2) * interp(t3)
            case tk_div:            return trunc(interp(t2)/interp(t3))
            case tk_mod:            return remainder(interp(t2),interp(t3))
            case tk_if:             {} = interp(t3[iff(interp(t2)?2:3)])
            case tk_not:            return not interp(t2)
            case tk_neg:            return - interp(t2)
            else
                error("unknown node type")
        end switch
    end if
    return NULL
end function

procedure main(sequence cl)
    open_files(cl)
    toks = lex()
    object t = parse()
    {} = interp(t)
    close_files()
end procedure

--main(command_line())
main({0,0,"primes.c"})
```

```txt

3 is prime
5 is prime
7 is prime
11 is prime
13 is prime
17 is prime
19 is prime
23 is prime
29 is prime
31 is prime
37 is prime
41 is prime
43 is prime
47 is prime
53 is prime
59 is prime
61 is prime
67 is prime
71 is prime
73 is prime
79 is prime
83 is prime
89 is prime
97 is prime
101 is prime
Total primes found: 26

```



## Python

Tested with Python 2.7 and 3.x

```Python
from __future__ import print_function
import sys, shlex, operator

nd_Ident, nd_String, nd_Integer, nd_Sequence, nd_If, nd_Prtc, nd_Prts, nd_Prti, nd_While, \
nd_Assign, nd_Negate, nd_Not, nd_Mul, nd_Div, nd_Mod, nd_Add, nd_Sub, nd_Lss, nd_Leq,     \
nd_Gtr, nd_Geq, nd_Eql, nd_Neq, nd_And, nd_Or = range(25)

all_syms = {
    "Identifier"  : nd_Ident,    "String"      : nd_String,
    "Integer"     : nd_Integer,  "Sequence"    : nd_Sequence,
    "If"          : nd_If,       "Prtc"        : nd_Prtc,
    "Prts"        : nd_Prts,     "Prti"        : nd_Prti,
    "While"       : nd_While,    "Assign"      : nd_Assign,
    "Negate"      : nd_Negate,   "Not"         : nd_Not,
    "Multiply"    : nd_Mul,      "Divide"      : nd_Div,
    "Mod"         : nd_Mod,      "Add"         : nd_Add,
    "Subtract"    : nd_Sub,      "Less"        : nd_Lss,
    "LessEqual"   : nd_Leq,      "Greater"     : nd_Gtr,
    "GreaterEqual": nd_Geq,      "Equal"       : nd_Eql,
    "NotEqual"    : nd_Neq,      "And"         : nd_And,
    "Or"          : nd_Or}

input_file  = None
globals     = {}

#*** show error and exit
def error(msg):
    print("%s" % (msg))
    exit(1)

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
def fetch_var(var_name):
    n = globals.get(var_name, None)
    if n == None:
        globals[var_name] = n = 0
    return n

#***
def interp(x):
    global globals

    if x == None: return None
    elif x.node_type == nd_Integer: return int(x.value)
    elif x.node_type == nd_Ident:   return fetch_var(x.value)
    elif x.node_type == nd_String:  return x.value

    elif x.node_type == nd_Assign:
                    globals[x.left.value] = interp(x.right)
                    return None
    elif x.node_type == nd_Add:     return interp(x.left) +   interp(x.right)
    elif x.node_type == nd_Sub:     return interp(x.left) -   interp(x.right)
    elif x.node_type == nd_Mul:     return interp(x.left) *   interp(x.right)
    # use C like division semantics
    # another way: abs(x) / abs(y) * cmp(x, 0) * cmp(y, 0)
    elif x.node_type == nd_Div:     return int(float(interp(x.left)) / interp(x.right))
    elif x.node_type == nd_Mod:     return int(float(interp(x.left)) % interp(x.right))
    elif x.node_type == nd_Lss:     return interp(x.left) <   interp(x.right)
    elif x.node_type == nd_Gtr:     return interp(x.left) >   interp(x.right)
    elif x.node_type == nd_Leq:     return interp(x.left) <=  interp(x.right)
    elif x.node_type == nd_Geq:     return interp(x.left) >=  interp(x.right)
    elif x.node_type == nd_Eql:     return interp(x.left) ==  interp(x.right)
    elif x.node_type == nd_Neq:     return interp(x.left) !=  interp(x.right)
    elif x.node_type == nd_And:     return interp(x.left) and interp(x.right)
    elif x.node_type == nd_Or:      return interp(x.left) or  interp(x.right)
    elif x.node_type == nd_Negate:  return -interp(x.left)
    elif x.node_type == nd_Not:     return not interp(x.left)

    elif x.node_type ==  nd_If:
                    if (interp(x.left)):
                        interp(x.right.left)
                    else:
                        interp(x.right.right)
                    return None

    elif x.node_type == nd_While:
                    while (interp(x.left)):
                        interp(x.right)
                    return None

    elif x.node_type == nd_Prtc:
                    print("%c" % (interp(x.left)), end='')
                    return None

    elif x.node_type == nd_Prti:
                    print("%d" % (interp(x.left)), end='')
                    return None

    elif x.node_type == nd_Prts:
                    print(interp(x.left), end='')
                    return None

    elif x.node_type == nd_Sequence:
                    interp(x.left)
                    interp(x.right)
                    return None
    else:
        error("error in code generator - found %d, expecting operator" % (x.node_type))

def str_trans(srce):
    dest = ""
    i = 0
    srce = srce[1:-1]
    while i < len(srce):
        if srce[i] == '\\' and i + 1 < len(srce):
            if srce[i + 1] == 'n':
                dest += '\n'
                i += 2
            elif srce[i + 1] == '\\':
                dest += '\\'
                i += 2
        else:
            dest += srce[i]
            i += 1

    return dest

def load_ast():
    line = input_file.readline()
    line_list = shlex.split(line, False, False)

    text = line_list[0]

    value = None
    if len(line_list) > 1:
        value = line_list[1]
        if value.isdigit():
            value = int(value)

    if text == ";":
        return None
    node_type = all_syms[text]
    if value != None:
        if node_type == nd_String:
            value = str_trans(value)

        return make_leaf(node_type, value)
    left = load_ast()
    right = load_ast()
    return make_node(node_type, left, right)

#*** main driver
input_file = sys.stdin
if len(sys.argv) > 1:
    try:
        input_file = open(sys.argv[1], "r", 4096)
    except IOError as e:
        error(0, 0, "Can't open %s" % sys.argv[1])

n = load_ast()
interp(n)
```


<b>

```txt

lex prime.t | parse | interp
3 is prime
5 is prime
7 is prime
11 is prime
13 is prime
17 is prime
19 is prime
23 is prime
29 is prime
31 is prime
37 is prime
41 is prime
43 is prime
47 is prime
53 is prime
59 is prime
61 is prime
67 is prime
71 is prime
73 is prime
79 is prime
83 is prime
89 is prime
97 is prime
101 is prime
Total primes found: 26

```

</b>


## Scheme



```scheme

(import (scheme base)
        (scheme file)
        (scheme process-context)
        (scheme write)
        (only (srfi 13) string-delete string-index string-trim))

;; Mappings from operation symbols to internal procedures.
;; We define operations appropriate to virtual machine:
;; e.g. division must return an int, not a rational
;; boolean values are treated as numbers: 0 is false, other is true
(define *unary-ops*
  (list (cons 'Negate (lambda (a) (- a)))
        (cons 'Not (lambda (a) (if (zero? a) 1 0)))))
(define *binary-ops*
  (let ((number-comp (lambda (op) (lambda (a b) (if (op a b) 1 0)))))
    (list (cons 'Add +)
          (cons 'Subtract -)
          (cons 'Multiply *)
          (cons 'Divide (lambda (a b) (truncate (/ a b)))) ; int division
          (cons 'Mod modulo)
          (cons 'Less (number-comp <))
          (cons 'Greater (number-comp >))
          (cons 'LessEqual (number-comp <=))
          (cons 'GreaterEqual (number-comp >=))
          (cons 'Equal (lambda (a b) (if (= a b) 1 0)))
          (cons 'NotEqual (lambda (a b) (if (= a b) 0 1)))
          (cons 'And (lambda (a b) ; make "and" work on numbers
                       (if (and (not (zero? a)) (not (zero? b))) 1 0)))
          (cons 'Or (lambda (a b) ; make "or" work on numbers
                      (if (or (not (zero? a)) (not (zero? b))) 1 0))))))

;; Read AST from given filename
;; - return as an s-expression
(define (read-code filename)
  (define (read-expr)
    (let ((line (string-trim (read-line))))
      (if (string=? line ";")
        '()
        (let ((space (string-index line #\space)))
          (if space
            (list (string->symbol (string-trim (substring line 0 space)))
                  (string-trim (substring line space (string-length line))))
            (list (string->symbol line) (read-expr) (read-expr)))))))
  ;
  (with-input-from-file
    filename
    (lambda ()
      (read-expr))))

;; interpret AST provided as an s-expression
(define run-program
  (let ((env '())) ; env is an association list for variable names
    (lambda (expr)
      (define (tidy-string str)
        (string-delete ; remove any quote marks
          #\" ; " (to appease Rosetta code's syntax highlighter)
          (list->string
            (let loop ((chars (string->list str))) ; replace newlines, obeying \\n
              (cond ((< (length chars) 2) ; finished list
                     chars)
                    ((and (>= (length chars) 3) ; preserve \\n
                          (char=? #\\ (car chars))
                          (char=? #\\ (cadr chars))
                          (char=? #\n (cadr (cdr chars))))
                     (cons (car chars)
                           (cons (cadr chars)
                                 (cons (cadr (cdr chars))
                                       (loop (cdr (cdr (cdr chars))))))))
                    ((and (char=? #\\ (car chars)) ; replace \n with newline
                          (char=? #\n (cadr chars)))
                     (cons #\newline (loop (cdr (cdr chars)))))
                    (else ; keep char and look further
                      (cons (car chars) (loop (cdr chars)))))))))
      ; define some more meaningful names for fields
      (define left cadr)
      (define right (lambda (x) (cadr (cdr x))))
      ;
      (if (null? expr)
        '()
        (case (car expr) ; interpret AST from the head node
          ((Integer)
           (string->number (left expr)))
          ((Identifier)
           (let ((val (assq (string->symbol (left expr)) env)))
             (if val
               (cdr val)
               (error "Variable not in environment"))))
          ((String)
           (left expr))
          ((Assign)
           (set! env (cons (cons (string->symbol (left (left expr)))
                                 (run-program (right expr)))
                           env)))
          ((Add Subtract Multiply Divide Mod
                Less Greater LessEqual GreaterEqual Equal NotEqual
                And Or)
           (let ((binop (assq (car expr) *binary-ops*)))
             (if binop
               ((cdr binop) (run-program (left expr))
                            (run-program (right expr)))
               (error "Could not find binary operator"))))
          ((Negate Not)
           (let ((unaryop (assq (car expr) *unary-ops*)))
             (if unaryop
               ((cdr unaryop) (run-program (left expr)))
               (error "Could not find unary operator"))))
          ((If)
           (if (not (zero? (run-program (left expr)))) ; 0 means false
             (run-program (left (right expr)))
             (run-program (right (right expr))))
           '())
          ((While)
           (let loop ()
             (unless (zero? (run-program (left expr)))
               (run-program (right expr))
               (loop)))
           '())
          ((Prtc)
           (display (integer->char (run-program (left expr))))
           '())
          ((Prti)
           (display (run-program (left expr)))
           '())
          ((Prts)
           (display (tidy-string (run-program (left expr))))
           '())
          ((Sequence)
           (run-program (left expr))
           (run-program (right expr))
           '())
          (else
            (error "Unknown node type")))))))

;; read AST from file and interpret, from filename passed on command line
(if (= 2 (length (command-line)))
  (run-program (read-code (cadr (command-line))))
  (display "Error: pass an ast filename\n"))

```


Output for primes program from above.  Also tested on programs in [[Compiler/Sample programs]].


```txt

3 is prime
5 is prime
7 is prime
11 is prime
13 is prime
17 is prime
19 is prime
23 is prime
29 is prime
31 is prime
37 is prime
41 is prime
43 is prime
47 is prime
53 is prime
59 is prime
61 is prime
67 is prime
71 is prime
73 is prime
79 is prime
83 is prime
89 is prime
97 is prime
101 is prime
Total primes found: 26

```



## zkl


```zkl
const{ var _n=-1; var[proxy]N=fcn{ _n+=1 }; }  // enumerator
const FETCH=N, STORE=N, PUSH=N, ADD=N,  SUB=N,  MUL=N, DIV=N, MOD=N,
      LT=N,    GT=N,    LE=N,   GE=N,   EQ=N,   NE=N,
      AND=N,   OR=N,    NEG=N,  NOT=N,
      JMP=N,   JZ=N,    PRTC=N, PRTS=N, PRTI=N, HALT=N;
const nd_String=N, nd_Sequence=N, nd_If=N, nd_While=N;
var [const]
   all_syms=Dictionary(
      "Identifier"  ,FETCH,       "String"      ,nd_String,
      "Integer"     ,PUSH,        "Sequence"    ,nd_Sequence,
      "If"          ,nd_If,       "Prtc"        ,PRTC,
      "Prts"        ,PRTS,        "Prti"        ,PRTI,
      "While"       ,nd_While,    "Assign"      ,STORE,
      "Negate"      ,NEG,         "Not"         ,NOT,
      "Multiply"    ,MUL,         "Divide"      ,DIV,
      "Mod"         ,MOD,         "Add"         ,ADD,
      "Subtract"    ,SUB,         "Less"        ,LT,
      "LessEqual"   ,LE,          "Greater"     ,GT,
      "GreaterEqual",GE,          "Equal"       ,EQ,
      "NotEqual"    ,NE,          "And"         ,AND,
      "Or"          ,OR,	  "halt"	,HALT),
   bops=Dictionary(ADD,'+, SUB,'-, MUL,'*, DIV,'/, MOD,'%,
		   LT,'<, GT,'>, LE,'<=, GE,'>=, NE,'!=, EQ,'==, NE,'!=);

class Node{
   fcn init(_node_type, _value, _left=Void, _right=Void){
      var type=_node_type, left=_left, right=_right, value=_value;
   }
}

fcn runNode(node){
   var vars=Dictionary();  // fcn local static var
   if(Void==node) return();
   switch(node.type){
      case(PUSH,nd_String){ return(node.value) }
      case(FETCH){ return(vars[node.value]) }
      case(STORE){ vars[node.left.value]=runNode(node.right); return(Void); }
      case(nd_If){
         if(runNode(node.left)) runNode(node.right.left);
	 else                   runNode(node.right.right);
      }
      case(nd_While)
         { while(runNode(node.left)){ runNode(node.right) } return(Void) }
      case(nd_Sequence){ runNode(node.left); runNode(node.right); return(Void) }
      case(PRTC)       { print(runNode(node.left).toAsc()) }
      case(PRTI,PRTS)  { print(runNode(node.left)) }
      case(NEG)        { return(-runNode(node.left)) }
      case(NOT)        { return(not runNode(node.left)) }
      case(AND)        { return(runNode(node.left) and runNode(node.right)) }
      case(OR)         { return(runNode(node.left) or  runNode(node.right)) }
      else{
	 if(op:=bops.find(node.type))
	    return(op(runNode(node.left),runNode(node.right)));
	 else throw(Exception.AssertionError(
	    "Unknown node type: %d".fmt(node.type)))
      }
   }
   Void
}
```


```zkl
fcn load_ast(file){
   line:=file.readln().strip();		// one or two tokens
   if(line[0]==";") return(Void);
   parts,type,value := line.split(),parts[0],parts[1,*].concat(" ");
   type=all_syms[type];
   if(value){
      try{ value=value.toInt() }catch{}
      if(type==nd_String) value=value[1,-1].replace("\\n","\n");
      return(Node(type,value));
   }
   left,right := load_ast(file),load_ast(file);
   Node(type,Void,left,right)
}
```


```zkl
ast:=load_ast(File(vm.nthArg(0)));
runNode(ast);
```

```txt

$ zkl runAST.zkl primeAST.txt
3 is prime
5 is prime
7 is prime
11 is prime
...
89 is prime
97 is prime
101 is prime
Total primes found: 26

```

