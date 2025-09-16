+++
title = "Compiler/code generator"
description = ""
date = 2019-06-30T15:14:44Z
aliases = []
[extra]
id = 21169
[taxonomies]
categories = ["task"]
tags = []
languages = [
  "algol_68",
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
  "zkl",
]
+++

## Task

{{task}}Code Generator

A code generator translates the output of the syntax analyzer and/or semantic analyzer
into lower level code, either assembly, object, or virtual.

Take the output of the Syntax analyzer [[Compiler/syntax_analyzer|task]] - which is a [[Flatten_a_list|flattened]] Abstract Syntax Tree (AST) - and convert it to virtual machine code, that can be run by the
[[Compiler/virtual_machine_interpreter|Virtual machine interpreter]].  The output is in text format, and represents virtual assembly code.

The program should read input from a file and/or stdin, and write output to a file and/or
stdout.

;Example - given the simple program (below), stored in a file called while.t, create the list of tokens, using one of the Lexical analyzer [[Compiler/lexical_analyzer|solutions]]

 lex < while.t > while.lex

;Run one of the Syntax analyzer [[Compiler/syntax_analyzer|solutions]]:

 parse < while.lex > while.ast

;while.ast can be input into the code generator.

;The following table shows the input to lex, lex output, the AST produced by the parser, and the generated virtual assembly code.

 Run as:  lex < while.t | parse | gen

{| class="wikitable"
|-
! Input to lex
! Output from lex, input to parse
! Output from parse
! Output from gen, input to VM
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

    1      1   Identifier      count
    1      7   Op_assign
    1      9   Integer              1
    1     10   Semicolon
    2      1   Keyword_while
    2      7   LeftParen
    2      8   Identifier      count
    2     14   Op_less
    2     16   Integer             10
    2     18   RightParen
    2     20   LeftBrace
    3      5   Keyword_print
    3     10   LeftParen
    3     11   String          "count is: "
    3     23   Comma
    3     25   Identifier      count
    3     30   Comma
    3     32   String          "\n"
    3     36   RightParen
    3     37   Semicolon
    4      5   Identifier      count
    4     11   Op_assign
    4     13   Identifier      count
    4     19   Op_add
    4     21   Integer              1
    4     22   Semicolon
    5      1   RightBrace
    6      1   End_of_input
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

| style="vertical-align:top" |
<b>
```txt
Datasize: 1 Strings: 2
"count is: "
"\n"
   0 push  1
   5 store [0]
  10 fetch [0]
  15 push  10
  20 lt
  21 jz     (43) 65
  26 push  0
  31 prts
  32 fetch [0]
  37 prti
  38 push  1
  43 prts
  44 fetch [0]
  49 push  1
  54 add
  55 store [0]
  60 jmp    (-51) 10
  65 halt
```
</b>
|}

; Input format:

As shown in the table, above, the output from the [[Compiler/syntax_analyzer|syntax analyzer]] is a flattened AST.

In the AST, Identifier, Integer, and String, are terminal nodes, e.g, they do not have child nodes.

Loading this data into an internal parse tree should be as simple as:


```python

def load_ast()
    line = readline()
    # Each line has at least one token
    line_list = tokenize the line, respecting double quotes

    text = line_list[0] # first token is always the node type

    if text == ";"
        return None

    node_type = text # could convert to internal form if desired

    # A line with two tokens is a leaf node
    # Leaf nodes are: Identifier, Integer String
    # The 2nd token is the value
    if len(line_list) > 1
        return make_leaf(node_type, line_list[1])

    left = load_ast()
    right = load_ast()
    return make_node(node_type, left, right)

```


; Output format - refer to the table above

* The first line is the header: Size of data, and number of constant strings.
** size of data is the number of 32-bit unique variables used.  In this example, one variable, '''count'''
** number of constant strings is just that - how many there are
* After that, the constant strings
* Finally, the assembly code

;Registers:

* sp: the stack pointer - points to the next top of stack.  The stack is a 32-bit integer array.

* pc: the program counter - points to the current instruction to be performed.  The code is an array of bytes.

;Data:

32-bit integers and strings

;Instructions:

Each instruction is one byte.  The following instructions also have a 32-bit integer operand:

 fetch [index]

where index is an index into the data array.

 store [index]

where index is an index into the data array.

 push n

where value is a 32-bit integer that will be pushed onto the stack.

 jmp (n) addr

where (n) is a 32-bit integer specifying the distance between the current location and the
desired location.  addr is an unsigned value of the actual code address.

 jz (n) addr

where (n) is a 32-bit integer specifying the distance between the current location and the
desired location.  addr is an unsigned value of the actual code address.

The following instructions do not have an operand.  They perform their operation directly
against the stack:

For the following instructions, the operation is performed against the top two entries in
the stack:

 add
 sub
 mul
 div
 mod
 lt
 gt
 le
 ge
 eq
 ne
 and
 or

For the following instructions, the operation is performed against the top entry in the
stack:

 neg
 not

 prtc

Print the word at stack top as a character.

 prti

Print the word at stack top as an integer.

 prts

Stack top points to an index into the string pool.  Print that entry.

 halt

Unconditional stop.

; Additional examples

Your solution should pass all the test cases above and the additional tests found '''[[Compiler/Sample_programs|Here]]'''.

The C and Python versions can be considered reference implementations.

;Related Tasks

* [[Compiler/lexical_analyzer|Lexical Analyzer task]]
* [[Compiler/syntax_analyzer|Syntax Analyzer task]]
* [[Compiler/virtual_machine_interpreter|Virtual Machine Interpreter task]]
* [[Compiler/AST_interpreter|AST Interpreter task]]

<hr>
__TOC__


## ALGOL 68

Based on the Algol W sample. This generates .NET IL assembler code which can be compiled with the .NET ilasm assembler to generate an exe that can be run under Windows (and presumably Mono though I haven't tried that).


Apart from the namespace, class and method blocks surrounding the code, the main differences between IL and the task's assembly code are: no "compare-le", "compare-ge", "compare-ne", "prts", "prtc", "prti" and "not" instructions, symbolic labels are used and symbolic local variable names can be used. Some IL instructions have different names, e.g. "stloc" instead of "store". The "prt*" instructions are handled by calling the relevant System.Out.Print method. The compare and "not" instructions are handled by generating equivalent instruction sequences.


As noted in the code, the generated IL is naive - the sample focuses on simplicity.

```algol68
# RC Compiler code generator #
COMMENT
    this writes a .NET IL assembler source to standard output.
    If the output is stored in a file called "rcsample.il",
    it could be compiled the command:
        ilasm /opt /out:rcsample.exe rcsample.il
    (Note ilasm may not be in the PATH by default(

    Note: The generated IL is *very* naive
COMMENT

# parse tree nodes #
MODE NODE = STRUCT( INT type, REF NODE left, right, INT value );
INT nidentifier   =  1, nstring    =  2, ninteger  =  3, nsequence  =  4, nif        =  5, nprtc     =  6, nprts   =  7
  , nprti         =  8, nwhile     =  9, nassign   = 10, nnegate    = 11, nnot       = 12, nmultiply = 13, ndivide = 14
  , nmod          = 15, nadd       = 16, nsubtract = 17, nless      = 18, nlessequal = 19, ngreater  = 20
  , ngreaterequal = 21, nequal     = 22, nnotequal = 23, nand       = 24, nor        = 25
  ;
# op codes #
INT ofetch =  1, ostore =  2, opush =  3, oadd =  4, osub  =  5, omul  =  6, odiv  =  7, omod     =  8
  , olt    =  9, ogt    = 10, ole   = 11, oge  = 12, oeq   = 13, one   = 14, oand  = 15, oor      = 16
  , oneg   = 17, onot   = 18, ojmp  = 19, ojz  = 20, oprtc = 21, oprts = 22, oprti = 23, opushstr = 24
  ;
[]INT    ndop
= ( -1               , -1             , -1            , -1             , -1             , -1            , -1
  , -1               , -1             , -1            , oneg           , -1             , omul          , odiv
  , omod             , oadd           , osub          , olt            , -1             , ogt
  , -1               , oeq            , -1            , oand           , oor
  ) ;
[]STRING ndname
= ( "Identifier"     , "String"       , "Integer"     , "Sequence"     , "If"           , "Prtc"        , "Prts"
  , "Prti"           , "While"        , "Assign"      , "Negate"       , "Not"          , "Multiply"    , "Divide"
  , "Mod"            , "Add"          , "Subtract"    , "Less"         , "LessEqual"    , "Greater"
  , "GreaterEqual"   , "Equal"        , "NotEqual"    , "And"          , "Or"
  ) ;
[]STRING opname
= ( "ldloc  ",  "stloc  ",   "ldc.i4 ",  "add    ",  "sub    ", "mul    ",  "div    ",  "rem    "
  , "clt    ",  "cgt    ",   "?le    ",  "?ge    ",  "ceq    ", "?ne    ",  "and    ",  "or     "
  , "neg    ",  "?not   ",   "br     ",  "brfalse",  "?prtc  ", "?prts  ",  "?prti  ",  "ldstr  "
  ) ;
# string and identifier arrays - a hash table might be better... #
INT max string number = 1024;
[ 0 : max string number ]STRING identifiers, strings;
FOR s pos FROM 0 TO max string number DO
    identifiers[ s pos ] := "";
    strings    [ s pos ] := ""
OD;
# label number for label generation #
INT next label number := 0;
# returns the next free label number #
PROC new label = INT: next label number +:= 1;

# returns a new node with left and right branches #
PROC op node      = ( INT op type, REF NODE left, right )REF NODE: HEAP NODE := NODE( op type, left, right, 0 );
# returns a new operand node #
PROC operand node = ( INT op type, value )REF NODE: HEAP NODE := NODE( op type, NIL, NIL, value );

# reports an error and stops #
PROC gen error = ( STRING message )VOID:
     BEGIN
        print( ( message, newline ) );
        stop
     END # gen error # ;

# reads a node from standard input #
PROC read node = REF NODE:
     BEGIN
        REF NODE result := NIL;

        # parses a string from line and stores it in a string in the text array #
        # - if it is not already present in the specified textElement list.     #
        # returns the position of the string in the text array                  #
        PROC read string = ( REF[]STRING text list, CHAR terminator )INT:
             BEGIN
                # get the text of the string #
                STRING str := line[ l pos ];
                l pos +:= 1;
                WHILE IF l pos <= UPB line THEN line[ l pos ] /= terminator ELSE FALSE FI DO
                    str   +:= line[ l pos ];
                    l pos +:= 1
                OD;
                IF l pos > UPB line THEN gen error( "Unterminated String in node file: (" + line + ")." ) FI;
                # attempt to find the text in the list of strings/identifiers #
                INT  t pos  := LWB text list;
                BOOL found  := FALSE;
                INT  result := LWB text list - 1;
                FOR t pos FROM LWB text list TO UPB text list WHILE NOT found DO
                    IF found := text list[ t pos ] = str THEN
                        # found the string #
                        result := t pos
                    ELIF text list[ t pos ] = "" THEN
                        # have an empty slot for ther string #
                        found := TRUE;
                        text list[ t pos ] := str;
                        result := t pos
                    FI
                OD;
                IF NOT found THEN gen error( "Out of string space." ) FI;
                result
             END # read string # ;
        # gets an integer from the line - no checks for valid digits #
        PROC read integer = INT:
             BEGIN
                 INT n := 0;
                 WHILE line[ l pos ] /= " " DO
                     ( n *:= 10 ) +:= ( ABS line[ l pos ] - ABS "0" );
                     l pos +:= 1
                 OD;
                 n
             END # read integer # ;

        STRING line, name;
        INT    l pos := 1, nd type := -1;
        read( ( line, newline ) );
        line +:= " ";
        # get the node type name #
        WHILE line[ l pos ] = " " DO l pos +:= 1 OD;
        name := "";
        WHILE IF l pos > UPB line THEN FALSE ELSE line[ l pos ] /= " " FI DO
            name +:= line[ l pos ];
            l pos +:= 1
        OD;
        # determine the node type #
        nd type := LWB nd name;
        IF name /= ";" THEN
            # not a null node #
            WHILE IF nd type <= UPB nd name THEN name /= nd name[ nd type ] ELSE FALSE FI DO nd type +:= 1 OD;
            IF nd type > UPB nd name THEN gen error( "Malformed node: (" + line + ")." ) FI;
            # handle the additional parameter for identifier/string/integer, or sub-nodes for operator nodes #
            IF nd type = ninteger OR nd type = nidentifier OR nd type = nstring THEN
                WHILE line[ l pos ] = " " DO l pos +:= 1 OD;
                IF     nd type = ninteger    THEN result := operand node( nd type, read integer )
                ELIF   nd type = nidentifier THEN result := operand node( nd type, read string( identifiers, " "  ) )
                ELSE # nd type = nString     #    result := operand node( nd type, read string( strings,     """" ) )
                FI
            ELSE
                # operator node #
                REF NODE left node = read node;
                result := op node( nd type, left node, read node )
            FI
        FI;
        result
     END # read node # ;

# returns a formatted op code for code generation #
PROC operation = ( INT op code )STRING: "            " + op name[ op code ] + "  ";
# defines the specified label #
PROC define label = ( INT label number )VOID: print( ( "lbl_", whole( label number, 0 ), ":", newline ) );
# generates code to load a string value #
PROC gen load string   = ( INT value )VOID:
     BEGIN
        print( ( operation( opushstr ), "  ", strings[ value ], """", newline ) )
     END # push string # ;
# generates code to load a constant value #
PROC gen load constant = ( INT value )VOID: print( ( operation( opush ), "  ", whole( value, 0 ), newline ) );
# generates an operation acting on an address #
PROC gen data op = ( INT op, address )VOID: print( ( operation( op ), "  l_", identifiers[ address ], newline ) );
# generates a nullary operation #
PROC gen op 0    = ( INT op )VOID:          print( ( operation( op ), newline ) );
# generates a "not" instruction sequence #
PROC gen not = VOID:
     BEGIN
        gen load constant( 0 );
        print( ( operation( oeq ), newline ) )
     END # gen not # ;
# generates a negated condition #
PROC gen not op = ( INT op, REF NODE n )VOID:
     BEGIN
        gen(  left OF n );
        gen( right OF n );
        gen op 0( op );
        gen not
     END # gen not op # ;
# generates a jump operation #
PROC gen jump = ( INT op, label )VOID: print( ( operation( op ), "  lbl_", whole( label, 0 ), newline ) );
# generates code to output something to System.Console.Out #
PROC gen output = ( REF NODE n, STRING output type )VOID:
     BEGIN
        print( ( "            call       " ) );
        print( ( "class [mscorlib]System.IO.TextWriter [mscorlib]System.Console::get_Out()", newline ) );
        gen( left OF n );
        print( ( "            callvirt   " ) );
        print( ( "instance void [mscorlib]System.IO.TextWriter::Write(", output type, ")", newline ) )
     END # gen output # ;

# generates the code header - assembly info, namespace, class and start of the Main method #
PROC code header = VOID:
     BEGIN
        print( ( ".assembly extern mscorlib { auto }",                                  newline ) );
        print( ( ".assembly RccSample {}",                                              newline ) );
        print( ( ".module RccSample.exe",                                               newline ) );
        print( ( ".namespace Rcc.Sample",                                               newline ) );
        print( ( "{",                                                                   newline ) );
        print( ( "    .class public auto ansi Program extends [mscorlib]System.Object", newline ) );
        print( ( "    {",                                                               newline ) );
        print( ( "        .method public static void Main() cil managed",               newline ) );
        print( ( "        {",                                                           newline ) );
        print( ( "           .entrypoint",                                              newline ) );
        # output the local variables #
        BOOL   have locals  := FALSE;
        STRING local prefix := "           .locals init (int32 l_";
        FOR s pos FROM LWB identifiers TO UPB identifiers WHILE identifiers[ s pos ] /= "" DO
            print( ( local prefix, identifiers[ s pos ], newline ) );
            local prefix := "                        ,int32 l_";
            have locals  := TRUE
        OD;
        IF have locals THEN
            # there were some local variables defined - output the terminator #
            print( ( "                        )", newline ) )
        FI
     END # code header # ;

# generates code for the node n #
PROC gen = ( REF NODE n )VOID:
     IF n IS REF NODE( NIL )        THEN # null node       #
        SKIP
     ELIF type OF n = nidentifier   THEN # load identifier #
        gen data op( ofetch, value OF n )
     ELIF type OF n = nstring       THEN # load string     #
        gen load string( value OF n )
     ELIF type OF n = ninteger      THEN # load integer    #
        gen load constant( value OF n )
     ELIF type OF n = nsequence     THEN # list            #
        gen(  left OF n );
        gen( right OF n )
     ELIF type OF n = nif           THEN # if-else         #
        INT else label := new label;
        gen( left OF n );
        gen jump( ojz, else label );
        gen( left OF right OF n );
        IF right OF right OF n IS REF NODE( NIL ) THEN
            # no "else" part #
            define label( else label )
        ELSE
            # have an "else" part #
            INT end if label := new label;
            gen jump( ojmp, end if label );
            define label( else label );
            gen( right OF right OF n );
            define label( end if label )
        FI
     ELIF type OF n = nwhile        THEN # while-loop      #
        INT loop label := new label;
        INT exit label := new label;
        define label( loop label );
        gen(  left OF n );
        gen jump( ojz,  exit label );
        gen( right OF n );
        gen jump( ojmp, loop label );
        define label( exit label )
     ELIF type OF n = nassign       THEN # assignment      #
        gen( right OF n );
        gen data op( ostore, value OF left OF n )
     ELIF type OF n = nnot          THEN # bolean not      #
        gen( left OF n );
        gen not
     ELIF type OF n = ngreaterequal THEN # compare >=      #
        gen not op( olt, n )
     ELIF type OF n = nnotequal     THEN # compare not =   #
        gen not op( oeq, n )
     ELIF type OF n = nlessequal    THEN # compare <=      #
        gen not op( ogt, n )
     ELIF type OF n = nprts         THEN # print string    #
        gen output( n, "string" )
     ELIF type OF n = nprtc         THEN # print character #
        gen output( n, "char" )
     ELIF type OF n = nprti         THEN # print integer   #
        gen output( n, "int32" )
     ELSE                                # everything else #
        gen(  left OF n );
        gen( right OF n ); # right will be null for a unary op so no code will be generated #
        print( ( operation( ndop( type OF n ) ), newline ) )
     FI # gen # ;

# generates the code trailer - return instruction, end of Main method, end of class and end of namespace #
PROC code trailer = VOID:
     BEGIN
        print( ( "            ret",           newline ) );
        print( ( "        } // Main method",  newline ) );
        print( ( "    } // Program class",    newline ) );
        print( ( "} // Rcc.Sample namespace", newline ) )
     END # code trailer # ;

# parse the output from the syntax analyser and generate code from the parse tree #
REF NODE code = read node;
code header;
gen( code );
code trailer
```

```txt

.assembly extern mscorlib { auto }
.assembly RccSample {}
.module RccSample.exe
.namespace Rcc.Sample
{
    .class public auto ansi Program extends [mscorlib]System.Object
    {
        .method public static void Main() cil managed
        {
           .entrypoint
           .locals init (int32 l_count
                        )
            ldc.i4     1
            stloc      l_count
lbl_1:
            ldloc      l_count
            ldc.i4     10
            clt
            brfalse    lbl_2
            call       class [mscorlib]System.IO.TextWriter [mscorlib]System.Console::get_Out()
            ldstr      "count is: "
            callvirt   instance void [mscorlib]System.IO.TextWriter::Write(string)
            call       class [mscorlib]System.IO.TextWriter [mscorlib]System.Console::get_Out()
            ldloc      l_count
            callvirt   instance void [mscorlib]System.IO.TextWriter::Write(int32)
            call       class [mscorlib]System.IO.TextWriter [mscorlib]System.Console::get_Out()
            ldstr      "\n"
            callvirt   instance void [mscorlib]System.IO.TextWriter::Write(string)
            ldloc      l_count
            ldc.i4     1
            add
            stloc      l_count
            br         lbl_1
lbl_2:
            ret
        } // Main method
    } // Program class
} // Rcc.Sample namespace

```



## ALGOL W


```algolw
begin % code generator %
    % parse tree nodes %
    record node( integer         type
               ; reference(node) left, right
               ; integer         iValue % nString/nIndentifier number or nInteger value %
               );
    integer    nIdentifier, nString, nInteger, nSequence, nIf,   nPrtc, nPrts
          ,    nPrti,       nWhile,  nAssign,  nNegate,   nNot,  nMultiply
          ,    nDivide,     nMod,    nAdd,     nSubtract, nLess, nLessEqual
          ,    nGreater,    nGreaterEqual,     nEqual,    nNotEqual,    nAnd, nOr
          ;
    string(14) array ndName ( 1 :: 25 );
    integer    array nOp    ( 1 :: 25 );
    integer    MAX_NODE_TYPE;
    % string literals and identifiers - uses a linked list - a hash table might be better... %
    string(1)  array text ( 0 :: 4095 );
    integer    textNext, TEXT_MAX;
    record textElement ( integer start, length; reference(textElement) next );
    reference(textElement) idList, stList;
    % op codes %
    integer    oFetch, oStore, oPush
          ,    oAdd,   oSub,   oMul, oDiv, oMod, oLt, oGt,   oLe,   oGe,   oEq,  oNe
          ,    oAnd,   oOr,    oNeg, oNot, oJmp, oJz, oPrtc, oPrts, oPrti, oHalt
          ;
    string(6)  array opName ( 1 :: 24 );
    % code - although this is intended to be byte code, as we are going to output    %
    %        an assembler source, we use integers for convenience                    %
    % labelLocations are: - ( referencing location + 1 ) if they have been referenced but not defined yet, %
    %                     zero     if they are unreferenced and undefined,                                 %
    %                     ( referencing location + 1 )   if they are defined                               %
    integer    array byteCode ( 0 :: 4095 );
    integer    array labelLocation( 1 :: 4096 );
    integer    nextLocation, MAX_LOCATION, nextLabelNumber, MAX_LABEL_NUMBER;

    % returns a new node with left and right branches %
    reference(node) procedure opNode ( integer value opType; reference(node) value opLeft, opRight ) ; begin
        node( opType, opLeft, opRight, 0 )
    end opNode ;

    % returns a new operand node %
    reference(node) procedure operandNode ( integer value opType, opValue ) ; begin
        node( opType, null, null, opValue )
    end operandNode ;

    % reports an error and stops %
    procedure genError( string(80) value message ); begin
        integer errorPos;
        write( s_w := 0, "**** Code generation error: " );
        errorPos := 0;
        while errorPos < 80 and message( errorPos // 1 ) not = "." do begin
            writeon( s_w := 0, message( errorPos // 1 ) );
            errorPos := errorPos + 1
        end while_not_at_end_of_message ;
        writeon( s_w := 0, "." );
        assert( false )
    end genError ;

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
            if lPos > 255 then genError( "Unterminated String in node file." );
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
                if textNext + sLen > TEXT_MAX then genError( "Text space exhausted." )
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
            if ndType > MAX_NODE_TYPE then genError( "Malformed node." );
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

    % returns the next free label number %
    integer procedure newLabel ; begin
        nextLabelNumber := nextLabelNumber + 1;
        if nextLabelNumber > MAX_LABEL_NUMBER then genError( "Program too complex" );
        nextLabelNumber
    end newLabel ;

    % defines the specified label to be at the next location %
    procedure defineLabel ( integer value labelNumber ) ; begin
        if labelLocation( labelNumber ) > 0 then genError( "Label already defined" )
        else begin
            % this is the first definition of the label, define it and if it has already been referenced, fill in the reference %
            integer currValue;
            currValue := labelLocation( labelNumber );
            labelLocation( labelNumber ) := nextLocation + 1; % we store pc + 1 to ensure the label location is positive %
            if currValue < 0 then % already referenced % byteCode( - ( currValue + 1 ) ) := labelLocation( labelNumber )
        end
    end defineLabel ;

    % stores a byte in the code %
    procedure genByte ( integer value byteValue ) ; begin
        if nextLocation > MAX_LOCATION then genError( "Program too large" );
        byteCode( nextLocation ) := byteValue;
        nextLocation := nextLocation + 1
    end genByte ;

    % stores an integer in the code %
    procedure genInteger ( integer value integerValue ) ; begin
        % we are storing the bytes of the code in separate integers for convenience %
        genByte( integerValue ); genByte( 0 ); genByte( 0 ); genByte( 0 )
    end genInteger ;

    % generates an operation acting on an address %
    procedure genDataOp ( integer value opCode, address ) ; begin
        genByte( opCode );
        genInteger( address )
    end genDataOp ;

    % generates a nullary operation %
    procedure genOp0  ( integer value opCode ) ; begin
        genByte( opCode )
    end genOp0 ;

    % generates a unary/binary operation %
    procedure genOp ( reference(node) value n ) ; begin
        gen(  left(n) );
        gen( right(n) ); % right will be null for a unary op so no code will be generated %
        genByte( nOp( type(n) ) )
    end genOp ;

    % generates a jump operation %
    procedure genJump   ( integer value opCode, labelNumber ) ; begin
        genByte( opCode );
        % if the label is not defined yet - set it's location to the negative of the referencing location %
        % so it can be resolved later %
        if labelLocation( labelNumber ) = 0 then labelLocation( labelNumber ) := - ( nextLocation + 1 );
        genInteger( labelLocation( labelNumber ) )
    end genJump ;

    % generates code for the node n %
    procedure gen ( reference(node) value n ) ; begin

        if           n  = null        then % empty node % begin end
        else if type(n) = nIdentifier then genDataOp( oFetch, iValue(n) )
        else if type(n) = nString     then genDataOp( oPush,  iValue(n) - 1 )
        else if type(n) = nInteger    then genDataOp( oPush,  iValue(n) )
        else if type(n) = nSequence   then begin
            gen(  left(n) );
            gen( right(n) )
            end
        else if type(n) = nIf         then % if-else         % begin
            integer elseLabel;
            elseLabel := newLabel;
            gen( left(n) );
            genJump( oJz, elseLabel );
            gen( left( right(n) ) );
            if right(right(n)) = null then % no "else" part % defineLabel( elseLabel )
            else begin
                % have an "else" part %
                integer endIfLabel;
                endIfLabel := newLabel;
                genJump( oJmp, endIfLabel );
                defineLabel( elseLabel );
                gen( right(right(n)) );
                defineLabel( endIfLabel )
            end
            end
        else if type(n) = nWhile      then % while-loop      % begin
            integer loopLabel, exitLabel;
            loopLabel := newLabel;
            exitLabel := newLabel;
            defineLabel( loopLabel );
            gen(  left(n) );
            genJump( oJz,  exitLabel );
            gen( right(n) );
            genJump( oJmp, loopLabel );
            defineLabel( exitLabel )
            end
        else if type(n) = nAssign     then % assignment      % begin
            gen( right( n ) );
            genDataOp( oStore, iValue(left(n)) )
            end
        else genOp( n )
    end gen ;

    % outputs the generated code to standard output %
    procedure emitCode ; begin

        % counts the number of elements in a text element list %
        integer procedure countElements ( reference(textElement) value txHead ) ; begin
            integer count;
            reference(textElement) txPos;
            count := 0;
            txPos := txHead;
            while txPos not = null do begin
                count := count + 1;
                txPos := next(txPos)
            end while_txPos_not_null ;
            count
        end countElements ;

        integer pc, op;
        reference(textElement) txPos;

        % code header %
        write( i_w := 1, s_w := 0
             , "Datasize: ", countElements( idList )
             , " Strings: ", countElements( stList )
             );
        % output the string literals %
        txPos := stList;
        while txPos not = null do begin
            integer cPos;
            write( """" );
            cPos := 1; % start from 1 to skip over the leading " %
            while cPos < length(txPos) do begin
                writeon( s_w := 0, text( start(txPos) + cPos ) );
                cPos := cPos + 1
            end while_not_end_of_string ;
            writeon( s_w := 0, """" );
            txPos := next(txPos)
        end while_not_at_end_of_literals ;

        % code body %
        pc := 0;
        while pc < nextLocation do begin
            op := byteCode( pc );
            write( i_w := 4, s_w := 0, pc, " ", opName( op ) );
            pc := pc + 1;
            if      op = oFetch or op = oStore then begin
                % data load/store - add the address in square brackets %
                writeon( i_w := 1, s_w := 0, "[", byteCode( pc ) - 1, "]" );
                pc := pc + 4
                end
            else if op = oPush                 then begin
                % push constant - add the constant %
                writeon( i_w := 1, s_w := 0, byteCode( pc ) );
                pc := pc + 4
                end
            else if op = oJmp   or op = oJz    then begin
                % jump - show the relative address in brackets and the absolute address %
                writeon( i_w := 1, s_w := 0, "(", ( byteCode( pc ) - 1 ) - pc, ") ", byteCode( pc ) - 1 );
                pc := pc + 4
            end
        end while_pc_lt_nextLocation
    end emitCode ;

    oFetch :=  1; opName( oFetch ) := "fetch"; oStore :=  2; opName( oStore ) := "store"; oPush :=  3; opName( oPush ) := "push";
    oAdd   :=  4; opName( oAdd   ) := "add";   oSub   :=  5; opName( oSub   ) := "sub";   oMul  :=  6; opName( oMul  ) := "mul";
    oDiv   :=  7; opName( oDiv   ) := "div";   oMod   :=  8; opName( oMod   ) := "mod";   oLt   :=  9; opName( oLt   ) := "lt";
    oGt    := 10; opName( oGt    ) := "gt";    oLe    := 11; opName( oLe    ) := "le";    oGe   := 12; opName( oGe   ) := "ge";
    oEq    := 13; opName( oEq    ) := "eq";    oNe    := 14; opName( oNe    ) := "ne";    oAnd  := 15; opName( oAnd  ) := "and";
    oOr    := 16; opName( oOr    ) := "or";    oNeg   := 17; opName( oNeg   ) := "neg";   oNot  := 18; opName( oNot  ) := "not";
    oJmp   := 19; opName( oJmp   ) := "jmp";   oJz    := 20; opName( oJz    ) := "jz";    oPrtc := 21; opName( oPrtc ) := "prtc";
    oPrts  := 22; opName( oPrts  ) := "prts";  oPrti  := 23; opName( oPrti  ) := "prti";  oHalt := 24; opName( oHalt ) := "halt";

    nIdentifier      :=  1; ndName( nIdentifier   ) := "Identifier";   nString   :=  2; ndName( nString   ) := "String";
    nInteger         :=  3; ndName( nInteger      ) := "Integer";      nSequence :=  4; ndName( nSequence ) := "Sequence";
    nIf              :=  5; ndName( nIf           ) := "If";           nPrtc     :=  6; ndName( nPrtc     ) := "Prtc";
    nPrts            :=  7; ndName( nPrts         ) := "Prts";         nPrti     :=  8; ndName( nPrti     ) := "Prti";
    nWhile           :=  9; ndName( nWhile        ) := "While";        nAssign   := 10; ndName( nAssign   ) := "Assign";
    nNegate          := 11; ndName( nNegate       ) := "Negate";       nNot      := 12; ndName( nNot      ) := "Not";
    nMultiply        := 13; ndName( nMultiply     ) := "Multiply";     nDivide   := 14; ndName( nDivide   ) := "Divide";
    nMod             := 15; ndName( nMod          ) := "Mod";          nAdd      := 16; ndName( nAdd      ) := "Add";
    nSubtract        := 17; ndName( nSubtract     ) := "Subtract";     nLess     := 18; ndName( nLess     ) := "Less";
    nLessEqual       := 19; ndName( nLessEqual    ) := "LessEqual";    nGreater  := 20; ndName( nGreater  ) := "Greater";
    nGreaterEqual    := 21; ndName( nGreaterEqual ) := "GreaterEqual"; nEqual    := 22; ndName( nEqual    ) := "Equal";
    nNotEqual        := 23; ndName( nNotEqual     ) := "NotEqual";     nAnd      := 24; ndName( nAnd      ) := "And";
    nOr              := 25; ndName( nOr           ) := "Or";
    MAX_NODE_TYPE    := 25; TEXT_MAX := 4095; textNext := 0;
    stList := idList := null;
    for nPos := 1 until MAX_NODE_TYPE do nOp( nPos ) := -1;
    nOp( nPrtc     ) := oPrtc; nOp( nPrts      ) := oPrts; nOp( nPrti    ) := oPrti; nOp( nNegate       ) := oNeg; nOp( nNot      ) := oNot;
    nOp( nMultiply ) := oMul;  nOp( nDivide    ) := oDiv;  nOp( nMod     ) := oMod;  nOp( nAdd          ) := oAdd; nOp( nSubtract ) := oSub;
    nOp( nLess     ) := oLt;   nOp( nLessEqual ) := oLe;   nOp( nGreater ) := oGt;   nOp( nGreaterEqual ) := oGe;  nOp( nEqual    ) := oEq;
    nOp( nNotEqual ) := oNe;   nOp( nAnd       ) := oAnd;  nOp( nOr      ) := oOr;
    nextLocation     := 0; MAX_LOCATION := 4095;
    for pc := 0 until MAX_LOCATION do byteCode( pc ) := 0;
    nextLabelNumber := 0; MAX_LABEL_NUMBER := 4096;
    for lPos := 1 until MAX_LABEL_NUMBER do labelLocation( lPos ) := 0;

    % parse the output from the syntax analyser and generate code from the parse tree %
    gen( readNode );
    genOp0( oHalt );
    emitCode
end.
```

The While Counter example

```txt

Datasize: 1 Strings: 2
"count is: "
"\n"
   0 push  1
   5 store [0]
  10 fetch [0]
  15 push  10
  20 lt
  21 jz    (43) 65
  26 push  0
  31 prts
  32 fetch [0]
  37 prti
  38 push  1
  43 prts
  44 fetch [0]
  49 push  1
  54 add
  55 store [0]
  60 jmp   (-51) 10
  65 halt

```



## AWK

Tested with gawk 4.1.1 and mawk 1.3.4.

```AWK

function error(msg) {
  printf("%s\n", msg)
  exit(1)
}

function bytes_to_int(bstr,          i, sum) {
  sum = 0
  for (i=word_size-1; i>=0; i--) {
    sum *= 256
    sum += code[bstr+i]
  }
  return sum
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

function emit_byte(x) {
  code[next_free_code_index++] = x
}

function emit_word(x,       i) {
  for (i=0; i<word_size; i++) {
    emit_byte(int(x)%256);
    x = int(x/256)
  }
}

function emit_word_at(at, n,             i) {
  for (i=0; i<word_size; i++) {
    code[at+i] = int(n)%256
    n = int(n/256)
  }
}

function hole(         t) {
  t = next_free_code_index
  emit_word(0)
  return t
}

function fetch_var_offset(name,       n) {
  if (name in globals) {
    n = globals[name]
  } else {
    globals[name] = globals_n
    n = globals_n
    globals_n += 1
  }
  return n
}

function fetch_string_offset(the_string,        n) {
  n = string_pool[the_string]
  if (n == "") {
    string_pool[the_string] = string_n
    n = string_n
    string_n += 1
  }
  return n
}

function code_gen(x,       n, p1, p2) {
  if (x == 0) {
    return
  } else if (node_type[x] == "nd_Ident") {
    emit_byte(FETCH)
    n = fetch_var_offset(node_value[x])
    emit_word(n)
  } else if (node_type[x] == "nd_Integer") {
    emit_byte(PUSH)
    emit_word(node_value[x])
  } else if (node_type[x] == "nd_String") {
    emit_byte(PUSH)
    n = fetch_string_offset(node_value[x])
    emit_word(n)
  } else if (node_type[x] == "nd_Assign") {
    n = fetch_var_offset(node_value[node_left[x]])
    code_gen(node_right[x])
    emit_byte(STORE)
    emit_word(n)
  } else if (node_type[x] == "nd_If") {
    code_gen(node_left[x])        # expr
    emit_byte(JZ)                 # if false, jump
    p1 = hole()                   # make room for jump dest
    code_gen(node_left[node_right[x]])        # if true statements
    if (node_right[node_right[x]] != 0) {
      emit_byte(JMP)            # jump over else statements
      p2 = hole()
    }
    emit_word_at(p1, next_free_code_index - p1)
    if (node_right[node_right[x]] != 0) {
      code_gen(node_right[node_right[x]])   # else statements
      emit_word_at(p2, next_free_code_index - p2)
    }
  } else if (node_type[x] == "nd_While") {
    p1 =next_free_code_index
    code_gen(node_left[x])
    emit_byte(JZ)
    p2 = hole()
    code_gen(node_right[x])
    emit_byte(JMP)                       # jump back to the top
    emit_word(p1 - next_free_code_index)
    emit_word_at(p2, next_free_code_index - p2)
  } else if (node_type[x] == "nd_Sequence") {
    code_gen(node_left[x])
    code_gen(node_right[x])
  } else if (node_type[x] == "nd_Prtc") {
    code_gen(node_left[x])
    emit_byte(PRTC)
  } else if (node_type[x] == "nd_Prti") {
    code_gen(node_left[x])
    emit_byte(PRTI)
  } else if (node_type[x] == "nd_Prts") {
    code_gen(node_left[x])
    emit_byte(PRTS)
  } else if (node_type[x] in operators) {
    code_gen(node_left[x])
    code_gen(node_right[x])
    emit_byte(operators[node_type[x]])
  } else if (node_type[x] in unary_operators) {
    code_gen(node_left[x])
    emit_byte(unary_operators[node_type[x]])
  } else {
    error("error in code generator - found '" node_type[x] "', expecting operator")
  }
}

function code_finish() {
  emit_byte(HALT)
}

function list_code() {
  printf("Datasize: %d Strings: %d\n", globals_n, string_n)
  # Make sure that arrays are sorted by value in ascending order.
  PROCINFO["sorted_in"] =  "@val_str_asc"
  # This is a dependency on GAWK.
  for (k in string_pool)
    print(k)
  pc = 0
  while (pc < next_free_code_index) {
    printf("%4d ", pc)
    op = code[pc]
    pc += 1
    if (op == FETCH) {
      x = bytes_to_int(pc)
      printf("fetch [%d]\n", x);
      pc += word_size
    } else if (op == STORE) {
      x = bytes_to_int(pc)
      printf("store [%d]\n", x);
      pc += word_size
    } else if (op == PUSH) {
      x = bytes_to_int(pc)
      printf("push  %d\n", x);
      pc += word_size
    } else if (op == ADD)  {  print("add")
    } else if (op == SUB)  {  print("sub")
    } else if (op == MUL)  {  print("mul")
    } else if (op == DIV)  {  print("div")
    } else if (op == MOD)  {  print("mod")
    } else if (op == LT)   {  print("lt")
    } else if (op == GT)   {  print("gt")
    } else if (op == LE)   {  print("le")
    } else if (op == GE)   {  print("ge")
    } else if (op == EQ)   {  print("eq")
    } else if (op == NE)   {  print("ne")
    } else if (op == AND)  {  print("and")
    } else if (op == OR)   {  print("or")
    } else if (op == NEG)  {  print("neg")
    } else if (op == NOT)  {  print("not")
    } else if (op == JMP)  {
      x = bytes_to_int(pc)
      printf("jmp    (%d) %d\n", x, pc + x);
      pc += word_size
    } else if (op == JZ)  {
      x = bytes_to_int(pc)
      printf("jz     (%d) %d\n", x, pc + x);
      pc += word_size
    } else if (op == PRTC) { print("prtc")
    } else if (op == PRTI) { print("prti")
    } else if (op == PRTS) { print("prts")
    } else if (op == HALT) { print("halt")
    } else                 { error("list_code: Unknown opcode '" op "'")
    }
  } # while pc
}

function load_ast(        line, line_list, text, n, node_type, value, left, right) {
  getline line
  n=split(line, line_list)
  text = line_list[1]
  if (text == ";")
    return 0
  node_type = all_syms[text]
  if (n > 1) {
    value = line_list[2]
    for (i=3;i<=n;i++)
      value = value " " line_list[i]
    if (value ~ /^[0-9]+$/)
      value = int(value)
    return make_leaf(node_type, value)
  }
  left = load_ast()
  right = load_ast()
  return make_node(node_type, left, right)
}

BEGIN {
  all_syms["Identifier"  ] = "nd_Ident"
  all_syms["String"      ] = "nd_String"
  all_syms["Integer"     ] = "nd_Integer"
  all_syms["Sequence"    ] = "nd_Sequence"
  all_syms["If"          ] = "nd_If"
  all_syms["Prtc"        ] = "nd_Prtc"
  all_syms["Prts"        ] = "nd_Prts"
  all_syms["Prti"        ] = "nd_Prti"
  all_syms["While"       ] = "nd_While"
  all_syms["Assign"      ] = "nd_Assign"
  all_syms["Negate"      ] = "nd_Negate"
  all_syms["Not"         ] = "nd_Not"
  all_syms["Multiply"    ] = "nd_Mul"
  all_syms["Divide"      ] = "nd_Div"
  all_syms["Mod"         ] = "nd_Mod"
  all_syms["Add"         ] = "nd_Add"
  all_syms["Subtract"    ] = "nd_Sub"
  all_syms["Less"        ] = "nd_Lss"
  all_syms["LessEqual"   ] = "nd_Leq"
  all_syms["Greater"     ] = "nd_Gtr"
  all_syms["GreaterEqual"] = "nd_Geq"
  all_syms["Equal"       ] = "nd_Eql"
  all_syms["NotEqual"    ] = "nd_Neq"
  all_syms["And"         ] = "nd_And"
  all_syms["Or"          ] = "nd_Or"

  FETCH=1; STORE=2; PUSH=3; ADD=4; SUB=5; MUL=6;
  DIV=7; MOD=8; LT=9; GT=10; LE=11; GE=12;
  EQ=13; NE=14; AND=15; OR=16; NEG=17; NOT=18;
  JMP=19; JZ=20; PRTC=21; PRTS=22; PRTI=23; HALT=24;

  operators["nd_Lss"] = LT
  operators["nd_Gtr"] = GT
  operators["nd_Leq"] = LE
  operators["nd_Geq"] = GE
  operators["nd_Eql"] = EQ
  operators["nd_Neq"] = NE
  operators["nd_And"] = AND
  operators["nd_Or" ] = OR
  operators["nd_Sub"] = SUB
  operators["nd_Add"] = ADD
  operators["nd_Div"] = DIV
  operators["nd_Mul"] = MUL
  operators["nd_Mod"] = MOD

  unary_operators["nd_Negate"] = NEG
  unary_operators["nd_Not"   ] = NOT

  next_free_node_index = 1
  next_free_code_index = 0
  globals_n   = 0
  string_n    = 0
  word_size   = 4
  input_file = "-"

  if (ARGC > 1)
    input_file = ARGV[1]
  n = load_ast()
  code_gen(n)
  code_finish()
  list_code()
}

```

<b>

```txt
Datasize: 1 Strings: 2
"count is: "
"\n"
   0 push  1
   5 store [0]
  10 fetch [0]
  15 push  10
  20 lt
  21 jz     (43) 65
  26 push  0
  31 prts
  32 fetch [0]
  37 prti
  38 push  1
  43 prts
  44 fetch [0]
  49 push  1
  54 add
  55 store [0]
  60 jmp    (-51) 10
  65 halt
```

</b>


## C

Tested with gcc 4.81 and later, compiles warning free with -Wall -Wextra

```cpp
#include <iostream>
#include <stdio.h>
#include <string.h>
#include <stdarg.h>
#include <stdint.h>
#include <ctype.h>

typedef unsigned char uchar;

typedef enum {
    nd_Ident, nd_String, nd_Integer, nd_Sequence, nd_If, nd_Prtc, nd_Prts, nd_Prti, nd_While,
    nd_Assign, nd_Negate, nd_Not, nd_Mul, nd_Div, nd_Mod, nd_Add, nd_Sub, nd_Lss, nd_Leq,
    nd_Gtr, nd_Geq, nd_Eql, nd_Neq, nd_And, nd_Or
} NodeType;

typedef enum { FETCH, STORE, PUSH, ADD, SUB, MUL, DIV, MOD, LT, GT, LE, GE, EQ, NE, AND,
    OR, NEG, NOT, JMP, JZ, PRTC, PRTS, PRTI, HALT
} Code_t;

typedef uchar code;

typedef struct Tree {
    NodeType node_type;
    struct Tree *left;
    struct Tree *right;
    char *value;
} Tree;

#define da_dim(name, type)  type *name = NULL;          \
                            int _qy_ ## name ## _p = 0;  \
                            int _qy_ ## name ## _max = 0

#define da_redim(name)      do {if (_qy_ ## name ## _p >= _qy_ ## name ## _max) \
                                name = realloc(name, (_qy_ ## name ## _max += 32) * sizeof(name[0]));} while (0)

#define da_rewind(name)     _qy_ ## name ## _p = 0

#define da_append(name, x)  do {da_redim(name); name[_qy_ ## name ## _p++] = x;} while (0)
#define da_len(name)        _qy_ ## name ## _p
#define da_add(name)        do {da_redim(name); _qy_ ## name ## _p++;} while (0)

FILE *source_fp, *dest_fp;
static int here;
da_dim(object, code);
da_dim(globals, const char *);
da_dim(string_pool, const char *);

// dependency: Ordered by NodeType, must remain in same order as NodeType enum
struct {
    char       *enum_text;
    NodeType   node_type;
    Code_t     opcode;
} atr[] = {
    {"Identifier"  , nd_Ident,    -1 },
    {"String"      , nd_String,   -1 },
    {"Integer"     , nd_Integer,  -1 },
    {"Sequence"    , nd_Sequence, -1 },
    {"If"          , nd_If,       -1 },
    {"Prtc"        , nd_Prtc,     -1 },
    {"Prts"        , nd_Prts,     -1 },
    {"Prti"        , nd_Prti,     -1 },
    {"While"       , nd_While,    -1 },
    {"Assign"      , nd_Assign,   -1 },
    {"Negate"      , nd_Negate,   NEG},
    {"Not"         , nd_Not,      NOT},
    {"Multiply"    , nd_Mul,      MUL},
    {"Divide"      , nd_Div,      DIV},
    {"Mod"         , nd_Mod,      MOD},
    {"Add"         , nd_Add,      ADD},
    {"Subtract"    , nd_Sub,      SUB},
    {"Less"        , nd_Lss,      LT },
    {"LessEqual"   , nd_Leq,      LE },
    {"Greater"     , nd_Gtr,      GT },
    {"GreaterEqual", nd_Geq,      GE },
    {"Equal"       , nd_Eql,      EQ },
    {"NotEqual"    , nd_Neq,      NE },
    {"And"         , nd_And,      AND},
    {"Or"          , nd_Or,       OR },
};

void error(const char *fmt, ... ) {
    va_list ap;
    char buf[1000];

    va_start(ap, fmt);
    vsprintf(buf, fmt, ap);
    va_end(ap);
    printf("error: %s\n", buf);
    exit(1);
}

Code_t type_to_op(NodeType type) {
    return atr[type].opcode;
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

/*** Code generator ***/

void emit_byte(int c) {
    da_append(object, (uchar)c);
    ++here;
}

void emit_int(int32_t n) {
    union {
        int32_t n;
        unsigned char c[sizeof(int32_t)];
    } x;

    x.n = n;

    for (size_t i = 0; i < sizeof(x.n); ++i) {
        emit_byte(x.c[i]);
    }
}

int hole() {
    int t = here;
    emit_int(0);
    return t;
}

void fix(int src, int dst) {
    *(int32_t *)(object + src) = dst-src;
}

int fetch_var_offset(const char *id) {
    for (int i = 0; i < da_len(globals); ++i) {
        if (strcmp(id, globals[i]) == 0)
            return i;
    }
    da_add(globals);
    int n = da_len(globals) - 1;
    globals[n] = strdup(id);
    return n;
}

int fetch_string_offset(const char *st) {
    for (int i = 0; i < da_len(string_pool); ++i) {
        if (strcmp(st, string_pool[i]) == 0)
            return i;
    }
    da_add(string_pool);
    int n = da_len(string_pool) - 1;
    string_pool[n] = strdup(st);
    return n;
}

void code_gen(Tree *x) {
    int p1, p2, n;

    if (x == NULL) return;
    switch (x->node_type) {
        case nd_Ident:
            emit_byte(FETCH);
            n = fetch_var_offset(x->value);
            emit_int(n);
            break;
        case nd_Integer:
            emit_byte(PUSH);
            emit_int(atoi(x->value));
            break;
        case nd_String:
            emit_byte(PUSH);
            n = fetch_string_offset(x->value);
            emit_int(n);
            break;
        case nd_Assign:
            n = fetch_var_offset(x->left->value);
            code_gen(x->right);
            emit_byte(STORE);
            emit_int(n);
            break;
        case nd_If:
            code_gen(x->left);        // if expr
            emit_byte(JZ);                  // if false, jump
            p1 = hole();                    // make room for jump dest
            code_gen(x->right->left);   // if true statements
            if (x->right->right != NULL) {
                emit_byte(JMP);
                p2 = hole();
            }
            fix(p1, here);
            if (x->right->right != NULL) {
                code_gen(x->right->right);
                fix(p2, here);
            }
            break;
        case nd_While:
            p1 = here;
            code_gen(x->left);        // while expr
            emit_byte(JZ);                  // if false, jump
            p2 = hole();                    // make room for jump dest
            code_gen(x->right);       // statements
            emit_byte(JMP);                 // back to the top
            fix(hole(), p1);                // plug the top
            fix(p2, here);                  // plug the 'if false, jump'
            break;
        case nd_Sequence:
            code_gen(x->left);
            code_gen(x->right);
            break;
        case nd_Prtc:
            code_gen(x->left);
            emit_byte(PRTC);
            break;
        case nd_Prti:
            code_gen(x->left);
            emit_byte(PRTI);
            break;
        case nd_Prts:
            code_gen(x->left);
            emit_byte(PRTS);
            break;
        case nd_Lss: case nd_Gtr: case nd_Leq: case nd_Geq: case nd_Eql: case nd_Neq:
        case nd_And: case nd_Or: case nd_Sub: case nd_Add: case nd_Div: case nd_Mul:
        case nd_Mod:
            code_gen(x->left);
            code_gen(x->right);
            emit_byte(type_to_op(x->node_type));
            break;
        case nd_Negate: case nd_Not:
            code_gen(x->left);
            emit_byte(type_to_op(x->node_type));
            break;
        default:
            error("error in code generator - found %d, expecting operator\n", x->node_type);
    }
}

void code_finish() {
    emit_byte(HALT);
}

void list_code() {
    fprintf(dest_fp, "Datasize: %d Strings: %d\n", da_len(globals), da_len(string_pool));
    for (int i = 0; i < da_len(string_pool); ++i)
        fprintf(dest_fp, "%s\n", string_pool[i]);

    code *pc = object;

    again: fprintf(dest_fp, "%5d ", (int)(pc - object));
    switch (*pc++) {
        case FETCH: fprintf(dest_fp, "fetch [%d]\n", *(int32_t *)pc);
                    pc += sizeof(int32_t);  goto again;
        case STORE: fprintf(dest_fp, "store [%d]\n", *(int32_t *)pc);
                    pc += sizeof(int32_t);  goto again;
        case PUSH : fprintf(dest_fp, "push  %d\n", *(int32_t *)pc);
                    pc += sizeof(int32_t);    goto again;
        case ADD  : fprintf(dest_fp, "add\n");      goto again;
        case SUB  : fprintf(dest_fp, "sub\n");      goto again;
        case MUL  : fprintf(dest_fp, "mul\n");      goto again;
        case DIV  : fprintf(dest_fp, "div\n");      goto again;
        case MOD  : fprintf(dest_fp, "mod\n");      goto again;
        case LT   : fprintf(dest_fp, "lt\n");       goto again;
        case GT   : fprintf(dest_fp, "gt\n");       goto again;
        case LE   : fprintf(dest_fp, "le\n");       goto again;
        case GE   : fprintf(dest_fp, "ge\n");       goto again;
        case EQ   : fprintf(dest_fp, "eq\n");       goto again;
        case NE   : fprintf(dest_fp, "ne\n");       goto again;
        case AND  : fprintf(dest_fp, "and\n");      goto again;
        case OR   : fprintf(dest_fp, "or\n");       goto again;
        case NOT  : fprintf(dest_fp, "not\n");      goto again;
        case NEG  : fprintf(dest_fp, "neg\n");      goto again;
        case JMP  : fprintf(dest_fp, "jmp    (%d) %d\n",
                        *(int32_t *)pc, (int32_t)(pc + *(int32_t *)pc - object));
                    pc += sizeof(int32_t); goto again;
        case JZ   : fprintf(dest_fp, "jz     (%d) %d\n",
                        *(int32_t *)pc, (int32_t)(pc + *(int32_t *)pc - object));
                    pc += sizeof(int32_t); goto again;
        case PRTC : fprintf(dest_fp, "prtc\n");     goto again;
        case PRTI : fprintf(dest_fp, "prti\n");     goto again;
        case PRTS : fprintf(dest_fp, "prts\n");     goto again;
        case HALT : fprintf(dest_fp, "halt\n");     break;
        default:error("listcode:Unknown opcode %d\n", *(pc - 1));
    }
}

void init_io(FILE **fp, FILE *std, const char mode[], const char fn[]) {
    if (fn[0] == '\0')
        *fp = std;
    else if ((*fp = fopen(fn, mode)) == NULL)
        error(0, 0, "Can't open %s\n", fn);
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
        for (++p; isspace(*p); ++p)
            ;
        return make_leaf(node_type, p);
    }

    Tree *left  = load_ast();
    Tree *right = load_ast();
    return make_node(node_type, left, right);
}

int main(int argc, char *argv[]) {
    init_io(&source_fp, stdin,  "r",  argc > 1 ? argv[1] : "");
    init_io(&dest_fp,   stdout, "wb", argc > 2 ? argv[2] : "");

    code_gen(load_ast());
    code_finish();
    list_code();

    return 0;
}
```


<b>

```txt
Datasize: 1 Strings: 2
"count is: "
"\n"
    0 push  1
    5 store [0]
   10 fetch [0]
   15 push  10
   20 lt
   21 jz     (43) 65
   26 push  0
   31 prts
   32 fetch [0]
   37 prti
   38 push  1
   43 prts
   44 fetch [0]
   49 push  1
   54 add
   55 store [0]
   60 jmp    (-51) 10
   65 halt
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
program-id. generator.
environment division.
configuration section.
repository.  function all intrinsic.
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
    03  t-lim pic 999 value 998.
    03  filler occurs 998.
        05  p1 pic 999.
        05  p2 pic 999.
        05  p3 pic 999.
        05  n1 pic 999.
        05  leaf.
            07  leaf-type pic x(14).
            07  leaf-value pic x(48).
        05  node redefines leaf.
            07  node-type pic x(14).
            07  node-left pic 999.
            07  node-right pic 999.

01  opcodes global.
    03  opFETCH pic x value x'00'.
    03  opSTORE pic x value x'01'.
    03  opPUSH  pic x value x'02'.
    03  opADD   pic x value x'03'.
    03  opSUB   pic x value x'04'.
    03  opMUL   pic x value x'05'.
    03  opDIV   pic x value x'06'.
    03  opMOD   pic x value x'07'.
    03  opLT    pic x value x'08'.
    03  opGT    pic x value x'09'.
    03  opLE    pic x value x'0A'.
    03  opGE    pic x value x'0B'.
    03  opEQ    pic x value x'0C'.
    03  opNE    pic x value x'0D'.
    03  opAND   pic x value x'0E'.
    03  opOR    pic x value x'0F'.
    03  opNEG   pic x value x'10'.
    03  opNOT   pic x value x'11'.
    03  opJMP   pic x value x'13'.
    03  opJZ    pic x value x'14'.
    03  opPRTC  pic x value x'15'.
    03  opPRTS  pic x value x'16'.
    03  opPRTI  pic x value x'17'.
    03  opHALT  pic x value x'18'.

01  variables global.
    03  v pic 99.
    03  v-max pic 99 value 0.
    03  v-lim pic 99 value 16.
    03  variable-entry occurs 16 pic x(48).

01  strings global.
    03  s pic 99.
    03  s-max pic 99 value 0.
    03  s-lim pic 99 value 16.
    03  string-entry occurs 16 pic x(48).

01  generated-code global.
    03  c  pic 999 value 1.
    03  c1 pic 999.
    03  c-lim pic 999 value 512.
    03  kode pic x(512).

procedure division chaining program-name.
start-generator.
    call 'loadast'
    if program-name <> spaces
        call 'readinput' *> close input-file
    end-if
    >>d perform print-ast
    call 'codegen' using t
    call 'emitbyte' using opHALT
    >>d call 'showhex' using kode c
    call 'listcode'
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
program-id. codegen common recursive.
data division.
working-storage section.
01  r pic ---9.
linkage section.
01  n pic 999.
procedure division using n.
start-codegen.
    if n = 0
        exit program
    end-if
    >>d display 'at 'c ' node=' space n space node-type(n) upon syserr
    evaluate node-type(n)
    when 'Identifier'
        call 'emitbyte' using opFetch
        call 'variableoffset' using leaf-value(n)
        call 'emitword' using v '0'
    when 'Integer'
        call 'emitbyte' using opPUSH
        call 'emitword' using leaf-value(n) '0'
    when 'String'
        call 'emitbyte' using opPUSH
        call 'stringoffset' using leaf-value(n)
        call 'emitword' using s '0'
    when 'Assign'
        call 'codegen' using node-right(n)
        call 'emitbyte' using opSTORE
        move node-left(n) to n1(n)
        call 'variableoffset' using leaf-value(n1(n))
        call 'emitword' using v '0'
    when 'If'
        call 'codegen' using node-left(n)          *> conditional expr
        call 'emitbyte' using opJZ                 *> jump to false path or exit
        move c to p1(n)
        call 'emitword' using '0' '0'
        move node-right(n) to n1(n)                *> true path
        call 'codegen' using node-left(n1(n))
        if node-right(n1(n)) <> 0                  *> there is a false path
            call 'emitbyte' using opJMP            *> jump past false path
            move c to p2(n)
            call 'emitword' using '0' '0'
            compute r = c - p1(n)                  *> fill in jump to false path
            call 'emitword' using r p1(n)
            call 'codegen' using node-right(n1(n)) *> false path
            compute r = c - p2(n)                  *> fill in jump to exit
            call 'emitword' using r p2(n)
        else
            compute r = c - p1(n)
            call 'emitword' using r p1(n)          *> fill in jump to exit
        end-if
    when 'While'
        move c to p3(n)                            *> save address of while start
        call 'codegen' using node-left(n)          *> conditional expr
        call 'emitbyte' using opJZ                 *> jump to exit
        move c to p2(n)
        call 'emitword' using '0' '0'
        call 'codegen' using node-right(n)         *> while body
        call 'emitbyte' using opJMP                *> jump to while start
        compute r = p3(n) - c
        call 'emitword' using r '0'
        compute r = c - p2(n)                      *> fill in jump to exit
        call 'emitword' using r p2(n)
    when 'Sequence'
        call 'codegen' using node-left(n)
        call 'codegen' using node-right(n)
    when 'Prtc'
        call 'codegen' using node-left(n)
        call 'emitbyte' using opPRTC
    when 'Prti'
        call 'codegen' using node-left(n)
        call 'emitbyte' using opPRTI
    when 'Prts'
        call 'codegen' using node-left(n)
        call 'emitbyte' using opPRTS
    when 'Less'
        call 'codegen' using node-left(n)
        call 'codegen' using node-right(n)
        call 'emitbyte' using opLT
    when 'Greater'
        call 'codegen' using node-left(n)
        call 'codegen' using node-right(n)
        call 'emitbyte' using opGT
    when 'LessEqual'
        call 'codegen' using node-left(n)
        call 'codegen' using node-right(n)
        call 'emitbyte' using opLE
    when 'GreaterEqual'
        call 'codegen' using node-left(n)
        call 'codegen' using node-right(n)
        call 'emitbyte' using opGE
    when 'Equal'
        call 'codegen' using node-left(n)
        call 'codegen' using node-right(n)
        call 'emitbyte' using opEQ
    when 'NotEqual'
        call 'codegen' using node-left(n)
        call 'codegen' using node-right(n)
        call 'emitbyte' using opNE
    when 'And'
        call 'codegen' using node-left(n)
        call 'codegen' using node-right(n)
        call 'emitbyte' using opAND
    when 'Or'
        call 'codegen' using node-left(n)
        call 'codegen' using node-right(n)
        call 'emitbyte' using opOR
    when 'Subtract'
        call 'codegen' using node-left(n)
        call 'codegen' using node-right(n)
        call 'emitbyte' using opSUB
    when 'Add'
        call 'codegen' using node-left(n)
        call 'codegen' using node-right(n)
        call 'emitbyte' using opADD
    when 'Divide'
        call 'codegen' using node-left(n)
        call 'codegen' using node-right(n)
        call 'emitbyte' using opDIV
    when 'Multiply'
        call 'codegen' using node-left(n)
        call 'codegen' using node-right(n)
        call 'emitbyte' using opMUL
    when 'Mod'
        call 'codegen' using node-left(n)
        call 'codegen' using node-right(n)
        call 'emitbyte' using opMOD
    when 'Negate'
       call 'codegen' using node-left(n)
       call 'emitbyte' using opNEG
    when 'Not'
        call 'codegen' using node-left(n)
        call 'emitbyte' using opNOT
    when other
        string 'in generator unknown node type: ' node-type(n) into error-record
        call 'reporterror'
    end-evaluate
    .
end program codegen.

identification division.
program-id. variableoffset common.
data division.
linkage section.
01  variable-value pic x(48).
procedure division using variable-value.
start-variableoffset.
    perform varying v from 1 by 1
    until v > v-max
    or variable-entry(v) = variable-value
        continue
    end-perform
    if v > v-lim
        string 'in generator variable offset v exceeds ' v-lim into error-record
        call 'reporterror'
    end-if
    if v > v-max
        move v to v-max
        move variable-value to variable-entry(v)
    end-if
    .
end program variableoffset.

identification division.
program-id. stringoffset common.
data division.
linkage section.
01  string-value pic x(48).
procedure division using string-value.
start-stringoffset.
    perform varying s from 1 by 1
    until s > s-max
    or string-entry(s) = string-value
        continue
    end-perform
    if s > s-lim
        string ' generator stringoffset s exceeds ' s-lim into error-record
        call 'reporterror'
    end-if
    if s > s-max
        move s to s-max
        move string-value to string-entry(s)
    end-if
    subtract 1 from s *> convert index to offset
    .
end program stringoffset.

identification division.
program-id. emitbyte common.
data division.
linkage section.
01  opcode pic x.
procedure division using opcode.
start-emitbyte.
    if c >= c-lim
        string 'in generator emitbyte c exceeds ' c-lim into error-record
        call 'reporterror'
    end-if
    move opcode to kode(c:1)
    add 1 to c
    .
end program emitbyte.

identification division.
program-id. emitword common.
data division.
working-storage section.
01  word-x.
    03  word usage binary-int.
01  loc pic 999.
linkage section.
01  word-value any length.
01  loc-value any length.
procedure division using word-value loc-value.
start-emitword.
    if c + length(word) > c-lim
        string 'in generator emitword exceeds ' c-lim into error-record
        call 'reporterror'
    end-if
    move numval(word-value) to word
    move numval(loc-value) to loc
    if loc = 0
        move word-x to kode(c:length(word))
        add length(word) to c
    else
        move word-x to kode(loc:length(word))
    end-if
    .
end program emitword.

identification division.
program-id. listcode common.
data division.
working-storage section.
01  word-x.
    03  word usage binary-int.
01  address-display pic ---9.
01  address-absolute pic zzz9.
01  data-display pic -(9)9.
01  v-display pic z9.
01  s-display pic z9.
01  c-display pic zzz9.
procedure division.
start-listcode.
    move v-max to v-display
    move s-max to s-display
    display 'Datasize: ' trim(v-display) space 'Strings: ' trim(s-display)

    perform varying s from 1 by 1
    until s > s-max
        display string-entry(s)
    end-perform

    move 1 to c1
    perform until c1 >= c
        compute c-display = c1 - 1
        display c-display space with no advancing
        evaluate kode(c1:1)
        when opFETCH
            add 1 to c1
            move kode(c1:4) to word-x
            compute address-display = word - 1
            display 'fetch [' trim(address-display) ']'
            add 3 to c1
        when opSTORE
            add 1 to c1
            move kode(c1:4) to word-x
            compute address-display = word - 1
            display 'store [' trim(address-display) ']'
            add 3 to c1
        when opPUSH
            add 1 to c1
            move kode(c1:4) to word-x
            move word to data-display
            display 'push  ' trim(data-display)
            add 3 to c1
        when opADD   display 'add'
        when opSUB   display 'sub'
        when opMUL   display 'mul'
        when opDIV   display 'div'
        when opMOD   display 'mod'
        when opLT    display 'lt'
        when opGT    display 'gt'
        when opLE    display 'le'
        when opGE    display 'ge'
        when opEQ    display 'eq'
        when opNE    display 'ne'
        when opAND   display 'and'
        when opOR    display 'or'
        when opNEG   display 'neg'
        when opNOT   display 'not'
        when opJMP
            move kode(c1 + 1:length(word)) to word-x
            move word to address-display
            compute address-absolute = c1 + word
            display 'jmp    (' trim(address-display) ') ' trim(address-absolute)
            add length(word) to c1
        when opJZ
            move kode(c1 + 1:length(word)) to word-x
            move word to address-display
            compute address-absolute = c1 + word
            display 'jz     (' trim(address-display) ') ' trim(address-absolute)
            add length(word) to c1
        when opPRTC  display 'prtc'
        when opPRTI  display 'prti'
        when opPRTS  display 'prts'
        when opHALT  display 'halt'
        when other
            string 'in generator unknown opcode ' kode(c1:1) into error-record
            call 'reporterror'
        end-evaluate
        add 1 to c1
    end-perform
    .
end program listcode.

identification division.
program-id. loadast common recursive.
procedure division.
start-loadast.
    if l >= l-lim
        string 'in generator loadast l exceeds ' l-lim into error-record
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
program-id. makenode common.
data division.
linkage section.
01  parm-type any length.
01  parm-l-left pic 999.
01  parm-l-right pic 999.
procedure division using parm-type parm-l-left parm-l-right.
start-makenode.
    if t >= t-lim
        string 'in generator makenode t exceeds ' t-lim into error-record
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
        string 'in generator makeleaf t exceeds ' t-lim into error-record
        call 'reporterror'
    end-if
    move parm-type to leaf-type(t)
    move parm-value to leaf-value(t)
    .
end program makeleaf.

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
            string 'in generator ' trim(input-name) ' not found' into error-record
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
        string 'in generator ' trim(input-name) ' unexpected input-status: ' input-status
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

identification division.
program-id. showhex common.

data division.
working-storage section.
01  hex.
    03  filler pic x(32) value '000102030405060708090A0B0C0D0E0F'.
    03  filler pic x(32) value '101112131415161718191A1B1C1D1E1F'.
    03  filler pic x(32) value '202122232425262728292A2B2C2D2E2F'.
    03  filler pic x(32) value '303132333435363738393A3B3C3D3E3F'.
    03  filler pic x(32) value '404142434445464748494A4B4C4D4E4F'.
    03  filler pic x(32) value '505152535455565758595A5B5C5D5E5F'.
    03  filler pic x(32) value '606162636465666768696A6B6C6D6E6F'.
    03  filler pic x(32) value '707172737475767778797A7B7C7D7E7F'.
    03  filler pic x(32) value '808182838485868788898A8B8C8D8E8F'.
    03  filler pic x(32) value '909192939495969798999A9B9C9D9E9F'.
    03  filler pic x(32) value 'A0A1A2A3A4A5A6A7A8A9AAABACADAEAF'.
    03  filler pic x(32) value 'B0B1B2B3B4B5B6B7B8B9BABBBCBDBEBF'.
    03  filler pic x(32) value 'C0C1C2C3C4C5C6C7C8C9CACBCCCDCECF'.
    03  filler pic x(32) value 'D0D1D2D3D4D5D6D7D8D9DADBDCDDDEDF'.
    03  filler pic x(32) value 'E0E1E2E3E4E5E6E7E8E9EAEBECEDEEEF'.
    03  filler pic x(32) value 'F0F1F2F3F4F5F6F7F8F9FAFBFCFDFEFF'.

01  cdx pic 9999.
01  bdx pic 999.
01  byte-count pic 9.
01  bytes-per-word pic 9 value 4.
01  word-count pic 9.
01  words-per-line pic 9 value 8.

linkage section.
01  data-field any length.
01  length-data-field pic 999.

procedure division using
    by reference data-field
    by reference length-data-field.
start-showhex.
    move 1 to byte-count
    move 1 to word-count
    perform varying cdx from 1 by 1
    until cdx > length-data-field
         compute bdx = 2 * ord(data-field(cdx:1)) - 1 end-compute
         display hex(bdx:2) with no advancing upon syserr
         add 1 to byte-count end-add
         if byte-count > bytes-per-word
             display ' ' with no advancing upon syserr
             move 1 to byte-count
             add 1 to word-count end-add
         end-if
         if word-count > words-per-line
             display ' ' upon syserr
             move 1 to word-count
         end-if
    end-perform
    if word-count <> 1
    or byte-count <> 1
        display ' ' upon syserr
    end-if
    display ' ' upon syserr
    goback
    .
end program showhex.
end program generator.
```


```txt
prompt$ ./lexer <testcases/Count | ./parser | ./generator
Datasize: 1 Strings: 2
"count is: "
"\n"
   0 push  1
   5 store [0]
  10 fetch [0]
  15 push  10
  20 lt
  21 jz     (43) 65
  26 push  0
  31 prts
  32 fetch [0]
  37 prti
  38 push  1
  43 prts
  44 fetch [0]
  49 push  1
  54 add
  55 store [0]
  60 jmp    (-51) 10
  65 halt
```



## Forth

Tested with Gforth 0.7.3

```Forth
CREATE BUF 0 ,
: PEEK   BUF @ 0= IF KEY BUF ! THEN BUF @ ;
: GETC   PEEK  0 BUF ! ;
: SPACE?   DUP BL = SWAP 9 14 WITHIN OR ;
: >SPACE   BEGIN PEEK SPACE? WHILE GETC DROP REPEAT ;
: DIGIT?   48 58 WITHIN ;
: >Integer   >SPACE  0
   BEGIN  PEEK DIGIT?
   WHILE  GETC [CHAR] 0 -  SWAP 10 * +  REPEAT ;
: SKIP ( xt --)
   BEGIN PEEK OVER EXECUTE WHILE GETC DROP REPEAT DROP ;
: WORD ( xt -- c-addr)  DUP >R SKIP  PAD 1+
   BEGIN PEEK R@ EXECUTE INVERT
   WHILE GETC OVER C! CHAR+
   REPEAT  R> SKIP  PAD TUCK - 1-  PAD C! ;
: INTERN ( c-addr -- c-addr)
   HERE TUCK OVER C@ CHAR+ DUP ALLOT CMOVE ;
: "?   [CHAR] " = ;
: "TYPE"   [CHAR] " EMIT  TYPE  [CHAR] " EMIT ;
: .   0 .R ;
: 3@ ( addr -- w3 w2 w1)
   [ 2 CELLS ]L + DUP @ SWAP CELL - DUP @ SWAP CELL - @ ;

CREATE BUF' 12 ALLOT
: PREPEND ( c-addr c -- c-addr)  BUF' 1+ C!
   COUNT 10 MIN DUP 1+ BUF' C!  BUF' 2 + SWAP CMOVE  BUF' ;
: >NODE ( c-addr -- n)   [CHAR] $ PREPEND  FIND
   IF EXECUTE ELSE ." unrecognized node " COUNT TYPE CR THEN ;
: NODE ( n left right -- addr)  HERE >R , , , R> ;

: CONS ( a b l -- l)  HERE >R , , , R> ;
: FIRST ( l -- a)  [ 2 CELLS ]L + @ ;
: SECOND ( l -- b)  CELL+ @ ;
: C=? ( c-addr1 c-addr2 -- t|f)  COUNT ROT COUNT COMPARE 0= ;
: LOOKUP ( c-addr l -- n t | c-addr f)
   BEGIN DUP WHILE OVER OVER FIRST C=?
     IF NIP SECOND TRUE EXIT THEN  @
   REPEAT  DROP FALSE ;

CREATE GLOBALS 0 ,  CREATE STRINGS 0 ,
: DEPTH ( pool -- n)  DUP IF SECOND 1+ THEN ;
: FISH ( c-addr pool -- n pool') TUCK LOOKUP  IF SWAP
   ELSE INTERN OVER DEPTH ROT OVER >R CONS  R> SWAP THEN ;
: >Identifier   ['] SPACE? WORD GLOBALS @ FISH GLOBALS ! ;
: >String       ['] "? WORD STRINGS @ FISH STRINGS ! ;
: >;   0 ;
: HANDLER   [CHAR] @ PREPEND  FIND DROP ;
: READER ( c-addr -- xt t | f)
   [CHAR] > PREPEND  FIND  DUP 0= IF NIP THEN ;
DEFER GETAST
: READ ( c-addr -- right left)  READER
   IF EXECUTE 0 ELSE GETAST GETAST THEN SWAP ;
: (GETAST)   ['] SPACE? WORD  DUP HANDLER >R  READ  R> NODE ;
' (GETAST) IS GETAST

CREATE PC 0 ,
: i32! ( n addr --)
   OVER           $FF AND OVER C! 1+
   OVER  8 RSHIFT $FF AND OVER C! 1+
   OVER 16 RSHIFT $FF AND OVER C! 1+
   OVER 24 RSHIFT $FF AND OVER C!    DROP DROP ;
: i32, ( n --)  HERE i32!  4 ALLOT  4 PC +! ;
: i8, ( c --)  C,  1 PC +! ;
: i8@+   DUP 1+ SWAP C@  1 PC +! ;
: i32@+ ( addr -- addr+4 n)
   i8@+                 >R  i8@+  8 LSHIFT R> OR >R
   i8@+ 16 LSHIFT R> OR >R  i8@+ 24 LSHIFT R> OR ;

CREATE #OPS 0 ,
: OP:   CREATE #OPS @ ,  1 #OPS +!  DOES> @ ;
OP: fetch  OP: store  OP: push  OP: jmp  OP: jz
OP: prtc   OP: prti   OP: prts  OP: neg  OP: not
OP: add    OP: sub    OP: mul   OP: div  OP: mod
OP: lt     OP: gt     OP: le    OP: ge
OP: eq     OP: ne     OP: and   OP: or   OP: halt

: GEN ( ast --)  3@ EXECUTE ;
: @; ( r l)  DROP DROP ;
: @Identifier   fetch i8, i32, DROP ;
: @Integer    push i8, i32, DROP ;
: @String    push i8, i32, DROP ;
: @Prtc   GEN prtc i8, DROP ;
: @Prti   GEN prti i8, DROP ;
: @Prts   GEN prts i8, DROP ;
: @Not    GEN not i8, DROP ;
: @Negate   GEN neg i8, DROP ;
: @Sequence   GEN GEN ;
: @Assign   CELL+ @ >R GEN  store i8, R> i32, ;
: @While   PC @ SWAP  GEN  jz i8, HERE >R 0 i32,
   SWAP GEN  jmp i8, i32,  PC @ R> i32! ;
: @If   GEN  jz i8, HERE >R 0 i32,
   CELL+ DUP CELL+ @ DUP @ ['] @; = IF DROP @
   ELSE SWAP @ GEN  jmp i8, HERE 0 i32,  PC @ R> i32!  >R
   THEN  GEN PC @ R> i32! ;
: BINARY   >R GEN GEN R> i8, ;
: @Subtract   sub BINARY ;  : @Add            add BINARY ;
: @Mod        mod BINARY ;  : @Multiply       mul BINARY ;
: @Divide     div BINARY ;
: @Less       lt  BINARY ;  : @LessEqual      le  BINARY ;
: @Greater    gt  BINARY ;  : @GreaterEqual   ge  BINARY ;
: @Equal      eq  BINARY ;  : @NotEqual       ne  BINARY ;
: @And        and BINARY ;  : @Or             or  BINARY ;

: REVERSE ( l -- l')  0 SWAP
   BEGIN DUP WHILE TUCK DUP @  ROT ROT  ! REPEAT  DROP ;
: .STRINGS   STRINGS @ REVERSE  BEGIN DUP
   WHILE DUP FIRST COUNT "TYPE" CR @ REPEAT DROP ;
: .HEADER ( --)
   ." Datasize: " GLOBALS @ DEPTH . SPACE
   ." Strings: "  STRINGS @ DEPTH . CR  .STRINGS ;
: GENERATE ( ast -- addr u)
   0 PC ! HERE >R  GEN halt i8,  R> PC @ ;
: ,"   [CHAR] " PARSE TUCK HERE SWAP CMOVE ALLOT ;
CREATE "OPS"
," fetch store push  jmp   jz    prtc  prti  prts  "
," neg   not   add   sub   mul   div   mod   lt    "
," gt    le    ge    eq    ne    and   or    halt  "
: .i32   i32@+ . ;
: .[i32]   [CHAR] [ EMIT .i32 [CHAR] ] EMIT ;
: .off   [CHAR] ( EMIT PC @ >R i32@+ DUP R> - . [CHAR] ) EMIT
    SPACE . ;
CREATE .INT ' .[i32] , ' .[i32] , ' .i32 , ' .off , ' .off ,
: EMIT ( addr u --)  >R 0 PC !
   BEGIN PC @ R@ <
   WHILE PC @ 5 .R SPACE  i8@+
     DUP 6 * "OPS" + 6 TYPE
     DUP 5 < IF CELLS .INT + @ EXECUTE ELSE DROP THEN CR
   REPEAT DROP R> DROP ;
GENERATE EMIT BYE
```

Passes all tests.



## Go

```go
package main

import (
    "bufio"
    "encoding/binary"
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

type code = byte

const (
    fetch code = iota
    store
    push
    add
    sub
    mul
    div
    mod
    lt
    gt
    le
    ge
    eq
    ne
    and
    or
    neg
    not
    jmp
    jz
    prtc
    prts
    prti
    halt
)

type Tree struct {
    nodeType NodeType
    left     *Tree
    right    *Tree
    value    string
}

// dependency: Ordered by NodeType, must remain in same order as NodeType enum
type atr struct {
    enumText string
    nodeType NodeType
    opcode   code
}

var atrs = []atr{
    {"Identifier", ndIdent, 255},
    {"String", ndString, 255},
    {"Integer", ndInteger, 255},
    {"Sequence", ndSequence, 255},
    {"If", ndIf, 255},
    {"Prtc", ndPrtc, 255},
    {"Prts", ndPrts, 255},
    {"Prti", ndPrti, 255},
    {"While", ndWhile, 255},
    {"Assign", ndAssign, 255},
    {"Negate", ndNegate, neg},
    {"Not", ndNot, not},
    {"Multiply", ndMul, mul},
    {"Divide", ndDiv, div},
    {"Mod", ndMod, mod},
    {"Add", ndAdd, add},
    {"Subtract", ndSub, sub},
    {"Less", ndLss, lt},
    {"LessEqual", ndLeq, le},
    {"Greater", ndGtr, gt},
    {"GreaterEqual", ndGeq, ge},
    {"Equal", ndEql, eq},
    {"NotEqual", ndNeq, ne},
    {"And", ndAnd, and},
    {"Or", ndOr, or},
}

var (
    stringPool []string
    globals    []string
    object     []code
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

func nodeType2Op(nodeType NodeType) code {
    return atrs[nodeType].opcode
}

func makeNode(nodeType NodeType, left *Tree, right *Tree) *Tree {
    return &Tree{nodeType, left, right, ""}
}

func makeLeaf(nodeType NodeType, value string) *Tree {
    return &Tree{nodeType, nil, nil, value}
}

/*** Code generator ***/

func emitByte(c code) {
    object = append(object, c)
}

func emitWord(n int) {
    bs := make([]byte, 4)
    binary.LittleEndian.PutUint32(bs, uint32(n))
    for _, b := range bs {
        emitByte(code(b))
    }
}

func emitWordAt(at, n int) {
    bs := make([]byte, 4)
    binary.LittleEndian.PutUint32(bs, uint32(n))
    for i := at; i < at+4; i++ {
        object[i] = code(bs[i-at])
    }
}

func hole() int {
    t := len(object)
    emitWord(0)
    return t
}

func fetchVarOffset(id string) int {
    for i := 0; i < len(globals); i++ {
        if globals[i] == id {
            return i
        }
    }
    globals = append(globals, id)
    return len(globals) - 1
}

func fetchStringOffset(st string) int {
    for i := 0; i < len(stringPool); i++ {
        if stringPool[i] == st {
            return i
        }
    }
    stringPool = append(stringPool, st)
    return len(stringPool) - 1
}

func codeGen(x *Tree) {
    if x == nil {
        return
    }
    var n, p1, p2 int
    switch x.nodeType {
    case ndIdent:
        emitByte(fetch)
        n = fetchVarOffset(x.value)
        emitWord(n)
    case ndInteger:
        emitByte(push)
        n, err = strconv.Atoi(x.value)
        check(err)
        emitWord(n)
    case ndString:
        emitByte(push)
        n = fetchStringOffset(x.value)
        emitWord(n)
    case ndAssign:
        n = fetchVarOffset(x.left.value)
        codeGen(x.right)
        emitByte(store)
        emitWord(n)
    case ndIf:
        codeGen(x.left)       // if expr
        emitByte(jz)          // if false, jump
        p1 = hole()           // make room forjump dest
        codeGen(x.right.left) // if true statements
        if x.right.right != nil {
            emitByte(jmp)
            p2 = hole()
        }
        emitWordAt(p1, len(object)-p1)
        if x.right.right != nil {
            codeGen(x.right.right)
            emitWordAt(p2, len(object)-p2)
        }
    case ndWhile:
        p1 = len(object)
        codeGen(x.left)                // while expr
        emitByte(jz)                   // if false, jump
        p2 = hole()                    // make room for jump dest
        codeGen(x.right)               // statements
        emitByte(jmp)                  // back to the top
        emitWord(p1 - len(object))     // plug the top
        emitWordAt(p2, len(object)-p2) // plug the 'if false, jump'
    case ndSequence:
        codeGen(x.left)
        codeGen(x.right)
    case ndPrtc:
        codeGen(x.left)
        emitByte(prtc)
    case ndPrti:
        codeGen(x.left)
        emitByte(prti)
    case ndPrts:
        codeGen(x.left)
        emitByte(prts)
    case ndLss, ndGtr, ndLeq, ndGeq, ndEql, ndNeq,
        ndAnd, ndOr, ndSub, ndAdd, ndDiv, ndMul, ndMod:
        codeGen(x.left)
        codeGen(x.right)
        emitByte(nodeType2Op(x.nodeType))
    case ndNegate, ndNot:
        codeGen(x.left)
        emitByte(nodeType2Op(x.nodeType))
    default:
        msg := fmt.Sprintf("error in code generator - found %d, expecting operator\n", x.nodeType)
        reportError(msg)
    }
}

func codeFinish() {
    emitByte(halt)
}

func listCode() {
    fmt.Printf("Datasize: %d Strings: %d\n", len(globals), len(stringPool))
    for _, s := range stringPool {
        fmt.Println(s)
    }
    pc := 0
    for pc < len(object) {
        fmt.Printf("%5d ", pc)
        op := object[pc]
        pc++
        switch op {
        case fetch:
            x := int32(binary.LittleEndian.Uint32(object[pc : pc+4]))
            fmt.Printf("fetch [%d]\n", x)
            pc += 4
        case store:
            x := int32(binary.LittleEndian.Uint32(object[pc : pc+4]))
            fmt.Printf("store [%d]\n", x)
            pc += 4
        case push:
            x := int32(binary.LittleEndian.Uint32(object[pc : pc+4]))
            fmt.Printf("push  %d\n", x)
            pc += 4
        case add:
            fmt.Println("add")
        case sub:
            fmt.Println("sub")
        case mul:
            fmt.Println("mul")
        case div:
            fmt.Println("div")
        case mod:
            fmt.Println("mod")
        case lt:
            fmt.Println("lt")
        case gt:
            fmt.Println("gt")
        case le:
            fmt.Println("le")
        case ge:
            fmt.Println("ge")
        case eq:
            fmt.Println("eq")
        case ne:
            fmt.Println("ne")
        case and:
            fmt.Println("and")
        case or:
            fmt.Println("or")
        case neg:
            fmt.Println("neg")
        case not:
            fmt.Println("not")
        case jmp:
            x := int32(binary.LittleEndian.Uint32(object[pc : pc+4]))
            fmt.Printf("jmp    (%d) %d\n", x, int32(pc)+x)
            pc += 4
        case jz:
            x := int32(binary.LittleEndian.Uint32(object[pc : pc+4]))
            fmt.Printf("jz     (%d) %d\n", x, int32(pc)+x)
            pc += 4
        case prtc:
            fmt.Println("prtc")
        case prti:
            fmt.Println("prti")
        case prts:
            fmt.Println("prts")
        case halt:
            fmt.Println("halt")
        default:
            reportError(fmt.Sprintf("listCode: Unknown opcode %d", op))
        }
    }
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
        return makeLeaf(nodeType, s)
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
    codeGen(loadAst())
    codeFinish()
    listCode()
}
```


while counter example:

```txt

Datasize: 1 Strings: 2
"count is: "
"\n"
    0 push  1
    5 store [0]
   10 fetch [0]
   15 push  10
   20 lt
   21 jz     (43) 65
   26 push  0
   31 prts
   32 fetch [0]
   37 prti
   38 push  1
   43 prts
   44 fetch [0]
   49 push  1
   54 add
   55 store [0]
   60 jmp    (-51) 10
   65 halt

```


## Java

```java
package codegenerator;

import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Scanner;

public class CodeGenerator {
    final static int WORDSIZE = 4;

    static byte[] code = {};

    static Map<String, NodeType> str_to_nodes = new HashMap<>();
    static List<String> string_pool = new ArrayList<>();
    static List<String> variables = new ArrayList<>();
    static int string_count = 0;
    static int var_count = 0;

    static Scanner s;
    static NodeType[] unary_ops = {
        NodeType.nd_Negate, NodeType.nd_Not
    };
    static NodeType[] operators = {
        NodeType.nd_Mul, NodeType.nd_Div, NodeType.nd_Mod, NodeType.nd_Add, NodeType.nd_Sub,
        NodeType.nd_Lss, NodeType.nd_Leq, NodeType.nd_Gtr, NodeType.nd_Geq,
        NodeType.nd_Eql, NodeType.nd_Neq, NodeType.nd_And, NodeType.nd_Or
    };

    static enum Mnemonic {
        NONE, FETCH, STORE, PUSH, ADD, SUB, MUL, DIV, MOD, LT, GT, LE, GE, EQ, NE, AND, OR, NEG, NOT,
        JMP, JZ, PRTC, PRTS, PRTI, HALT
    }
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
        nd_None("", Mnemonic.NONE), nd_Ident("Identifier", Mnemonic.NONE), nd_String("String", Mnemonic.NONE), nd_Integer("Integer", Mnemonic.NONE), nd_Sequence("Sequence", Mnemonic.NONE),
        nd_If("If", Mnemonic.NONE),
        nd_Prtc("Prtc", Mnemonic.NONE), nd_Prts("Prts", Mnemonic.NONE), nd_Prti("Prti", Mnemonic.NONE), nd_While("While", Mnemonic.NONE),
        nd_Assign("Assign", Mnemonic.NONE),
        nd_Negate("Negate", Mnemonic.NEG), nd_Not("Not", Mnemonic.NOT), nd_Mul("Multiply", Mnemonic.MUL), nd_Div("Divide", Mnemonic.DIV), nd_Mod("Mod", Mnemonic.MOD), nd_Add("Add", Mnemonic.ADD),
        nd_Sub("Subtract", Mnemonic.SUB), nd_Lss("Less", Mnemonic.LT), nd_Leq("LessEqual", Mnemonic.LE),
        nd_Gtr("Greater", Mnemonic.GT), nd_Geq("GreaterEqual", Mnemonic.GE), nd_Eql("Equal", Mnemonic.EQ),
        nd_Neq("NotEqual", Mnemonic.NE), nd_And("And", Mnemonic.AND), nd_Or("Or", Mnemonic.OR);

        private final String name;
        private final Mnemonic m;

        NodeType(String name, Mnemonic m) {
            this.name = name;
            this.m = m;
        }
        Mnemonic getMnemonic() { return this.m; }

        @Override
        public String toString() { return this.name; }
    }
    static void appendToCode(int b) {
        code = Arrays.copyOf(code, code.length + 1);
        code[code.length - 1] = (byte) b;
    }
    static void emit_byte(Mnemonic m) {
        appendToCode(m.ordinal());
    }
    static void emit_word(int n) {
        appendToCode(n >> 24);
        appendToCode(n >> 16);
        appendToCode(n >> 8);
        appendToCode(n);
    }
    static void emit_word_at(int pos, int n) {
        code[pos] = (byte) (n >> 24);
        code[pos + 1] = (byte) (n >> 16);
        code[pos + 2] = (byte) (n >> 8);
        code[pos + 3] = (byte) n;
    }
    static int get_word(int pos) {
        int result;
        result = ((code[pos] & 0xff) << 24) + ((code[pos + 1] & 0xff)  << 16) + ((code[pos + 2] & 0xff)  << 8) + (code[pos + 3] & 0xff) ;

        return result;
    }
    static int fetch_var_offset(String name) {
        int n;
        n = variables.indexOf(name);
        if (n == -1) {
            variables.add(name);
            n = var_count++;
        }
        return n;
    }
    static int fetch_string_offset(String str) {
        int n;
        n = string_pool.indexOf(str);
        if (n == -1) {
            string_pool.add(str);
            n = string_count++;
        }
        return n;
    }
    static int hole() {
        int t = code.length;
        emit_word(0);
        return t;
    }
    static boolean arrayContains(NodeType[] a, NodeType n) {
        boolean result = false;
        for (NodeType test: a) {
            if (test.equals(n)) {
                result = true;
                break;
            }
        }
        return result;
    }
    static void code_gen(Node x) throws Exception {
        int n, p1, p2;
        if (x == null) return;

        switch (x.nt) {
            case nd_None: return;
            case nd_Ident:
                emit_byte(Mnemonic.FETCH);
                n = fetch_var_offset(x.value);
                emit_word(n);
                break;
            case nd_Integer:
                emit_byte(Mnemonic.PUSH);
                emit_word(Integer.parseInt(x.value));
                break;
            case nd_String:
                emit_byte(Mnemonic.PUSH);
                n = fetch_string_offset(x.value);
                emit_word(n);
                break;
            case nd_Assign:
                n = fetch_var_offset(x.left.value);
                code_gen(x.right);
                emit_byte(Mnemonic.STORE);
                emit_word(n);
                break;
            case nd_If:
                p2 = 0; // to avoid NetBeans complaining about 'not initialized'
                code_gen(x.left);
                emit_byte(Mnemonic.JZ);
                p1 = hole();
                code_gen(x.right.left);
                if (x.right.right != null) {
                    emit_byte(Mnemonic.JMP);
                    p2 = hole();
                }
                emit_word_at(p1, code.length - p1);
                if (x.right.right != null) {
                    code_gen(x.right.right);
                    emit_word_at(p2, code.length - p2);
                }
                break;
            case nd_While:
                p1 = code.length;
                code_gen(x.left);
                emit_byte(Mnemonic.JZ);
                p2 = hole();
                code_gen(x.right);
                emit_byte(Mnemonic.JMP);
                emit_word(p1 - code.length);
                emit_word_at(p2, code.length - p2);
                break;
            case nd_Sequence:
                code_gen(x.left);
                code_gen(x.right);
                break;
            case nd_Prtc:
                code_gen(x.left);
                emit_byte(Mnemonic.PRTC);
                break;
            case nd_Prti:
                code_gen(x.left);
                emit_byte(Mnemonic.PRTI);
                break;
            case nd_Prts:
                code_gen(x.left);
                emit_byte(Mnemonic.PRTS);
                break;
            default:
                if (arrayContains(operators, x.nt)) {
                    code_gen(x.left);
                    code_gen(x.right);
                    emit_byte(x.nt.getMnemonic());
                } else if (arrayContains(unary_ops, x.nt)) {
                    code_gen(x.left);
                    emit_byte(x.nt.getMnemonic());
                } else {
                    throw new Exception("Error in code generator! Found " + x.nt + ", expecting operator.");
                }
        }
    }
    static void list_code() throws Exception {
        int pc = 0, x;
        Mnemonic op;
        System.out.println("Datasize: " + var_count + " Strings: " + string_count);
        for (String s: string_pool) {
            System.out.println(s);
        }
        while (pc < code.length) {
            System.out.printf("%4d ", pc);
            op = Mnemonic.values()[code[pc++]];
            switch (op) {
                case FETCH:
                    x = get_word(pc);
                    System.out.printf("fetch [%d]", x);
                    pc += WORDSIZE;
                    break;
                case STORE:
                    x = get_word(pc);
                    System.out.printf("store [%d]", x);
                    pc += WORDSIZE;
                    break;
                case PUSH:
                    x = get_word(pc);
                    System.out.printf("push  %d", x);
                    pc += WORDSIZE;
                    break;
                case ADD: case SUB: case MUL: case DIV: case MOD:
                case LT: case GT: case LE: case GE: case EQ: case NE:
                case AND: case OR: case NEG: case NOT:
                case PRTC: case PRTI: case PRTS: case HALT:
                    System.out.print(op.toString().toLowerCase());
                    break;
                case JMP:
                    x = get_word(pc);
                    System.out.printf("jmp     (%d) %d", x, pc + x);
                    pc += WORDSIZE;
                    break;
                case JZ:
                    x = get_word(pc);
                    System.out.printf("jz      (%d) %d", x, pc + x);
                    pc += WORDSIZE;
                    break;
                default:
                    throw new Exception("Unknown opcode " + code[pc] + "@" + (pc - 1));
            }
            System.out.println();
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
                code_gen(n);
                emit_byte(Mnemonic.HALT);
                list_code();
            } catch (Exception e) {
                System.out.println("Ex: "+e);//.getMessage());
            }
        }
    }
}

```



## Julia


```julia
import Base.show

mutable struct Asm32
    offset::Int32
    code::String
    arg::Int32
    targ::Int32
end
Asm32(code, arg = 0) = Asm32(0, code, arg, 0)

show(io::IO, a::Asm32) = print(io, lpad("$(a.offset)", 6), lpad(a.code, 8),
    a.targ > 0 ? (lpad("($(a.arg))", 8) * lpad("$(a.targ)", 4)) :
    (a.code in ["store", "fetch"] ? lpad("[$(a.arg)]", 8) :
    (a.code in ["push"] ? lpad("$(a.arg)", 8) : "")))

const ops32 = Dict{String,String}("Multiply" => "mul", "Divide" => "div", "Mod" => "mod", "Add" => "add",
    "Subtract" => "sub", "Less" => "lt", "Greater" => "gt", "LessEqual" => "le", "GreaterEqual" => "ge",
    "Equal" => "eq", "NotEqual" => "ne", "And" => "and", "or" => "or", "Not" => "not", "Minus" => "neg",
    "Prtc" => "prtc", "Prti" => "prti", "Prts" => "prts")

function compiletoasm(io)
    identifiers = Vector{String}()
    strings = Vector{String}()
    labels = Vector{Int}()

    function cpile(io, islefthandside = false)
        arr = Vector{Asm32}()
        jlabel() = (push!(labels, length(labels) + 1); labels[end])
        m = match(r"^(\w+|;)\s*([\d\w\"\\ \S]+)?", strip(readline(io)))
        x, val = m == nothing ? Pair(";", 0) : m.captures
        if x == ";" return arr
        elseif x == "Assign"
            lhs = cpile(io, true)
            rhs = cpile(io)
            append!(arr, rhs)
            append!(arr, lhs)
            if length(arr) > 100 exit() end
        elseif x == "Integer" push!(arr, Asm32("push", parse(Int32, val)))
        elseif x == "String"
            if !(val in strings)
                push!(strings, val)
            end
            push!(arr, Asm32("push", findfirst(x -> x == val, strings) - 1))
        elseif x == "Identifier"
            if !(val in identifiers)
                if !islefthandside
                    throw("Identifier $val referenced before it is assigned")
                end
                push!(identifiers, val)
            end
            push!(arr, Asm32(islefthandside ? "store" : "fetch", findfirst(x -> x == val, identifiers) - 1))
        elseif haskey(ops32, x)
            append!(arr, cpile(io))
            append!(arr, cpile(io))
            push!(arr, Asm32(ops32[x]))
        elseif x ==  "If"
            append!(arr, cpile(io))
            x, y = jlabel(), jlabel()
            push!(arr, Asm32("jz", x))
            append!(arr, cpile(io))
            push!(arr, Asm32("jmp", y))
            a = cpile(io)
            if length(a) < 1
                push!(a, Asm32("nop", 0))
            end
            a[1].offset = x
            append!(arr, a)
            push!(arr, Asm32(y, "nop", 0, 0)) # placeholder
        elseif x == "While"
            x, y = jlabel(), jlabel()
            a = cpile(io)
            if length(a) < 1
                push!(a, Asm32("nop", 0))
            end
            a[1].offset = x
            append!(arr, a)
            push!(arr, Asm32("jz", y))
            append!(arr, cpile(io))
            push!(arr, Asm32("jmp", x), Asm32(y, "nop", 0, 0))
        elseif x == "Sequence"
            append!(arr, cpile(io))
            append!(arr, cpile(io))
        else
            throw("unknown node type: $x")
        end
        arr
    end

    # compile AST
    asmarr = cpile(io)
    push!(asmarr, Asm32("halt"))
    # move address markers to working code and prune nop code
    for (i, acode) in enumerate(asmarr)
        if acode.code == "nop" && acode.offset != 0 && i < length(asmarr)
            asmarr[i + 1].offset = asmarr[i].offset
        end
    end
    filter!(x -> x.code != "nop", asmarr)
    # renumber offset column with actual offsets
    pos = 0
    jmps = Dict{Int, Int}()
    for acode in asmarr
        if acode.offset > 0
            jmps[acode.offset] = pos
        end
        acode.offset = pos
        pos += acode.code in ["push", "store", "fetch", "jz", "jmp"] ? 5 : 1
    end
    # fix up jump destinations
    for acode in asmarr
        if acode.code in ["jz", "jmp"]
            if haskey(jmps, acode.arg)
                acode.targ = jmps[acode.arg]
                acode.arg = acode.targ - acode.offset -1
            else
                throw("unknown jump location: $acode")
            end
        end
    end
    # print Datasize and Strings header
    println("Datasize: $(length(identifiers)) Strings: $(length(strings))\n" *
        join(strings, "\n") )
    # print assembly lines
    foreach(println, asmarr)
end

const testAST = raw"""
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
Integer       1    """

iob = IOBuffer(testAST) # use an io buffer here for testing, but could use stdin instead of iob

compiletoasm(iob)

```
```txt

 Datasize: 1 Strings: 2
 "count is: "
 "\n"
     0    push       1
     5   store     [0]
    10   fetch     [0]
    15    push      10
    20      lt
    21      jz    (43)  65
    26    push       0
    31    prts
    32   fetch     [0]
    37    prti
    38    push       1
    43    prts
    44   fetch     [0]
    49    push       1
    54     add
    55   store     [0]
    60     jmp   (-51)  10
    65    halt

```



## M2000 Interpreter


```M2000 Interpreter

Module CodeGenerator (s$){
	Function code$(op$) {
		=format$("{0::-6} {1}", pc, op$)
		pc++
	}
	Function code2$(op$, n$) {
		=format$("{0::-6} {1} {2}", pc, op$, n$)
		pc+=5
	}
	Function code3$(op$,pc, st, ed) {
		=format$("{0::-6} {1} ({2}) {3}", pc, op$, ed-st-1, ed)
	}

	Enum tok {
		gneg, gnot, gmul, gdiv, gmod, gadd, gle, gsub, glt
		gle, ggt, gge, geq, gne, gand, gor, gprtc, gprti, gprts,
		gif, gwhile, gAssign, gSeq, gstring, gidentifier, gint, gnone
	}

	\\ Inventories are lists with keys, or keys/data (key must be unique)
	\\ there is one type more the Invetory Queue which get same keys.
	\\ But here not used.
	Inventory symb="Multiply":=gmul, "Divide":=gdiv, "Mod":=gmod, "Add":=gadd
	Append  symb, "Negate":=gneg, "Not":=gnot,"Less":=glt,"Subtract":=gsub
	Append  symb, "LessEqual":=gle, "Greater":=ggt, "GreaterEqual":=gge, "Sequence":=gSeq
	Append  symb, "Equal":=geq, "NotEqual":=gne,  "And":=gand, "Or":=gor, "While":=gwhile
	Append  symb, "Prtc":=gprtc,"Prti":=gprti,"Prts":=gprts, "Assign":=gAssign, "If":=gif
	Append  symb, "String":=gstring, "Identifier":=gidentifier, "Integer":=gint, ";", gnone

	Inventory DataSet
	\\ We set string as key. key maybe an empty string, a string or a number.
	\\ so we want eash string to saved one time only.
	Inventory Strings

	Const nl$=chr$(13)+chr$(10), Ansi=3
	Def z$, lim, line$, newvar_ok, i=0
	Document message$=nl$
	Global pc     \\ functions have own scope, so we make it global, for this module, and childs.

	Dim lines$()
	s$=filter$(s$,chr$(9))   \\ exclude tabs
	Lines$()=piece$(s$,nl$) \\ break to lines
	lim=len(Lines$())
	Flush ' empty stack (there is a current stack of values which we use here)

	Load_Ast()
	If not stack.size=1 Then Flush : Error "Ast not loaded"
	AST=array   \\ pop the array from stack
	Document Assembly$, Header$

	\\ all lines of assembly goes to stack. Maybe not in right order.
	\\ Push statement push to top, Data statement push to bottom of stack

	CodeGenerator(Ast)
	Data  code$("halt") ' append to end of stack
	\\ So now we get all data (letters) from stack
	While not empty
		Assembly$=letter$+nl$
	end while
	\\ So now we have to place them in order
	Sort Assembly$

	\\ Let's make the header
	Header$=format$("Datasize: {0} Strings: {1}", Len(Dataset),Len(strings))
	\\ we use an iterator object, str^ is the counter, readonly, but Eval$() use it from object.
	str=each(strings)
	While str
		Header$=nl$+Eval$(str)
	End while
	Assembly$=nl$
	\\ insert to line 1 the Header
	Insert 1 Assembly$=Header$
	\\ Also we check for warnings
	If len(message$)>2 then Assembly$="Warnings: "+nl$+message$
	\\ So now we get a report
	\\ (at each 3/4 of window's lines, the printing stop and wait for user response, any key)
	Report Assembly$
	Clipboard Assembly$
	Save.Doc Assembly$, "code.t", Ansi
	End
	\\ subs have 10000 limit for recursion but can be extended to 1000000 or more.
	Sub CodeGenerator(t)

		If len(t)=3 then
			select case  t#val(0)
			Case gSeq
				CodeGenerator(t#val(1)) : CodeGenerator(t#val(2))
			Case gwhile
			{
				local spc=pc
				CodeGenerator(t#val(1))
				local pc1=pc
				pc+=5 ' room for jz
				CodeGenerator(t#val(2))
				data code3$("jz",pc1, pc1, pc+5)
				data code3$("jmp",pc,  pc, spc)
				pc+=5  ' room for jmp
			}
			Case gif
			{
				CodeGenerator(t#val(1))
				local pc1=pc, pc2
				pc+=5
				CodeGenerator(t#val(2)#val(1))
				If len(t#val(2)#val(2))>0 then
					pc2=pc
					pc+=5
					data code3$("jz",pc1, pc1, pc)
					CodeGenerator(t#val(2)#val(2))
					data code3$("jmp",pc2, pc2, pc)
				else
					data code3$("jz",pc1, pc1, pc)
				end If
			}
			Case gAssign
			{
				CodeGenerator(t#val(2))
				local newvar_ok=true
				CodeGenerator(t#val(1))
			}
			case gneg to gnot, gprtc to gprts
				CodeGenerator(t#val(1)) : data code$(mid$(eval$(t#val(0)),2))
			case gmul to gor
			{
				CodeGenerator(t#val(1))
				CodeGenerator(t#val(2))
				data code$(mid$(eval$(t#val(0)),2))
			}
			End select
		Else.if len(t)=2 then
			select case  t#val(0)
			Case gString
			{
				local spos
				If exist(strings,t#val$(1)) then
					spos=eval(strings!)
				else
					append strings, t#val$(1)
					spos=len(strings)-1
				end If
				Push code2$("push",str$(spos,0))
			}
			Case gInt
				Push code2$("push",t#val$(1), pc)
			Case gIdentifier
			{
				local ipos
				If exist(dataset,t#val$(1)) then
					ipos=Eval(dataset!)  ' return position
				else.if newvar_ok then
					Append dataset, t#val$(1)
					ipos=len(dataset)-1
				else
					message$="Variable "+t#val$(1)+" not initialized"+nl$

				end If
				If newvar_ok then
					Push code2$("store","["+str$(ipos, 0)+"]")
				else
					Push code2$("fetch","["+str$(ipos, 0)+"]")
				end If
			}
			end select
		End If
	End Sub
	Sub Load_Ast()
			If i>=lim then Push (,) : exit sub
			do
			line$=Trim$(lines$(i))
			I++
			tok$=piece$(line$," ")(0)
			until line$<>"" or i>=lim
			If tok$="Identifier" then
				Push (gidentifier,trim$(Mid$(line$,11)))
			else.if tok$="Integer" then
				long n=Val(Mid$(line$,8))  ' check overflow
				Push (gint, Trim$(Mid$(line$,8)))
			else.if tok$="String" then
				Push (gstring,Trim$(Mid$(line$,7)))
			else.if tok$=";" then
				Push (,)
			Else
				local otok=symb(tok$)
				Load_Ast()
				Load_Ast()
				Shift 2
				Push (otok,array, array)
			End If
	End Sub
}

CodeGenerator {
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
}

```


```txt
Datasize: 1 Strings: 2
"count is: "
"\n"
     0 push
     5 store [0]
    10 fetch [0]
    15 push
    20 lt
    21 jz (43) 65
    26 push 0
    31 prts
    32 fetch [0]
    37 prti
    38 push 1
    43 prts
    44 fetch [0]
    49 push
    54 add
    55 store [0]
    60 jmp (-51) 10
    65 halt
</pre >


## Perl

Tested with perl v5.26.1

```Perl
#!/usr/bin/perl

use strict;   # gen.pl - flatAST to stack machine code
use warnings; # http://www.rosettacode.org/wiki/Compiler/code_generator

my $stringcount = my $namecount = my $pairsym = my $pc = 0;
my (%strings, %names);
my %opnames = qw( Less lt LessEqual le Multiply mul Subtract sub Divide div
  GreaterEqual ge Equal eq Greater gt NotEqual ne Negate neg );

sub tree
  {
  my ($A, $B) = ( '_' . ++$pairsym, '_' . ++$pairsym ); # labels for jumps
  my $line = <> // return '';
  (local $_, my $arg) = $line =~ /^(\w+|;)\s+(.*)/ or die "bad input $line";
  /Identifier/ ? "fetch [@{[ $names{$arg} //= $namecount++ ]}]\n" :
    /Sequence/ ? tree() . tree() :
    /Integer/  ? "push  $arg\n" :
    /String/   ? "push  @{[ $strings{$arg} //= $stringcount++ ]}\n" :
    /Assign/   ? join '', reverse tree() =~ s/fetch/store/r, tree() :
    /While/    ? "$A:\n@{[ tree() ]}jz    $B\n@{[ tree() ]}jmp   $A\n$B:\n" :
    /If/       ? tree() . "jz    $A\n@{[ !<> . # !<> skips second 'If'
                  tree() ]}jmp   $B\n$A:\n@{[ tree() ]}$B:\n" :
    /;/        ? '' :
    tree() . tree() . ($opnames{$_} // lc) . "\n";
  }

$_ = tree() . "halt\n";

s/^jmp\s+(\S+)\n(_\d+:\n)\1:\n/$2/gm;                # remove jmp next
s/^(?=[a-z]\w*(.*))/                                 # add locations
  (sprintf("%4d ", $pc), $pc += $1 ? 5 : 1)[0] /gem;
my %labels = /^(_\d+):(?=(?:\n_\d+:)*\n *(\d+) )/gm; # pc addr of labels
s/^ *(\d+) j(?:z|mp) *\K(_\d+)$/ (@{[                # fix jumps
  $labels{$2} - $1 - 1]}) $labels{$2}/gm;
s/^_\d+.*\n//gm;                                     # remove labels

print "Datasize: $namecount Strings: $stringcount\n";
print "$_\n" for sort { $strings{$a} <=> $strings{$b} } keys %strings;
print;
```

Passes all tests.


## Phix

Reusing parse.e from the [[Compiler/syntax_analyzer#Phix|Syntax Analyzer task]]

Deviates somewhat from the task specification in that it generates executable machine code.

```Phix
--
-- demo\rosetta\Compiler\cgen.e
--
### ======================

--
--  The reusable part of cgen.exw
--

include parse.e

global sequence vars = {},
                strings = {},
                stringptrs = {}

global integer chain = 0
global sequence code = {}

function var_idx(sequence inode)
    if inode[1]!=tk_Identifier then ?9/0 end if
    string ident = inode[2]
    integer n = find(ident,vars)
    if n=0 then
        vars = append(vars,ident)
        n = length(vars)
    end if
    return n
end function

function string_idx(sequence inode)
    if inode[1]!=tk_String then ?9/0 end if
    string s = inode[2]
    integer n = find(s,strings)
    if n=0 then
        strings = append(strings,s)
        stringptrs = append(stringptrs,0)
        n = length(strings)
    end if
    return n
end function

function gen_size(object t)
-- note: must be kept precisely in sync with gen_rec!
--        (relentlessly tested via estsize/actsize)
integer size = 0
    if t!=NULL then
        integer n_type = t[1]
        string node_type = tkNames[n_type]
        switch n_type do
            case tk_Sequence:
                size += gen_size(t[2])
                size += gen_size(t[3])
            case tk_assign:
                size += gen_size(t[3])+6
            case tk_Integer:
                size += 5
            case tk_Identifier:
                size += 6
            case tk_String:
                size += 5
            case tk_while:
                -- emit: @@:<condition><topjmp(@f)><body><tailjmp(@b)>@@:
                size += gen_size(t[2])+3
                integer body = gen_size(t[3])
                integer stail = iff(size+body+2>128?5:2)
                integer stop  = iff(body+stail >127?6:2)
                size += stop+body+stail
            case tk_lt:
            case tk_le:
            case tk_ne:
            case tk_eq:
            case tk_gt:
            case tk_ge:
                size += gen_size(t[2])
                size += gen_size(t[3])
                size += 10
            case tk_and:
            case tk_or:
                size += gen_size(t[2])
                size += gen_size(t[3])
                size += 15
            case tk_add:
            case tk_sub:
                size += gen_size(t[2])
                size += gen_size(t[3])
                size += 4
            case tk_mul:
                size += gen_size(t[2])
                size += gen_size(t[3])
                size += 5
            case tk_div:
            case tk_mod:
                size += gen_size(t[2])
                size += gen_size(t[3])
                size += 6
            case tk_putc:
            case tk_Printi:
            case tk_Prints:
                size += gen_size(t[2])
                size += 5
            case tk_if:
                size += gen_size(t[2])+3
                if t[3][1]!=tk_if then ?9/0 end if
                integer truesize = gen_size(t[3][2])
                integer falsesize = gen_size(t[3][3])
                integer elsejmp = iff(falsesize=0?0:iff(falsesize>127?5:2))
                integer mainjmp = iff(truesize+elsejmp>127?6:2)
                size += mainjmp+truesize+elsejmp+falsesize
            case tk_not:
                size += gen_size(t[2])
                size += 9
            case tk_neg:
                size += gen_size(t[2])
                size += 4
            else:
                ?9/0
        end switch
    end if
    return size
end function

procedure gen_rec(object t)
-- the recursive part of code_gen
    if t!=NULL then
        integer initsize = length(code)
        integer estsize = gen_size(t)   -- (test the gen_size function)
        integer n_type = t[1]
        string node_type = tkNames[n_type]
        switch n_type do
            case tk_Sequence:
                gen_rec(t[2])
                gen_rec(t[3])
            case tk_assign:
                integer n = var_idx(t[2])
                gen_rec(t[3])
                code &= {0o217,0o005,chain,1,n,0}   -- pop [i]
                chain = length(code)-3
            case tk_Integer:
                integer n = t[2]
                code &= 0o150&int_to_bytes(n)       -- push imm32
            case tk_while:
                -- emit: @@:<condition><topjmp(@f)><body><tailjmp(@b)>@@:
                integer looptop = length(code)
                gen_rec(t[2])
                code &= {0o130,                                 -- pop eax
                         0o205,0o300}                           -- test eax,eax
                integer bodysize = gen_size(t[3])
                -- can we use short jumps?
                -- disclaimer: size calcs are not heavily tested; if in
                --             doubt reduce 128/7 by 8, and if that works
                --             then yep, you just found a boundary case.
                integer stail = iff(length(code)+bodysize+4-looptop>128?5:2)
                integer offset = bodysize+stail
                integer stop  = iff(offset>127?6:2)
                if stop=2 then
                    code &= {0o164,offset}                      -- jz (short) end
                else
                    code &= {0o017,0o204}&int_to_bytes(offset)  -- jz (long) end
                end if
                gen_rec(t[3])
                offset = looptop-(length(code)+stail)
                if stail=2 then
                    code &= 0o353&offset                        -- jmp looptop (short)
                else
                    code &= 0o351&int_to_bytes(offset)          -- jmp looptop (long)
                end if
            case tk_lt:
            case tk_le:
            case tk_gt:
            case tk_ge:
            case tk_ne:
            case tk_eq:
                gen_rec(t[2])
                gen_rec(t[3])
                integer xrm
                if    n_type=tk_ne then xrm = 0o225 -- (#95)
                elsif n_type=tk_lt then xrm = 0o234 -- (#9C)
                elsif n_type=tk_ge then xrm = 0o235 -- (#9D)
                elsif n_type=tk_le then xrm = 0o236 -- (#9E)
                elsif n_type=tk_gt then xrm = 0o237 -- (#9F)
                else ?9/0
                end if
                code &= { 0o061,0o300,                          -- xor eax,eax
                          0o132,                                -- pop edx
                          0o131,                                -- pop ecx
                          0o071,0o321,                          -- cmp ecx,edx
                          0o017,xrm,0o300,                      -- setcc al
                          0o120}                                -- push eax
            case tk_or:
            case tk_and:
                gen_rec(t[2])
                gen_rec(t[3])
                integer op = find(n_type,{tk_or,0,0,tk_and})
                op *= 0o010
                code &= { 0o130,                                -- pop eax
                          0o131,                                -- pop ecx
                          0o205,0o300,                          -- test eax,eax
                          0o017,0o225,0o300,                    -- setne al
                          0o205,0o311,                          -- test ecx,ecx
                          0o017,0o225,0o301,                    -- setne cl
                          op,0o310,                             -- or/and al,cl
                          0o120}                                -- push eax
            case tk_add:
            case tk_sub:
                gen_rec(t[2])
                gen_rec(t[3])
                integer op = find(n_type,{tk_add,0,0,0,0,tk_sub})
                op = 0o001 + (op-1)*0o010
                code &= { 0o130,                                -- pop eax
                          op,0o004,0o044}                       -- add/or/and/sub [esp],eax
            case tk_mul:
                gen_rec(t[2])
                gen_rec(t[3])
                code &= { 0o131,                                -- pop ecx
                          0o130,                                -- pop eax
                          0o367,0o341,                          -- mul ecx
                          0o120}                                -- push eax
            case tk_div:
            case tk_mod:
                gen_rec(t[2])
                gen_rec(t[3])
                integer push = 0o120+(n_type=tk_mod)*2
                code &= { 0o131,                                -- pop ecx
                          0o130,                                -- pop eax
                          0o231,                                -- cdq (eax -> edx:eax)
                          0o367,0o371,                          -- idiv ecx
                          push}                                 -- push eax|edx
            case tk_Identifier:
                integer n = var_idx(t)
                code &= {0o377,0o065,chain,1,n,0}               -- push [n]
                chain = length(code)-3
            case tk_putc:
            case tk_Printi:
            case tk_Prints:
                gen_rec(t[2])
                integer n = find(n_type,{tk_putc,tk_Printi,tk_Prints})
                code &= {0o350,chain,3,n,0}                     -- call :printc/i/s
                chain = length(code)-3
            case tk_String:
                integer n = string_idx(t)
                code &= {0o150,chain,2,n,0}                     -- push RawStringPtr(string)
                chain = length(code)-3
            case tk_if:
                -- emit: <condition><mainjmp><truepart>[<elsejmp><falsepart>]
                gen_rec(t[2])
                code &= {0o130,                                 -- pop eax
                         0o205,0o300}                           -- test eax,eax
                if t[3][1]!=tk_if then ?9/0 end if
                integer truesize = gen_size(t[3][2])
                integer falsesize = gen_size(t[3][3])
                integer elsejmp = iff(falsesize=0?0:iff(falsesize>127?5:2))
                integer offset = truesize+elsejmp
                integer mainjmp = iff(offset>127?6:2)
                if mainjmp=2 then
                    code &= {0o164,offset}                      -- jz (short) else/end
                else
                    code &= {0o017,0o204}&int_to_bytes(offset)  -- jz (long) else/end
                end if
                gen_rec(t[3][2])
                if falsesize!=0 then
                    offset = falsesize
                    if elsejmp=2 then
                        code &= 0o353&offset                    -- jmp end if (short)
                    else
                        code &= 0o351&int_to_bytes(offset)      -- jmp end if (long)
                    end if
                    gen_rec(t[3][3])
                end if
            case tk_not:
                gen_rec(t[2])
                code &= {0o132,                                 -- pop edx
                         0o061,0o300,                           -- xor eax,eax
                         0o205,0o322,                           -- test edx,edx
                         0o017,0o224,0o300,                     -- setz al
                         0o120}                                 -- push eax
            case tk_neg:
                gen_rec(t[2])
                code &= {0o130,                             -- pop eax
                         0o367,0o330,                       -- neg eax
                         0o120}                             -- push eax
            else:
                error("error in code generator - found %d, expecting operator\n", {n_type})
        end switch
        integer actsize = length(code)
        if initsize+estsize!=actsize then ?"9/0" end if -- (test gen_size)
    end if
end procedure

global procedure code_gen(object t)
--
-- Generates proper machine code.
--
-- Example: i=10; print "\n"; print i; print "\n"
-- Result in vars, strings, chain, code (declared above)
--    where vars is: {"i"},
--          strings is {"\n"},
--          code is { 0o150,#0A,#00,#00,#00,        -- 1: push 10
--                    0o217,0o005,0,1,1,0           -- 6: pop [i]
--                    0o150,8,2,1,0,                -- 12: push ("\n")
--                    0o350,13,3,3,0,               -- 17: call :prints
--                    0o377,0o065,18,1,1,0,         -- 22: push [i]
--                    0o350,24,3,2,0,               -- 28: call :printi
--                    0o150,29,2,1,0,               -- 33: push ("\n")
--                    0o350,34,3,3,0,               -- 38: call :prints
--                    0o303}                        -- 43: ret
--          and chain is 39 (->34->29->24->18->13->8->0)
-- The chain connects all places where we need an actual address before
--  the code is executed, with the byte after the link differentiating
--  between var(1), string(2), and builtin(3), and the byte after that
--  determining the instance of the given type - not that any of them
--  are actually limited to a byte in the above intermediate form, and
--  of course the trailing 0 of each {link,type,id,0} is just there to
--  reserve the space we will need.
--
    gen_rec(t)
    code = append(code,0o303)   -- ret (0o303=#C3)
end procedure

include builtins/VM/puts1.e -- low-level console i/o routines

function setbuiltins()
atom printc,printi,prints
    #ilASM{
        jmp :setbuiltins
    ::printc
        lea edi,[esp+4]
        mov esi,1
        call :%puts1ediesi  -- (edi=raw text, esi=length)
        ret 4
    ::printi
        mov eax,[esp+4]
        push 0              -- no cr
        call :%putsint      -- (nb limited to +/-9,999,999,999)
        ret 4
    ::prints
        mov edi,[esp+4]
        mov esi,[edi-12]
        call :%puts1ediesi  -- (edi=raw text, esi=length)
        ret 4
    ::setbuiltins
        mov eax,:printc
        lea edi,[printc]
        call :%pStoreMint
        mov eax,:printi
        lea edi,[printi]
        call :%pStoreMint
        mov eax,:prints
        lea edi,[prints]
        call :%pStoreMint
          }
    return {printc,printi,prints}
end function

global constant builtin_names = {"printc","printi","prints"}
global constant builtins = setbuiltins()

global atom var_mem, code_mem

function RawStringPtr(integer n)    -- (based on IupRawStringPtr from pGUI.e)
--
-- Returns a raw string pointer for s, somewhat like allocate_string(s), but using the existing memory.
-- NOTE: The return is only valid as long as the value passed as the parameter remains in existence.
--
atom res
    string s = strings[n]
    #ilASM{
            mov eax,[s]
            lea edi,[res]
            shl eax,2
            call :%pStoreMint
          }
    stringptrs[n] = res
    return res
end function

global procedure fixup()
    var_mem = allocate(length(vars)*4)
    mem_set(var_mem,0,length(vars)*4)
    code_mem = allocate(length(code))
    poke(code_mem,code)
    while chain!=0 do
        integer this = chain
        chain = code[this]
        integer ftype = code[this+1]
        integer id = code[this+2]
        switch ftype do
            case 1: -- vars
                poke4(code_mem+this-1,var_mem+(id-1)*4)
            case 2: -- strings
                poke4(code_mem+this-1,RawStringPtr(id))
            case 3: -- builtins
                poke4(code_mem+this-1,builtins[id]-(code_mem+this+3))
        end switch
    end while
end procedure
```

And a simple test driver for the specific task:

```Phix
--
-- demo\rosetta\Compiler\cgen.exw
--
### ========================

--
--  Generates 32-bit machine code (see note in vm.exw)
--

include cgen.e

function get_var_name(atom addr)
    integer n = (addr-var_mem)/4+1
    if n<1 or n>length(vars) then ?9/0 end if
    return vars[n]
end function

function hxl(integer pc, object oh, string fmt, sequence args={})
-- helper routine to display the octal/hex bytes just decoded,
-- along with the code offset and the human-readable text.
    if length(args) then fmt = sprintf(fmt,args) end if
    sequence octhex = {}
    atom base = code_mem+pc
    integer len = 0
    if integer(oh) then -- all octal
        for i=1 to oh do
            octhex = append(octhex,sprintf("0o%03o",peek(base)))
            base += 1
        end for
        len = oh
    else    -- some octal and some hex
        for i=1 to length(oh) by 2 do
            for j=1 to oh[i] do
                octhex = append(octhex,sprintf("0o%03o",peek(base)))
                base += 1
            end for
            len += oh[i]
            for j=1 to oh[i+1] do
                octhex = append(octhex,sprintf("#%02x",peek(base)))
                base += 1
            end for
            len += oh[i+1]
        end for
    end if
    printf(output_file,"%4d: %-30s %s\n",{pc+1,join(octhex,","),fmt})
    return len
end function

constant cccodes = {"o?" ,"no?","b?" ,"ae?","z" ,"ne" ,"be?","a?",
--                    0  ,  1  ,  2  ,  3  ,  4 ,  5  ,  6  , 7  ,
                    "s?" ,"ns?","pe?","po?","l" ,"ge" ,"le" ,"g" }
--                    8  ,  9  , 10  , 11  , 12 , 13  , 14  , 15

constant regs = {"eax","ecx","edx"} -- (others as/when needed)

procedure decode()
-- for a much more complete (and better organised) disassembler, see p2asm.e
integer pc = 0, -- nb 0-based
        opcode, xrm

    while pc<length(code) do
        opcode = peek(code_mem+pc)
        xrm = -1
        switch opcode do
            case 0o150:
                atom vaddr = peek4s(code_mem+pc+1)
                integer n = find(vaddr,stringptrs)
                object arg = iff(n?enquote(strings[n])
                                  :sprintf("%d",vaddr))
                pc += hxl(pc,{1,4},"push %s",{arg})
            case 0o217:
            case 0o377:
                integer n = find(opcode,{0o217,0o377})
                string op = {"pop","push"}[n]
                xrm = peek(code_mem+pc+1)
                if n!=find(xrm,{0o005,0o065}) then exit end if
                atom addr = peek4u(code_mem+pc+2)
                pc += hxl(pc,{2,4},"%s [%s]",{op,get_var_name(addr)})
            case 0o061:
            case 0o071:
            case 0o205:
                integer n = find(opcode,{0o061,0o071,0o205})
                string op = {"xor","cmp","test"}[n]
                xrm = peek(code_mem+pc+1)
                if and_bits(xrm,0o300)!=0o300 then exit end if
                string r1 = regs[and_bits(xrm,0o070)/0o010+1]
                string r2 = regs[and_bits(xrm,0o007)+1]
                pc += hxl(pc,2,"%s %s,%s",{op,r1,r2})
            case 0o017:
                xrm = peek(code_mem+pc+1)
                switch xrm do
                    case 0o224:
                    case 0o225:
                    case 0o234:
                    case 0o235:
                    case 0o236:
                    case 0o237:
                        string cc = cccodes[and_bits(xrm,0o017)+1]
                        xrm = peek(code_mem+pc+2)
                        if xrm=0o300 then
                            pc += hxl(pc,3,"set%s al",{cc})
                        elsif xrm=0o301 then
                            pc += hxl(pc,3,"set%s cl",{cc})
                        else
                            exit
                        end if
                    case 0o204:
                        integer offset = peek4s(code_mem+pc+2)
                        pc += hxl(pc,{2,4},"jz %d",{pc+6+offset+1})
                    else
                        exit
                end switch
            case 0o010:
            case 0o040:
                xrm = peek(code_mem+pc+1)
                if xrm=0o310 then
                    string lop = {"or","and"}[find(opcode,{0o010,0o040})]
                    pc += hxl(pc,2,"%s al,cl",{lop})
                else
                    exit
                end if
            case 0o120:
            case 0o122:
            case 0o130:
            case 0o131:
            case 0o132:
                string op = {"push","pop"}[find(and_bits(opcode,0o070),{0o020,0o030})]
                string reg = regs[and_bits(opcode,0o007)+1]
                pc += hxl(pc,1,"%s %s",{op,reg})
            case 0o231:
                pc += hxl(pc,1,"cdq")
            case 0o164:
            case 0o353:
                string jop = iff(opcode=0o164?"jz":"jmp")
                integer offset = peek1s(code_mem+pc+1)
                pc += hxl(pc,{1,1},"%s %d",{jop,pc+2+offset+1})
            case 0o351:
                integer offset = peek4s(code_mem+pc+1)
                pc += hxl(pc,{1,4},"jmp %d",{pc+5+offset+1})
            case 0o303:
                pc += hxl(pc,1,"ret")
            case 0o350:
                integer offset = peek4s(code_mem+pc+1)
                atom addr = offset+code_mem+pc+5
                integer n = find(addr,builtins)
                pc += hxl(pc,{1,4},"call :%s",{builtin_names[n]})
            case 0o001:
            case 0o041:
            case 0o051:
                integer n = find(opcode,{0o001,0o041,0o051})
                string op = {"add","and","sub"}[n]
                xrm = peek(code_mem+pc+1)
                switch xrm do
                    case 0o004:
                        if peek(code_mem+pc+2)=0o044 then
                            pc += hxl(pc,3,"%s [esp],eax",{op})
                        else
                            exit
                        end if
                    else
                        exit
                end switch
            case 0o367:
                xrm = peek(code_mem+pc+1)
                if and_bits(xrm,0o300)!=0o300 then exit end if
                integer n = find(and_bits(xrm,0o070),{0o030,0o040,0o070})
                if n=0 then exit end if
                string op = {"neg","mul","idiv"}[n]
                string reg = regs[and_bits(xrm,0o007)+1]
                pc += hxl(pc,2,"%s %s",{op,reg})
            else
                exit
        end switch
    end while
    if pc<length(code) then
        ?"incomplete:"
        if xrm=-1 then
            ?{pc+1,sprintf("0o%03o",opcode)}
        else
            ?{pc+1,sprintf("0o%03o 0o%03o",{opcode,xrm})}
        end if
    end if
end procedure

procedure main(sequence cl)
    open_files(cl)
    toks = lex()
    object t = parse()
    code_gen(t)
    fixup()
    decode()
    free({var_mem,code_mem})
    close_files()
end procedure

--main(command_line())
main({0,0,"gcd.c"})
```

```txt

   1: 0o150,#2F,#04,#00,#00          push 1071
   6: 0o217,0o005,#70,#BE,#73,#00    pop [a]
  12: 0o150,#05,#04,#00,#00          push 1029
  17: 0o217,0o005,#74,#BE,#73,#00    pop [b]
  23: 0o377,0o065,#74,#BE,#73,#00    push [b]
  29: 0o150,#00,#00,#00,#00          push 0
  34: 0o061,0o300                    xor eax,eax
  36: 0o132                          pop edx
  37: 0o131                          pop ecx
  38: 0o071,0o321                    cmp edx,ecx
  40: 0o017,0o225,0o300              setne al
  43: 0o120                          push eax
  44: 0o130                          pop eax
  45: 0o205,0o300                    test eax,eax
  47: 0o164,#32                      jz 99
  49: 0o377,0o065,#74,#BE,#73,#00    push [b]
  55: 0o217,0o005,#78,#BE,#73,#00    pop [new_a]
  61: 0o377,0o065,#70,#BE,#73,#00    push [a]
  67: 0o377,0o065,#74,#BE,#73,#00    push [b]
  73: 0o131                          pop ecx
  74: 0o130                          pop eax
  75: 0o231                          cdq
  76: 0o367,0o371                    idiv ecx
  78: 0o122                          push edx
  79: 0o217,0o005,#74,#BE,#73,#00    pop [b]
  85: 0o377,0o065,#78,#BE,#73,#00    push [new_a]
  91: 0o217,0o005,#70,#BE,#73,#00    pop [a]
  97: 0o353,#B4                      jmp 23
  99: 0o377,0o065,#70,#BE,#73,#00    push [a]
 105: 0o350,#2F,#49,#0B,#00          call :printi
 110: 0o303                          ret

```



## Python

Tested with Python 2.7 and 3.x

```Python
from __future__ import print_function
import sys, struct, shlex, operator

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

FETCH, STORE, PUSH, ADD, SUB, MUL, DIV, MOD, LT, GT, LE, GE, EQ, NE, AND, OR, NEG, NOT, \
JMP, JZ, PRTC, PRTS, PRTI, HALT = range(24)

operators = {nd_Lss: LT, nd_Gtr: GT, nd_Leq: LE, nd_Geq: GE, nd_Eql: EQ, nd_Neq: NE,
    nd_And: AND, nd_Or: OR, nd_Sub: SUB, nd_Add: ADD, nd_Div: DIV, nd_Mul: MUL, nd_Mod: MOD}

unary_operators = {nd_Negate: NEG, nd_Not: NOT}

input_file  = None
code        = bytearray()
string_pool = {}
globals     = {}
string_n    = 0
globals_n   = 0
word_size   = 4

#*** show error and exit
def error(msg):
    print("%s" % (msg))
    exit(1)

def int_to_bytes(val):
    return struct.pack("<i", val)

def bytes_to_int(bstr):
    return struct.unpack("<i", bstr)

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
def emit_byte(x):
    code.append(x)

#***
def emit_word(x):
    s = int_to_bytes(x)
    for x in s:
        code.append(x)

def emit_word_at(at, n):
    code[at:at+word_size] = int_to_bytes(n)

def hole():
    t = len(code)
    emit_word(0)
    return t

#***
def fetch_var_offset(name):
    global globals_n

    n = globals.get(name, None)
    if n == None:
        globals[name] = globals_n
        n = globals_n
        globals_n += 1
    return n

#***
def fetch_string_offset(the_string):
    global string_n

    n = string_pool.get(the_string, None)
    if n == None:
        string_pool[the_string] = string_n
        n = string_n
        string_n += 1
    return n

#***
def code_gen(x):
    if x == None: return
    elif x.node_type == nd_Ident:
        emit_byte(FETCH)
        n = fetch_var_offset(x.value)
        emit_word(n)
    elif x.node_type == nd_Integer:
        emit_byte(PUSH)
        emit_word(x.value)
    elif x.node_type == nd_String:
        emit_byte(PUSH)
        n = fetch_string_offset(x.value)
        emit_word(n)
    elif x.node_type == nd_Assign:
        n = fetch_var_offset(x.left.value)
        code_gen(x.right)
        emit_byte(STORE)
        emit_word(n)
    elif x.node_type == nd_If:
        code_gen(x.left)              # expr
        emit_byte(JZ)                 # if false, jump
        p1 = hole()                   # make room for jump dest
        code_gen(x.right.left)        # if true statements
        if (x.right.right != None):
            emit_byte(JMP)            # jump over else statements
            p2 = hole()
        emit_word_at(p1, len(code) - p1)
        if (x.right.right != None):
            code_gen(x.right.right)   # else statements
            emit_word_at(p2, len(code) - p2)
    elif x.node_type == nd_While:
        p1 = len(code)
        code_gen(x.left)
        emit_byte(JZ)
        p2 = hole()
        code_gen(x.right)
        emit_byte(JMP)                       # jump back to the top
        emit_word(p1 - len(code))
        emit_word_at(p2, len(code) - p2)
    elif x.node_type == nd_Sequence:
        code_gen(x.left)
        code_gen(x.right)
    elif x.node_type == nd_Prtc:
        code_gen(x.left)
        emit_byte(PRTC)
    elif x.node_type == nd_Prti:
        code_gen(x.left)
        emit_byte(PRTI)
    elif x.node_type == nd_Prts:
        code_gen(x.left)
        emit_byte(PRTS)
    elif x.node_type in operators:
        code_gen(x.left)
        code_gen(x.right)
        emit_byte(operators[x.node_type])
    elif x.node_type in unary_operators:
        code_gen(x.left)
        emit_byte(unary_operators[x.node_type])
    else:
        error("error in code generator - found %d, expecting operator" % (x.node_type))

#***
def code_finish():
    emit_byte(HALT)

#***
def list_code():
    print("Datasize: %d Strings: %d" % (len(globals), len(string_pool)))

    for k in sorted(string_pool, key=string_pool.get):
        print(k)

    pc = 0
    while pc < len(code):
        print("%4d " % (pc), end='')
        op = code[pc]
        pc += 1
        if op == FETCH:
            x = bytes_to_int(code[pc:pc+word_size])[0]
            print("fetch [%d]" % (x));
            pc += word_size
        elif op == STORE:
            x = bytes_to_int(code[pc:pc+word_size])[0]
            print("store [%d]" % (x));
            pc += word_size
        elif op == PUSH:
            x = bytes_to_int(code[pc:pc+word_size])[0]
            print("push  %d" % (x));
            pc += word_size
        elif op == ADD:   print("add")
        elif op == SUB:   print("sub")
        elif op == MUL:   print("mul")
        elif op == DIV:   print("div")
        elif op == MOD:   print("mod")
        elif op == LT:    print("lt")
        elif op == GT:    print("gt")
        elif op == LE:    print("le")
        elif op == GE:    print("ge")
        elif op == EQ:    print("eq")
        elif op == NE:    print("ne")
        elif op == AND:   print("and")
        elif op == OR:    print("or")
        elif op == NEG:   print("neg")
        elif op == NOT:   print("not")
        elif op == JMP:
            x = bytes_to_int(code[pc:pc+word_size])[0]
            print("jmp    (%d) %d" % (x, pc + x));
            pc += word_size
        elif op == JZ:
            x = bytes_to_int(code[pc:pc+word_size])[0]
            print("jz     (%d) %d" % (x, pc + x));
            pc += word_size
        elif op == PRTC:  print("prtc")
        elif op == PRTI:  print("prti")
        elif op == PRTS:  print("prts")
        elif op == HALT:  print("halt")
        else: error("list_code: Unknown opcode %d", (op));

def load_ast():
    line = input_file.readline()
    line_list = shlex.split(line, False, False)

    text = line_list[0]
    if text == ";":
        return None
    node_type = all_syms[text]

    if len(line_list) > 1:
        value = line_list[1]
        if value.isdigit():
            value = int(value)
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
        error("Can't open %s" % sys.argv[1])

n = load_ast()
code_gen(n)
code_finish()
list_code()
```


<b>

```txt
Datasize: 1 Strings: 2
"count is: "
"\n"
   0 push  1
   5 store [0]
  10 fetch [0]
  15 push  10
  20 lt
  21 jz     (43) 65
  26 push  0
  31 prts
  32 fetch [0]
  37 prti
  38 push  1
  43 prts
  44 fetch [0]
  49 push  1
  54 add
  55 store [0]
  60 jmp    (-51) 10
  65 halt
```

</b>


## Scheme



```scheme

(import (scheme base)
        (scheme file)
        (scheme process-context)
        (scheme write)
        (only (srfi 1) delete-duplicates list-index)
        (only (srfi 13) string-delete string-index string-trim))

(define *names* '((Add add) (Subtract sub) (Multiply mul) (Divide div) (Mod mod)
                            (Less lt) (Greater gt) (LessEqual le) (GreaterEqual ge)
                            (Equal eq) (NotEqual ne) (And and) (Or or) (Negate neg)
                            (Not not) (Prts prts) (Prti prti) (Prtc prtc)))

(define (change-name name)
  (if (assq name *names*)
    (cdr (assq name *names*))
    (error "Cannot find name" name)))

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
  (with-input-from-file filename (lambda () (read-expr))))

;; run a three-pass assembler
(define (generate-code ast)
  (define new-address ; create a new unique address - for jump locations
    (let ((count 0))
      (lambda ()
        (set! count (+ 1 count))
        (string->symbol (string-append "loc-" (number->string count))))))
  ; define some names for fields
  (define left cadr)
  (define right (lambda (x) (cadr (cdr x))))
  ;
  (define (extract-values ast)
    (if (null? ast)
      (values '() '())
      (case (car ast)
        ((Integer)
         (values '() '()))
        ((Negate Not Prtc Prti Prts)
         (extract-values (left ast)))
        ((Assign Add Subtract Multiply Divide Mod Less Greater LessEqual GreaterEqual
                 Equal NotEqual And Or If While Sequence)
         (let-values (((a b) (extract-values (left ast)))
                      ((c d) (extract-values (right ast))))
                     (values (delete-duplicates (append a c) string=?)
                             (delete-duplicates (append b d) string=?))))
        ((String)
         (values '() (list (left ast))))
        ((Identifier)
         (values (list (left ast)) '())))))
  ;
  (let-values (((constants strings) (extract-values ast)))
              (define (constant-idx term)
                (list-index (lambda (s) (string=? s term)) constants))
              (define (string-idx term)
                (list-index (lambda (s) (string=? s term)) strings))
              ;
              (define (pass-1 ast asm) ; translates ast into a list of basic operations
                (if (null? ast)
                  asm
                  (case (car ast)
                    ((Integer)
                     (cons (list 'push (left ast)) asm))
                    ((Identifier)
                     (cons (list 'fetch (constant-idx (left ast))) asm))
                    ((String)
                     (cons (list 'push (string-idx (left ast))) asm))
                    ((Assign)
                     (cons (list 'store (constant-idx (left (left ast)))) (pass-1 (right ast) asm)))
                    ((Add Subtract Multiply Divide Mod Less Greater LessEqual GreaterEqual
                          Equal NotEqual And Or) ; binary operators
                     (cons (change-name (car ast))
                           (pass-1 (right ast) (pass-1 (left ast) asm))))
                    ((Negate Not Prtc Prti Prts) ; unary operations
                     (cons (change-name (car ast))
                           (pass-1 (left ast) asm)))
                    ((If)
                     (let ((label-else (new-address))
                           (label-end (new-address)))
                       (if (null? (right (right ast)))
                         (cons (list 'label label-end) ; label for end of if statement
                               (pass-1 (left (right ast)) ; output the 'then block
                                       (cons (list 'jz label-end) ; jump to end when test is false
                                             (pass-1 (left ast) asm))))
                         (cons (list 'label label-end) ; label for end of if statement
                               (pass-1 (right (right ast)) ; output the 'else block
                                       (cons (list 'label label-else)
                                             (cons (list 'jmp label-end) ; jump past 'else, after 'then
                                                   (pass-1 (left (right ast)) ; output the 'then block
                                                           (cons (list 'jz label-else) ; jumpt to else when false
                                                                 (pass-1 (left ast) asm))))))))))
                    ((While)
                     (let ((label-test (new-address))
                           (label-end (new-address)))
                       (cons (list 'label label-end) ; introduce a label for end of while block
                             (cons (list 'jmp label-test) ; jump back to repeat test
                                   (pass-1 (right ast)  ; output the block
                                           (cons (list 'jz label-end) ; test failed, jump around block
                                                 (pass-1 (left ast) ; output the test
                                                         (cons (list 'label label-test) ; introduce a label for test
                                                               asm))))))))
                    ((Sequence)
                     (pass-1 (right ast) (pass-1 (left ast) asm)))
                    (else
                      "Unknown token type"))))
              ;
              (define (pass-2 asm) ; adds addresses and fills in jump locations
                (define (fill-addresses)
                  (let ((addr 0))
                    (map (lambda (instr)
                           (let ((res (cons addr instr)))
                             (unless (eq? (car instr) 'label)
                               (set! addr (+ addr (if (= 1 (length instr)) 1 5))))
                             res))
                         asm)))
                ;
                (define (extract-labels asm)
                  (let ((labels '()))
                    (for-each (lambda (instr)
                                (when (eq? (cadr instr) 'label)
                                  (set! labels (cons (cons (cadr (cdr instr)) (car instr))
                                                     labels))))
                              asm)
                    labels))
                ;
                (define (add-jump-locations asm labels rec)
                  (cond ((null? asm)
                         (reverse rec))
                        ((eq? (cadr (car asm)) 'label) ; ignore the labels
                         (add-jump-locations (cdr asm) labels rec))
                        ((memq (cadr (car asm)) '(jmp jz)) ; replace labels with addresses for jumps
                         (add-jump-locations (cdr asm)
                                             labels
                                             (cons (list (car (car asm)) ; previous address
                                                         (cadr (car asm)) ; previous jump type
                                                         (cdr (assq (cadr (cdar asm)) labels))) ; actual address
                                                   rec)))
                        (else
                          (add-jump-locations (cdr asm) labels (cons (car asm) rec)))))
                ;
                (let ((asm+addr (fill-addresses)))
                  (add-jump-locations asm+addr (extract-labels asm+addr) '())))
              ;
              (define (output-instruction instr)
                   (display (number->string (car instr))) (display #\tab)
                   (display (cadr instr)) (display #\tab)
                (case (cadr instr)
                  ((fetch store)
                   (display "[") (display (number->string (cadr (cdr instr)))) (display "]\n"))
                  ((jmp jz)
                   (display
                     (string-append "("
                                    (number->string (- (cadr (cdr instr)) (car instr) 1))
                                    ")"))
                   (display #\tab)
                   (display (number->string (cadr (cdr instr)))) (newline))
                  ((push)
                   (display (cadr (cdr instr))) (newline))
                  (else
                    (newline))))
              ; generate the code and output to stdout
              (display
                (string-append "Datasize: "
                               (number->string (length constants))
                               " Strings: "
                               (number->string (length strings))))
              (newline)
              (for-each (lambda (str) (display str) (newline))
                        strings)
              (for-each output-instruction
                        (pass-2 (reverse (cons (list 'halt) (pass-1 ast '())))))))

;; read AST from file and output code to stdout
(if (= 2 (length (command-line)))
  (generate-code (read-code (cadr (command-line))))
  (display "Error: pass an ast filename\n"))

```


Tested on all examples in [[Compiler/Sample programs]].


## zkl

```zkl
// This is a little endian machine

const WORD_SIZE=4;
const{ var _n=-1; var[proxy]N=fcn{ _n+=1 }; }  // enumerator
const FETCH=N, STORE=N, PUSH=N, ADD=N,  SUB=N,  MUL=N, DIV=N, MOD=N,
      LT=N,    GT=N,    LE=N,   GE=N,   EQ=N,   NE=N,
      AND=N,   OR=N,    NEG=N,  NOT=N,
      JMP=N,   JZ=N,    PRTC=N, PRTS=N, PRTI=N, HALT=N;
const nd_String=N, nd_Sequence=N, nd_If=N, nd_While=N;
var all_syms=Dictionary(
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
    "Or"          ,OR,		"halt"	      ,HALT);
var binOps=T(LT,GT,LE,GE,EQ,NE, AND,OR, SUB,ADD,DIV,MUL,MOD),
    unaryOps=T(NEG,NOT);

class Node{
   fcn init(_node_type, _value, _left=Void, _right=Void){
      var type=_node_type, left=_left, right=_right, value=_value;
   }
}

var vars=Dictionary(), strings=Dictionary(); // ( value:offset, ...)
fcn doVar(value){
   var offset=-1;  // fcn local static var
   offset=_doValue(value,vars,offset)
}
fcn doString(str){ str=str[1,-1];	// str is \"text\"
   var offset=-1;  // fcn local static var
   str=str.replace("\\n","\n");
   offset=_doValue(str,strings,offset)
}
fcn _doValue(value,vars,offset){  //--> offset of value in vars
   if(Void!=(n:=vars.find(value))) return(n);	// fetch existing value
   vars[value]=offset+=1;			// store new value
}

fcn asm(node,code){
   if(Void==node) return(code);
   emitB:='wrap(n){ code.append(n) };
   emitW:='wrap(n){ code.append(n.toLittleEndian(WORD_SIZE)) }; // signed
   switch(node.type){
      case(FETCH)    { emitB(FETCH); emitW(doVar(node.value));    }
      case(PUSH)     { emitB(PUSH);  emitW(node.value);           }
      case(nd_String){ emitB(PUSH);  emitW(doString(node.value)); }
      case(STORE){
         asm(node.right,code);
	 emitB(STORE); emitW(doVar(node.left.value));
      }
      case(nd_If){
	 asm(node.left,code);		# expr
	 emitB(JZ);			# if false, jump
	 p1,p2 := code.len(),0;
	 emitW(0);			# place holder for jump dest
	 asm(node.right.left,code);	# if true statements
	 if (node.right.right!=Void){
	    emitB(JMP);			# jump over else statements
	    p2=code.len();
	    emitW(0);
	 }
	 code[p1,WORD_SIZE]=(code.len() - p1).toLittleEndian(WORD_SIZE);
	 if(node.right.right!=Void){
	    asm(node.right.right,code);	# else statements
	    code[p2,WORD_SIZE]=(code.len() - p2).toLittleEndian(WORD_SIZE)
	 }
      }
      case(nd_While){
	 p1:=code.len();
	 asm(node.left,code);
	 emitB(JZ);
	 p2:=code.len();
	 emitW(0);			# place holder
	 asm(node.right,code);
	 emitB(JMP);			# jump back to the top
	 emitW(p1 - code.len());
	 code[p2,WORD_SIZE]=(code.len() - p2).toLittleEndian(WORD_SIZE);
      }
      case(nd_Sequence){ asm(node.left,code); asm(node.right,code); }
      case(PRTC,PRTI,PRTS){ asm(node.left,code); emitB(node.type); }
      else{
	 if(binOps.holds(node.type)){
	    asm(node.left,code); asm(node.right,code);
	    emitB(node.type);
	 }
	 else if(unaryOps.holds(node.type))
	    { asm(node.left,code); emitB(node.type); }
	 else throw(Exception.AssertionError(
	    "error in code generator - found %d, expecting operator"
	    .fmt(node.type)))
      }
   }
   code
}
fcn code_finish(code){
   code.append(HALT);
   // prepend the strings to the code,
   // using my magic [66,1 byte len,text], no trailing '\0' needed
   idxs:=strings.pump(Dictionary(),"reverse");
   idxs.keys.sort().reverse().pump(Void,'wrap(n){
      text:=idxs[n];
      code.insert(0,66,text.len(),text);
   })
}
```


```zkl
fcn unasm(code){
   all_ops,nthString := all_syms.pump(Dictionary(),"reverse"),-1;
   println("Datasize: %d bytes, Strings: %d bytes"
      .fmt(vars.len()*WORD_SIZE,strings.reduce(fcn(s,[(k,v)]){ s+k.len() },0)));
   word:='wrap(pc){ code.toLittleEndian(pc,WORD_SIZE,False) };  // signed
   pc:=0; while(pc<code.len()){
      op:=code[pc]; print("%4d: %2d ".fmt(pc,op));
      pc+=1;
      switch(op){
         case(66){
	    n,str := code[pc], code[pc+=1,n].text;
	    println("String #%d %3d \"%s\"".fmt(nthString+=1,n,
	        Compiler.Asm.quotify(str)));
	    pc+=n;
	 }
         case(FETCH,STORE,PUSH){
	    println("%s [%d]".fmt(all_ops[op],word(pc)));
	    pc+=WORD_SIZE;
	 }
	 case(ADD,SUB,MUL,DIV,MOD,LT,GT,LE,GE,EQ,NE,AND,OR,NEG,NOT,
	      PRTC,PRTI,PRTS,HALT){ println(all_ops[op]) }
         case(JMP){
	    n:=word(pc);
            println("jmp    (%d) %d".fmt(n, pc + n));
            pc+=WORD_SIZE;
	 }
	 case(JZ){
	    n:=word(pc);
            println("jz     (%d) %d".fmt(n, pc + n));
            pc+=WORD_SIZE;
	 }
	 else throw(Exception.AssertionError("Unknown opcode %d".fmt(op)));
      }
   }
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
      return(Node(type,value));
   }
   left,right := load_ast(file),load_ast(file);
   Node(type,Void,left,right)
}
```


```zkl
ast:=load_ast(File(vm.nthArg(0)));
code:=asm(ast,Data());
code_finish(code);
unasm(code);
File("code.bin","wb").write(code);
println("Wrote %d bytes to code.bin".fmt(code.len()));
```

File ast.txt is the text at the start of this task.
```txt

$ zkl codeGen.zkl ast.txt
Datasize: 4 bytes, Strings: 11 bytes
   0: 66 String #0  10 "\ncount is:"
  12: 66 String #1   1 "\n"
  15:  2 Integer [1]
  20:  1 Assign [0]
  25:  0 Identifier [0]
  30:  2 Integer [10]
  35:  8 LessEqual
  36: 19 jz     (43) 80
  41:  2 Integer [0]
  46: 21 Prts
  47:  0 Identifier [0]
  52: 22 Prti
  53:  2 Integer [1]
  58: 21 Prts
  59:  0 Identifier [0]
  64:  2 Integer [1]
  69:  3 Add
  70:  1 Assign [0]
  75: 18 jmp    (-51) 25
  80: 23 halt
Wrote 81 bytes to code.bin

$ zkl hexDump code1.bin
   0: 42 0a 63 6f 75 6e 74 20 | 69 73 3a 20 42 01 0a 02   B.count is: B...
  16: 01 00 00 00 01 00 00 00 | 00 00 00 00 00 00 02 0a   ................
  32: 00 00 00 08 13 2b 00 00 | 00 02 00 00 00 00 15 00   .....+..........
  48: 00 00 00 00 16 02 01 00 | 00 00 15 00 00 00 00 00   ................
  64: 02 01 00 00 00 03 01 00 | 00 00 00 12 cd ff ff ff   ................
  80: 17

```

