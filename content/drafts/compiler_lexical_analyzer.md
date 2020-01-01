+++
title = "Compiler/lexical analyzer"
description = ""
date = 2019-09-05T09:38:34Z
aliases = []
[extra]
id = 21046
[taxonomies]
categories = []
tags = []
+++

{{task}}Lexical Analyzer

Definition from [https://en.wikipedia.org/wiki/Lexical_analysis Wikipedia]:

: ''Lexical analysis is the process of converting a sequence of characters (such as in a computer program or web page) into a sequence of tokens (strings with an identified "meaning"). A program that performs lexical analysis may be called a lexer, tokenizer, or scanner (though "scanner" is also used to refer to the first stage of a lexer).''

{{task heading}}

Create a lexical analyzer for the simple programming language specified below.  The
program should read input from a file and/or stdin, and write output to a file and/or
stdout.  If the language being used has a lexer module/library/class, it would be great
if two versions of the solution are provided:  One without the lexer module, and one with.

{{task heading|Input Specification}}

The simple programming language to be analyzed is more or less a subset of [[C]]. It supports the following tokens:

;Operators

:::{| class="wikitable"
|-
!  Name          !!  Common name         !!  Character sequence
|-
|  <tt>Op_multiply</tt>     ||  multiply              ||  <tt>*</tt>
|-
|  <tt>Op_divide</tt>       ||  divide                ||  <tt>/</tt>
|-
|  <tt>Op_mod</tt>          ||  mod                   ||  <tt>%</tt>
|-
|  <tt>Op_add</tt>          ||  plus                  ||  <tt>+</tt>
|-
|  <tt>Op_subtract</tt>     ||  minus                 ||  <tt>-</tt>
|-
|  <tt>Op_negate</tt>       ||  unary minus           ||  <tt>-</tt>
|-
|  <tt>Op_less</tt>         ||  less than             ||  <tt><</tt>
|-
|  <tt>Op_lessequal</tt>    ||  less than or equal    ||  <tt><=</tt>
|-
|  <tt>Op_greater</tt>      ||  greater than          ||  <tt>&gt;</tt>
|-
|  <tt>Op_greaterequal</tt> ||  greater than or equal ||  <tt>&gt;=</tt>
|-
|  <tt>Op_equal</tt>        ||  equal                 ||  <tt>==</tt>
|-
|  <tt>Op_notequal</tt>     ||  not equal             ||  <tt>&#33;=</tt>
|-
|  <tt>Op_not</tt>          ||  unary not             ||  <tt>&#33;</tt>
|-
|  <tt>Op_assign</tt>       ||  assignment            ||  <tt>=</tt>
|-
|  <tt>Op_and</tt>          ||  logical and           ||  <tt>&amp;&amp;</tt>
|-
|  <tt>Op_or</tt>           ||  logical or            ||  <tt>&brvbar;&brvbar;</tt>
|}

* The <code>-</code> token should always be interpreted as <tt>Op_subtract</tt> by the lexer. Turning some <tt>Op_subtract</tt> into <tt>Op_negate</tt> will be the job of the syntax analyzer, which is not part of this task.

;Symbols

:::{| class="wikitable"
|-
!  Name        !!  Common name        !!  Character
|-
|  <tt>LeftParen</tt>   ||  left parenthesis   ||  <tt>(</tt>
|-
|  <tt>RightParen</tt>  ||  right parenthesis  ||  <tt>)</tt>
|-
|  <tt>LeftBrace</tt>   ||  left brace         ||  <tt>{</tt>
|-
|  <tt>RightBrace</tt>  ||  right brace        ||  <tt>}</tt>
|-
|  <tt>Semicolon</tt>   ||  semi-colon         ||  <tt>&#59;</tt>
|-
|  <tt>Comma</tt>       ||  comma              ||  <tt>,</tt>
|}

;Keywords

:::{| class="wikitable"
|-
!  Name           ||  Character sequence
|-
|  <tt>Keyword_if</tt>     ||  <tt>if</tt>
|-
|  <tt>Keyword_else</tt>   ||  <tt>else</tt>
|-
|  <tt>Keyword_while</tt>  ||  <tt>while</tt>
|-
|  <tt>Keyword_print</tt>  ||  <tt>print</tt>
|-
|  <tt>Keyword_putc</tt>   ||  <tt>putc</tt>
|}

;Identifiers and literals

These differ from the the previous tokens, in that each occurrence of them has a value associated with it.

:::{| class="wikitable"
|-
!  Name
!  Common name
!  Format description
!  Format regex
!  Value
|-
|  <tt>Identifier</tt>
|  identifier
|  one or more letter/number/underscore characters, but not starting with a number
|  <code style="white-space:nowrap">[_a-zA-Z][_a-zA-Z0-9]*</code>
|  as is
|-
|  <tt>Integer</tt>
|  integer literal
|  one or more digits
|  <code>[0-9]+</code>
|  as is, interpreted as a number
|-
|  <tt>Integer</tt>
|  char literal
|  exactly one character (anything except newline or single quote) or one of the allowed escape sequences, enclosed by single quotes
|  <code><nowiki>'([^'\n]|\\n|\\\\)'</nowiki></code>
|  the ASCII code point number of the character, e.g. 65 for <code>'A'</code> and 10 for <code>'\n'</code>
|-
|  <tt>String</tt>
|  string literal
|  zero or more characters (anything except newline or double quote), enclosed by double quotes
|  <code>"[^"\n]*"</code>
|  the characters without the double quotes and with escape sequences converted
|}

* For char and string literals, the <code>\n</code> escape sequence is supported to represent a new-line character.
* For char and string literals, to represent a backslash, use <code>\\</code>.
* No other special sequences are supported. This means that:
** Char literals cannot represent a single quote character (value 39).
** String literals cannot represent strings containing double quote characters.

;Zero-width tokens

:::{| class="wikitable"
|-
!  Name           ||  Location
|-
|  <tt>End_of_input</tt>     ||  when the end of the input stream is reached
|}

;White space

* Zero or more whitespace characters, or comments enclosed in <code>/* ... */</code>, are allowed between any two tokens, with the exceptions noted below.
* "Longest token matching" is used to resolve conflicts (e.g., in order to match '''<=''' as a single token rather than the two tokens '''<''' and '''=''').
* Whitespace is ''required'' between two tokens that have an alphanumeric character or underscore at the edge.
** This means: keywords, identifiers, and integer literals.
** e.g. <code>ifprint</code> is recognized as an identifier, instead of the keywords <tt>if</tt> and <tt>print</tt>.
** e.g. <code>42fred</code> is invalid, and neither recognized as a number nor an identifier.
* Whitespace is ''not allowed'' inside of tokens (except for chars and strings where they are part of the value).
** e.g. <code>& &</code> is invalid, and not interpreted as the <tt>&&</tt> operator.

For example, the following two program fragments are equivalent, and should produce the same token stream except for the line and column positions:

*
```c
if ( p /* meaning n is prime */ ) {
    print ( n , " " ) ;
    count = count + 1 ; /* number of primes found so far */
}
```

*
```c
if(p){print(n," ");count=count+1;}
```


;Complete list of token names


```txt

End_of_input  Op_multiply   Op_divide     Op_mod       Op_add     Op_subtract
Op_negate     Op_not        Op_less       Op_lessequal Op_greater Op_greaterequal
Op_equal      Op_notequal   Op_assign     Op_and       Op_or      Keyword_if
Keyword_else  Keyword_while Keyword_print Keyword_putc LeftParen  RightParen
LeftBrace     RightBrace    Semicolon     Comma        Identifier Integer
String

```


{{task heading|Output Format}}

The program output should be a sequence of lines, each consisting of the following whitespace-separated fields:

# the line number where the token starts
# the column number where the token starts
# the token name
# the token value (only for <tt>Identifier</tt>, <tt>Integer</tt>, and <tt>String</tt> tokens)
# the number of spaces between fields is up to you.  Neatly aligned is nice, but not a requirement.

{{task heading|Diagnostics}}

The following error conditions should be caught:

:::{| class="wikitable"
|-
! Error
! Example
|-
| Empty character constant
| <code>&apos;&apos;</code>
|-
| Unknown escape sequence.
| <code>\r</code>
|-
| Multi-character constant.
| <code>&apos;xx&apos;</code>
|-
| End-of-file in comment.     Closing comment characters not found.
|-
| End-of-file while scanning string literal. Closing string character not found.
|-
| End-of-line while scanning string literal. Closing string character not found before end-of-line.
|-
| Unrecognized character.
| <code>&#124;</code>
|-
| Invalid number. Starts like a number, but ends in non-numeric characters.
| <code>123abc</code>
|}

{{task heading|Test Cases}}
:{| class="wikitable"
|-
! Input
! Output
|-
| style="vertical-align:top" |
Test Case 1:

```c
/*
  Hello world
 */
print("Hello, World!\n");
```


| style="vertical-align:top" |
<b>
```txt

    4      1 Keyword_print
    4      6 LeftParen
    4      7 String         "Hello, World!\n"
    4     24 RightParen
    4     25 Semicolon
    5      1 End_of_input

```
</b>

|-
| style="vertical-align:top" |
Test Case 2:

```c
/*
  Show Ident and Integers
 */
phoenix_number = 142857;
print(phoenix_number, "\n");
```


| style="vertical-align:top" |
<b>
```txt

    4      1 Identifier     phoenix_number
    4     16 Op_assign
    4     18 Integer         142857
    4     24 Semicolon
    5      1 Keyword_print
    5      6 LeftParen
    5      7 Identifier     phoenix_number
    5     21 Comma
    5     23 String         "\n"
    5     27 RightParen
    5     28 Semicolon
    6      1 End_of_input

```
</b>

|-
| style="vertical-align:top" |
Test Case 3:

```c
/*
  All lexical tokens - not syntactically correct, but that will
  have to wait until syntax analysis
 */
/* Print   */  print    /* Sub     */  -
/* Putc    */  putc     /* Lss     */  <
/* If      */  if       /* Gtr     */  >
/* Else    */  else     /* Leq     */  <=
/* While   */  while    /* Geq     */  >=
/* Lbrace  */  {        /* Eq      */  ==
/* Rbrace  */  }        /* Neq     */  !=
/* Lparen  */  (        /* And     */  &&
/* Rparen  */  )        /* Or      */  ||
/* Uminus  */  -        /* Semi    */  ;
/* Not     */  !        /* Comma   */  ,
/* Mul     */  *        /* Assign  */  =
/* Div     */  /        /* Integer */  42
/* Mod     */  %        /* String  */  "String literal"
/* Add     */  +        /* Ident   */  variable_name
/* character literal */  '\n'
/* character literal */  '\\'
/* character literal */  ' '
```


| style="vertical-align:top" |
<b>
```txt

    5     16   Keyword_print
    5     40   Op_subtract
    6     16   Keyword_putc
    6     40   Op_less
    7     16   Keyword_if
    7     40   Op_greater
    8     16   Keyword_else
    8     40   Op_lessequal
    9     16   Keyword_while
    9     40   Op_greaterequal
   10     16   LeftBrace
   10     40   Op_equal
   11     16   RightBrace
   11     40   Op_notequal
   12     16   LeftParen
   12     40   Op_and
   13     16   RightParen
   13     40   Op_or
   14     16   Op_subtract
   14     40   Semicolon
   15     16   Op_not
   15     40   Comma
   16     16   Op_multiply
   16     40   Op_assign
   17     16   Op_divide
   17     40   Integer             42
   18     16   Op_mod
   18     40   String          "String literal"
   19     16   Op_add
   19     40   Identifier      variable_name
   20     26   Integer             10
   21     26   Integer             92
   22     26   Integer             32
   23      1   End_of_input

```
</b>

|-
| style="vertical-align:top" |
Test Case 4:

```c
/*** test printing, embedded \n and comments with lots of '*' ***/
print(42);
print("\nHello World\nGood Bye\nok\n");
print("Print a slash n - \\n.\n");
```


| style="vertical-align:top" |
<b>
```txt

    2      1 Keyword_print
    2      6 LeftParen
    2      7 Integer            42
    2      9 RightParen
    2     10 Semicolon
    3      1 Keyword_print
    3      6 LeftParen
    3      7 String          "\nHello World\nGood Bye\nok\n"
    3     38 RightParen
    3     39 Semicolon
    4      1 Keyword_print
    4      6 LeftParen
    4      7 String          "Print a slash n - \\n.\n"
    4     33 RightParen
    4     34 Semicolon
    5      1 End_of_input

```
</b>

|}

;Additional examples
Your solution should pass all the test cases above and the additional tests found '''[[Compiler/Sample_programs|Here]]'''.

{{task heading|Reference}}
The C and Python versions can be considered reference implementations.


;Related Tasks
* [[Compiler/syntax_analyzer|Syntax Analyzer task]]
* [[Compiler/code_generator|Code Generator task]]
* [[Compiler/virtual_machine_interpreter|Virtual Machine Interpreter task]]
* [[Compiler/AST_interpreter|AST Interpreter task]]
<hr>





## ALGOL W


```algolw
begin
    %lexical analyser %
    % Algol W strings are limited to 256 characters in length so we limit source lines %
    % and tokens to 256 characters %

    integer     lineNumber, columnNumber;
    string(256) line;
    string(256) tkValue;
    integer     tkType, tkLine, tkColumn, tkLength, tkIntegerValue;
    logical     tkTooLong;
    string(1)   currChar;
    string(1)   newlineChar;

    integer     LINE_WIDTH, MAX_TOKEN_LENGTH, MAXINTEGER_OVER_10, MAXINTEGER_MOD_10;
    integer     tOp_multiply   , tOp_divide        , tOp_mod       , tOp_add
          ,     tOp_subtract   , tOp_negate        , tOp_less      , tOp_lessequal
          ,     tOp_greater    , tOp_greaterequal  , tOp_equal     , tOp_notequal
          ,     tOp_not        , tOp_assign        , tOp_and       , tOp_or
          ,     tLeftParen     , tRightParen       , tLeftBrace    , tRightBrace
          ,     tSemicolon     , tComma            , tKeyword_if   , tKeyword_else
          ,     tKeyword_while , tKeyword_print    , tKeyword_putc , tIdentifier
          ,     tInteger       , tString           , tEnd_of_input , tComment
          ;

    string(16)  array tkName ( 1 :: 32 );

    % reports an error %
    procedure lexError( string(80) value message ); begin
        integer errorPos;
        write( i_w := 1, s_w := 0, "**** Error at(", lineNumber, ",", columnNumber, "): " );
        errorPos := 0;
        while errorPos < 80 and message( errorPos // 1 ) not = "." do begin
            writeon( s_w := 0, message( errorPos // 1 ) );
            errorPos := errorPos + 1
        end while_not_at_end_of_message ;
        writeon( s_w := 0, "." )
    end lexError ;

    % gets the next source character %
    procedure nextChar ; begin
        if      columnNumber = LINE_WIDTH then begin
            currChar     := newlineChar;
            columnNumber := columnNumber + 1
            end
        else if columnNumber > LINE_WIDTH then begin
            readcard( line );
            columnNumber := 1;
            if not XCPNOTED(ENDFILE) then lineNumber := lineNumber + 1;
            currChar     := line( 0 // 1 )
            end
        else begin
            currChar     := line( columnNumber // 1 );
            columnNumber := columnNumber + 1
        end
    end nextChar ;

    % gets the next token, returns the token type %
    integer procedure nextToken ; begin

        % returns true if currChar is in the inclusive range lowerValue to upperValue %
        %         false otherwise %
        logical procedure range( string(1) value lowerValue, upperValue ) ; begin
            currChar >= lowerValue and currChar <= upperValue
        end range ;

        % returns true if the current character can start an identifier, false otherwise %
        logical procedure identifierStartChar ; begin
            currChar = "_" or range( "a", "z" ) or range( "A", "Z" )
        end identifierStartChar ;

        % add the current character to the token and get the next %
        procedure addAndNextChar ; begin
            if tkLength >= MAX_TOKEN_LENGTH then tkTooLong := true
            else begin
                tkValue( tkLength // 1 ) := currChar;
                tkLength                 := tkLength + 1
            end if_symbol_not_too_long ;
            nextChar
        end % addAndNextChar % ;

        % handle a single character token %
        procedure singleCharToken( integer value tokenType ) ; begin
            tkType := tokenType;
            nextChar
        end singleCharToken ;

        % handle a doubled character token: && or || %
        procedure doubleCharToken( integer value tokenType ) ; begin
            string(1) firstChar;
            firstChar := currChar;
            tkType    := tokenType;
            nextChar;
            if currChar = firstChar then nextChar
            else % the character wasn't doubled % lexError( "Unrecognised character." );
        end singleCharToken ;

        % handle an operator or operator= token %
        procedure opOrOpEqual( integer value opToken, opEqualToken ) ; begin
            tkType := opToken;
            nextChar;
            if currChar = "=" then begin
                % have operator= %
                tkType := opEqualToken;
                nextChar
            end if_currChar_is_equal ;
        end opOrOpEqual ;

        % handle a / operator or /* comment %
        procedure divideOrComment ; begin
            tkType := tOp_divide;
            nextChar;
            if currChar = "*" then begin
                % have a comment %
                logical moreComment;
                tkType      := tComment;
                moreComment := true;
                while moreComment do begin
                    nextChar;
                    while currChar not = "*" and not XCPNOTED(ENDFILE) do nextChar;
                    while currChar     = "*" and not XCPNOTED(ENDFILE) do nextChar;
                    moreComment := ( currChar not = "/" and not XCPNOTED(ENDFILE) )
                end while_more_comment ;
                if not XCPNOTED(ENDFILE)
                then nextChar
                else lexError( "End-of-file in comment." )
            end if_currChar_is_star ;
        end divideOrComment ;

        % handle an indentifier or keyword %
        procedure identifierOrKeyword ; begin
            tkType := tIdentifier;
            while identifierStartChar or range( "0", "9" ) do addAndNextChar;
            % there are only 5 keywords, so we just test each in turn here %
            if      tkValue = "if"      then tkType  := tKeyword_if
            else if tkValue = "else"    then tkType  := tKeyword_else
            else if tkValue = "while"   then tkType  := tKeyword_while
            else if tkValue = "print"   then tkType  := tKeyword_print
            else if tkValue = "putc"    then tkType  := tKeyword_putc;
            if tkType not = tIdentifier then tkValue := "";
        end identifierOrKeyword ;

        % handle an integer literal %
        procedure integerLiteral ; begin
            logical overflowed;
            integer digit;
            overflowed := false;
            tkType     := tInteger;
            while range( "0", "9" ) do begin
                digit := ( decode( currChar ) - decode( "0" ) );
                if      tkIntegerValue > MAXINTEGER_OVER_10 then overflowed := true
                else if tkIntegerValue = MAXINTEGER_OVER_10
                    and digit          > MAXINTEGER_MOD_10  then overflowed := true
                else begin
                    tkIntegerValue := tkIntegerValue * 10;
                    tkIntegerValue := tkIntegerValue + digit;
                end;
                nextChar
            end while_have_a_digit ;
            if overflowed          then lexError( "Number too large." );
            if identifierStartChar then lexError( "Number followed by letter or underscore." );
        end integerLiteral ;

        % handle a char literal %
        procedure charLiteral ; begin
            nextChar;
            if      currChar = "'" or currChar = newlineChar then lexError( "Invalid character constant." )
            else if currChar = "\" then begin
                % have an escape %
                nextChar;
                if      currChar     = "n" then currChar := newlineChar
                else if currChar not = "\" then lexError( "Unknown escape sequence." )
            end;
            tkType         := tInteger;
            tkIntegerValue := decode( currChar );
            % should have a closing quoute next %
            nextChar;
            if   currChar not = "'"
            then lexError( "Multi-character constant." )
            else nextChar
        end charLiteral ;

        % handle a string literal %
        procedure stringLiteral ; begin
            tkType            := tString;
            tkValue( 0 // 1 ) := currChar;
            tkLength          := 1;
            nextChar;
            while currChar not = """" and currChar not = newlineChar and not XCPNOTED(ENDFILE) do addAndNextChar;
            if      currChar = newlineChar then lexError( "End-of-line while scanning string literal." )
            else if XCPNOTED(ENDFILE)      then lexError( "End-of-file while scanning string literal." )
            else    % currChar must be """" % addAndNextChar
        end stringLiteral ;

        while begin
            % skip white space %
            while ( currChar = " " or currChar = newlineChar ) and not XCPNOTED(ENDFILE) do nextChar;
            % get the token %
            tkLine         := lineNumber;
            tkColumn       := columnNumber;
            tkValue        := "";
            tkLength       := 0;
            tkIntegerValue := 0;
            tkTooLong      := false;
            if      XCPNOTED(ENDFILE)   then tkType := tEnd_of_input
            else if currChar = "*"      then singleCharToken( tOp_multiply )
            else if currChar = "/"      then divideOrComment
            else if currChar = "%"      then singleCharToken( tOp_mod      )
            else if currChar = "+"      then singleCharToken( tOp_add )
            else if currChar = "-"      then singleCharToken( tOp_subtract )
            else if currChar = "<"      then opOrOpEqual( tOp_less,    tOp_lessequal     )
            else if currChar = ">"      then opOrOpEqual( tOp_greater, tOp_greaterequal  )
            else if currChar = "="      then opOrOpEqual( tOp_assign,  tOp_equal         )
            else if currChar = "!"      then opOrOpEqual( tOp_not,     tOp_notequal      )
            else if currChar = "&"      then doubleCharToken( tOp_and     )
            else if currChar = "|"      then doubleCharToken( tOp_or      )
            else if currChar = "("      then singleCharToken( tLeftParen  )
            else if currChar = ")"      then singleCharToken( tRightParen )
            else if currChar = "{"      then singleCharToken( tLeftBrace  )
            else if currChar = "}"      then singleCharToken( tRightBrace )
            else if currChar = ";"      then singleCharToken( tSemicolon  )
            else if currChar = ","      then singleCharToken( tComma      )
            else if identifierStartChar then identifierOrKeyword
            else if range( "0", "9" )   then integerLiteral
            else if currChar = "'"      then charLiteral
            else if currChar = """"     then stringLiteral
            else begin
                lexError( "Unrecognised character." );
                singleCharToken( tComment )
            end ;
            % continue until we get something other than a comment %
            tkType = tComment
        end do begin end;
        if tkTooLong then if   tkType = tString
                          then lexError( "String literal too long." )
                          else lexError( "Identifier too long."     );
        tkType
    end nextToken ;

    % outputs the current token %
    procedure writeToken ; begin
        write( i_w := 5, s_w := 2, tkLine, tkColumn, tkName( tkType ) );
        if tkType = tInteger then writeon( i_w := 11, tkIntegerValue )
        else if tkLength > 0 then begin
            writeon( "  " );
            for tkPos := 0 until tkLength - 1 do writeon( s_w := 0, tkValue( tkPos // 1 ) );
        end
    end writeToken ;

    LINE_WIDTH       := 256; MAXINTEGER_MOD_10  := MAXINTEGER rem 10;
    MAX_TOKEN_LENGTH := 256; MAXINTEGER_OVER_10 := MAXINTEGER div 10;
    newlineChar      := code( 10 );
    tOp_multiply     :=  1; tkName( tOp_multiply     ) := "Op_multiply";
    tOp_divide       :=  2; tkName( tOp_divide       ) := "Op_divide";
    tOp_mod          :=  3; tkName( tOp_mod          ) := "Op_mod";
    tOp_add          :=  4; tkName( tOp_add          ) := "Op_add";
    tOp_subtract     :=  5; tkName( tOp_subtract     ) := "Op_subtract";
    tOp_negate       :=  6; tkName( tOp_negate       ) := "Op_negate";
    tOp_less         :=  7; tkName( tOp_less         ) := "Op_less";
    tOp_lessequal    :=  8; tkName( tOp_lessequal    ) := "Op_lessequal";
    tOp_greater      :=  9; tkName( tOp_greater      ) := "Op_greater";
    tOp_greaterequal := 10; tkName( tOp_greaterequal ) := "Op_greaterequal";
    tOp_equal        := 11; tkName( tOp_equal        ) := "Op_equal";
    tOp_notequal     := 12; tkName( tOp_notequal     ) := "Op_notequal";
    tOp_not          := 13; tkName( tOp_not          ) := "Op_not";
    tOp_assign       := 14; tkName( tOp_assign       ) := "Op_assign";
    tOp_and          := 15; tkName( tOp_and          ) := "Op_and";
    tOp_or           := 16; tkName( tOp_or           ) := "Op_or";
    tLeftParen       := 17; tkName( tLeftParen       ) := "LeftParen";
    tRightParen      := 18; tkName( tRightParen      ) := "RightParen";
    tLeftBrace       := 19; tkName( tLeftBrace       ) := "LeftBrace";
    tRightBrace      := 20; tkName( tRightBrace      ) := "RightBrace";
    tSemicolon       := 21; tkName( tSemicolon       ) := "Semicolon";
    tComma           := 22; tkName( tComma           ) := "Comma";
    tKeyword_if      := 23; tkName( tKeyword_if      ) := "Keyword_if";
    tKeyword_else    := 24; tkName( tKeyword_else    ) := "Keyword_else";
    tKeyword_while   := 25; tkName( tKeyword_while   ) := "Keyword_while";
    tKeyword_print   := 26; tkName( tKeyword_print   ) := "Keyword_print";
    tKeyword_putc    := 27; tkName( tKeyword_putc    ) := "Keyword_putc";
    tIdentifier      := 28; tkName( tIdentifier      ) := "Identifier";
    tInteger         := 29; tkName( tInteger         ) := "Integer";
    tString          := 30; tkName( tString          ) := "String";
    tEnd_of_input    := 31; tkName( tEnd_of_input    ) := "End_of_input";
    tComment         := 32; tkName( tComment         ) := "Comment";

    % allow the program to continue after reaching end-of-file %
    ENDFILE := EXCEPTION( false, 1, 0, false, "EOF" );
    % ensure the first call to nextToken reads the first line %
    lineNumber   := 0;
    columnNumber := LINE_WIDTH + 1;
    currChar     := " ";
    % get and print all tokens from standard input %
    while nextToken not = tEnd_of_input do writeToken;
    writeToken
end.
```

{{out}} Test case 3:

```txt

    5     16  Keyword_print
    5     40  Op_subtract
    6     16  Keyword_putc
    6     40  Op_less
    7     16  Keyword_if
    7     40  Op_greater
    8     16  Keyword_else
    8     40  Op_lessequal
    9     16  Keyword_while
    9     40  Op_greaterequal
   10     16  LeftBrace
   10     40  Op_equal
   11     16  RightBrace
   11     40  Op_notequal
   12     16  LeftParen
   12     40  Op_and
   13     16  RightParen
   13     40  Op_or
   14     16  Op_subtract
   14     40  Semicolon
   15     16  Op_not
   15     40  Comma
   16     16  Op_multiply
   16     40  Op_assign
   17     16  Op_divide
   17     40  Integer                  42
   18     16  Op_mod
   18     40  String              "String literal"
   19     16  Op_add
   19     40  Identifier          variable_name
   20     26  Integer                  10
   21     26  Integer                  92
   22     26  Integer                  32
   23      1  End_of_input

```



## AWK

Tested with gawk 4.1.1 and mawk 1.3.4.

```AWK

BEGIN {
  all_syms["tk_EOI"    ] = "End_of_input"
  all_syms["tk_Mul"    ] = "Op_multiply"
  all_syms["tk_Div"    ] = "Op_divide"
  all_syms["tk_Mod"    ] = "Op_mod"
  all_syms["tk_Add"    ] = "Op_add"
  all_syms["tk_Sub"    ] = "Op_subtract"
  all_syms["tk_Negate" ] = "Op_negate"
  all_syms["tk_Not"    ] = "Op_not"
  all_syms["tk_Lss"    ] = "Op_less"
  all_syms["tk_Leq"    ] = "Op_lessequal"
  all_syms["tk_Gtr"    ] = "Op_greater"
  all_syms["tk_Geq"    ] = "Op_greaterequal"
  all_syms["tk_Eq"     ] = "Op_equal"
  all_syms["tk_Neq"    ] = "Op_notequal"
  all_syms["tk_Assign" ] = "Op_assign"
  all_syms["tk_And"    ] = "Op_and"
  all_syms["tk_Or"     ] = "Op_or"
  all_syms["tk_If"     ] = "Keyword_if"
  all_syms["tk_Else"   ] = "Keyword_else"
  all_syms["tk_While"  ] = "Keyword_while"
  all_syms["tk_Print"  ] = "Keyword_print"
  all_syms["tk_Putc"   ] = "Keyword_putc"
  all_syms["tk_Lparen" ] = "LeftParen"
  all_syms["tk_Rparen" ] = "RightParen"
  all_syms["tk_Lbrace" ] = "LeftBrace"
  all_syms["tk_Rbrace" ] = "RightBrace"
  all_syms["tk_Semi"   ] = "Semicolon"
  all_syms["tk_Comma"  ] = "Comma"
  all_syms["tk_Ident"  ] = "Identifier"
  all_syms["tk_Integer"] = "Integer"
  all_syms["tk_String" ] = "String"

  ## single character only symbols
  symbols["{"   ] = "tk_Lbrace"
  symbols["}"   ] = "tk_Rbrace"
  symbols["("   ] = "tk_Lparen"
  symbols[")"   ] = "tk_Rparen"
  symbols["+"   ] = "tk_Add"
  symbols["-"   ] = "tk_Sub"
  symbols["*"   ] = "tk_Mul"
  symbols["%"   ] = "tk_Mod"
  symbols[";"   ] = "tk_Semi"
  symbols[","   ] = "tk_Comma"

  key_words["if"   ] = "tk_If"
  key_words["else" ] = "tk_Else"
  key_words["print"] = "tk_Print"
  key_words["putc" ] = "tk_Putc"
  key_words["while"] = "tk_While"

  # Set up an array that emulates the ord() function.
  for(n=0;n<256;n++)
    ord[sprintf("%c",n)]=n

  input_file = "-"
  if (ARGC > 1)
    input_file = ARGV[1]
  RS=FS=""   # read complete file into one line $0
  getline < input_file
  the_ch = " " # dummy first char - but it must be a space
  the_col  = 0 # always points to the current character
  the_line = 1
  for (the_nf=1; ; ) {
    split(gettok(), t, SUBSEP)
    printf("%5s  %5s %-14s", t[2], t[3], all_syms[t[1]])
    if      (t[1] == "tk_Integer") printf("   %5s\n", t[4])
    else if (t[1] == "tk_Ident"  ) printf("  %s\n",   t[4])
    else if (t[1] == "tk_String" ) printf("  \"%s\"\n", t[4])
    else                           print("")
    if (t[1] == "tk_EOI")
      break
  }
}

#*** show error and exit
function error(line, col, msg) {
  print(line, col, msg)
  exit(1)
}

# get the next character from the input
function next_ch() {
  the_ch = $the_nf
  the_nf  ++
  the_col ++
  if (the_ch == "\n") {
    the_line ++
    the_col = 0
  }
  return the_ch
}

#*** 'x' - character constants
function char_lit(err_line, err_col) {
  n = ord[next_ch()]              # skip opening quote
  if (the_ch == "'") {
    error(err_line, err_col, "empty character constant")
  } else if (the_ch == "\\") {
    next_ch()
    if (the_ch == "n")
      n = 10
    else if (the_ch == "\\")
      n = ord["\\"]
    else
      error(err_line, err_col, "unknown escape sequence " the_ch)
  }
  if (next_ch() != "'")
    error(err_line, err_col, "multi-character constant")
  next_ch()
  return "tk_Integer" SUBSEP err_line SUBSEP err_col SUBSEP n
}

#*** process divide or comments
function div_or_cmt(err_line, err_col) {
  if (next_ch() != "*")
    return "tk_Div" SUBSEP err_line SUBSEP err_col
  # comment found
  next_ch()
  while (1) {
    if (the_ch == "*") {
      if (next_ch() == "/") {
        next_ch()
        return gettok()
      } else if (the_ch == "") {
        error(err_line, err_col, "EOF in comment")
      }
    } else {
      next_ch()
    }
  }
}

#*** "string"
function string_lit(start, err_line, err_col) {
  text = ""
  while (next_ch() != start) {
    if (the_ch == "")
      error(err_line, err_col, "EOF while scanning string literal")
    if (the_ch == "\n")
      error(err_line, err_col, "EOL while scanning string literal")
    text = text the_ch
  }
  next_ch()
  return "tk_String" SUBSEP err_line SUBSEP err_col SUBSEP text
}

#*** handle identifiers and integers
function ident_or_int(err_line, err_col) {
  is_number = 1
  text = ""
  while ((the_ch ~ /^[0-9a-zA-Z]+$/)  || (the_ch == "_")) {
    text = text the_ch
    if (! (the_ch ~ /^[0-9]+$/))
      is_number = 0
    next_ch()
  }
  if (text == "")
    error(err_line, err_col, "ident_or_int: unrecognized character: " the_ch)
  if (text ~ /^[0-9]/) {
    if (! is_number)
      error(err_line, err_col, "invalid number: " text)
    n = text + 0
    return "tk_Integer" SUBSEP err_line SUBSEP err_col SUBSEP n
  }
  if (text in key_words)
    return key_words[text] SUBSEP err_line SUBSEP err_col
  return "tk_Ident" SUBSEP err_line SUBSEP err_col SUBSEP text
}

#*** look ahead for '>=', etc.
function follow(expect, ifyes, ifno, err_line, err_col) {
  if (next_ch() == expect) {
    next_ch()
    return ifyes SUBSEP err_line SUBSEP err_col
  }
  if (ifno == tk_EOI)
    error(err_line, err_col, "follow: unrecognized character: " the_ch)
  return ifno SUBSEP err_line SUBSEP err_col
}

#*** return the next token type
function gettok() {
  while (the_ch == " " || the_ch == "\n" || the_ch == "\r")
    next_ch()
  err_line = the_line
  err_col  = the_col
  if      (the_ch == "" )    return "tk_EOI" SUBSEP err_line SUBSEP err_col
  else if (the_ch == "/")    return div_or_cmt(err_line, err_col)
  else if (the_ch == "'")    return char_lit(err_line, err_col)
  else if (the_ch == "<")    return follow("=", "tk_Leq", "tk_Lss",    err_line, err_col)
  else if (the_ch == ">")    return follow("=", "tk_Geq", "tk_Gtr",    err_line, err_col)
  else if (the_ch == "=")    return follow("=", "tk_Eq",  "tk_Assign", err_line, err_col)
  else if (the_ch == "!")    return follow("=", "tk_Neq", "tk_Not",    err_line, err_col)
  else if (the_ch == "&")    return follow("&", "tk_And", "tk_EOI",    err_line, err_col)
  else if (the_ch == "|")    return follow("|", "tk_Or",  "tk_EOI",    err_line, err_col)
  else if (the_ch =="\"")    return string_lit(the_ch, err_line, err_col)
  else if (the_ch in symbols) {
    sym = symbols[the_ch]
    next_ch()
    return sym SUBSEP err_line SUBSEP err_col
  } else {
    return ident_or_int(err_line, err_col)
  }
}

```

{{out|case=count}}
<b>

```txt

    1      1 Identifier      count
    1      7 Op_assign
    1      9 Integer              1
    1     10 Semicolon
    2      1 Keyword_while
    2      7 LeftParen
    2      8 Identifier      count
    2     14 Op_less
    2     16 Integer             10
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
    4     21 Integer              1
    4     22 Semicolon
    5      1 RightBrace
    5      3 End_of_input

```

</b>


## C

Tested with gcc 4.81 and later, compiles warning free with -Wall -Wextra

```cpp
#include <iostream>
#include <stdio.h>
#include <stdarg.h>
#include <ctype.h>
#include <string.h>
#include <errno.h>
#include <stdbool.h>
#include <limits.h>

#define NELEMS(arr) (sizeof(arr) / sizeof(arr[0]))

#define da_dim(name, type)  type *name = NULL;          \
                            int _qy_ ## name ## _p = 0;  \
                            int _qy_ ## name ## _max = 0
#define da_rewind(name)     _qy_ ## name ## _p = 0
#define da_redim(name)      do {if (_qy_ ## name ## _p >= _qy_ ## name ## _max) \
                                name = realloc(name, (_qy_ ## name ## _max += 32) * sizeof(name[0]));} while (0)
#define da_append(name, x)  do {da_redim(name); name[_qy_ ## name ## _p++] = x;} while (0)
#define da_len(name)        _qy_ ## name ## _p

typedef enum {
    tk_EOI, tk_Mul, tk_Div, tk_Mod, tk_Add, tk_Sub, tk_Negate, tk_Not, tk_Lss, tk_Leq,
    tk_Gtr, tk_Geq, tk_Eq, tk_Neq, tk_Assign, tk_And, tk_Or, tk_If, tk_Else, tk_While,
    tk_Print, tk_Putc, tk_Lparen, tk_Rparen, tk_Lbrace, tk_Rbrace, tk_Semi, tk_Comma,
    tk_Ident, tk_Integer, tk_String
} TokenType;

typedef struct {
    TokenType tok;
    int err_ln, err_col;
    union {
        int n;                  /* value for constants */
        char *text;             /* text for idents */
    };
} tok_s;

static FILE *source_fp, *dest_fp;
static int line = 1, col = 0, the_ch = ' ';
da_dim(text, char);

tok_s gettok();

static void error(int err_line, int err_col, const char *fmt, ... ) {
    char buf[1000];
    va_list ap;

    va_start(ap, fmt);
    vsprintf(buf, fmt, ap);
    va_end(ap);
    printf("(%d,%d) error: %s\n", err_line, err_col, buf);
    exit(1);
}

static int next_ch() {     /* get next char from input */
    the_ch = getc(source_fp);
    ++col;
    if (the_ch == '\n') {
        ++line;
        col = 0;
    }
    return the_ch;
}

static tok_s char_lit(int n, int err_line, int err_col) {   /* 'x' */
    if (the_ch == '\'')
        error(err_line, err_col, "gettok: empty character constant");
    if (the_ch == '\\') {
        next_ch();
        if (the_ch == 'n')
            n = 10;
        else if (the_ch == '\\')
            n = '\\';
        else error(err_line, err_col, "gettok: unknown escape sequence \\%c", the_ch);
    }
    if (next_ch() != '\'')
        error(err_line, err_col, "multi-character constant");
    next_ch();
    return (tok_s){tk_Integer, err_line, err_col, {n}};
}

static tok_s div_or_cmt(int err_line, int err_col) { /* process divide or comments */
    if (the_ch != '*')
        return (tok_s){tk_Div, err_line, err_col, {0}};

    /* comment found */
    next_ch();
    for (;;) {
        if (the_ch == '*') {
            if (next_ch() == '/') {
                next_ch();
                return gettok();
            }
        } else if (the_ch == EOF)
            error(err_line, err_col, "EOF in comment");
        else
            next_ch();
    }
}

static tok_s string_lit(int start, int err_line, int err_col) { /* "st" */
    da_rewind(text);

    while (next_ch() != start) {
        if (the_ch == '\n') error(err_line, err_col, "EOL in string");
        if (the_ch == EOF)  error(err_line, err_col, "EOF in string");
        da_append(text, (char)the_ch);
    }
    da_append(text, '\0');

    next_ch();
    return (tok_s){tk_String, err_line, err_col, {.text=text}};
}

static int kwd_cmp(const void *p1, const void *p2) {
    return strcmp(*(char **)p1, *(char **)p2);
}

static TokenType get_ident_type(const char *ident) {
    static struct {
        char *s;
        TokenType sym;
    } kwds[] = {
        {"else",  tk_Else},
        {"if",    tk_If},
        {"print", tk_Print},
        {"putc",  tk_Putc},
        {"while", tk_While},
    }, *kwp;

    return (kwp = bsearch(&ident, kwds, NELEMS(kwds), sizeof(kwds[0]), kwd_cmp)) == NULL ? tk_Ident : kwp->sym;
}

static tok_s ident_or_int(int err_line, int err_col) {
    int n, is_number = true;

    da_rewind(text);
    while (isalnum(the_ch) || the_ch == '_') {
        da_append(text, (char)the_ch);
        if (!isdigit(the_ch))
            is_number = false;
        next_ch();
    }
    if (da_len(text) == 0)
        error(err_line, err_col, "gettok: unrecognized character (%d) '%c'\n", the_ch, the_ch);
    da_append(text, '\0');
    if (isdigit(text[0])) {
        if (!is_number)
            error(err_line, err_col, "invalid number: %s\n", text);
        n = strtol(text, NULL, 0);
        if (n == LONG_MAX && errno == ERANGE)
            error(err_line, err_col, "Number exceeds maximum value");
        return (tok_s){tk_Integer, err_line, err_col, {n}};
    }
    return (tok_s){get_ident_type(text), err_line, err_col, {.text=text}};
}

static tok_s follow(int expect, TokenType ifyes, TokenType ifno, int err_line, int err_col) {   /* look ahead for '>=', etc. */
    if (the_ch == expect) {
        next_ch();
        return (tok_s){ifyes, err_line, err_col, {0}};
    }
    if (ifno == tk_EOI)
        error(err_line, err_col, "follow: unrecognized character '%c' (%d)\n", the_ch, the_ch);
    return (tok_s){ifno, err_line, err_col, {0}};
}

tok_s gettok() {            /* return the token type */
    /* skip white space */
    while (isspace(the_ch))
        next_ch();
    int err_line = line;
    int err_col  = col;
    switch (the_ch) {
        case '{':  next_ch(); return (tok_s){tk_Lbrace, err_line, err_col, {0}};
        case '}':  next_ch(); return (tok_s){tk_Rbrace, err_line, err_col, {0}};
        case '(':  next_ch(); return (tok_s){tk_Lparen, err_line, err_col, {0}};
        case ')':  next_ch(); return (tok_s){tk_Rparen, err_line, err_col, {0}};
        case '+':  next_ch(); return (tok_s){tk_Add, err_line, err_col, {0}};
        case '-':  next_ch(); return (tok_s){tk_Sub, err_line, err_col, {0}};
        case '*':  next_ch(); return (tok_s){tk_Mul, err_line, err_col, {0}};
        case '%':  next_ch(); return (tok_s){tk_Mod, err_line, err_col, {0}};
        case ';':  next_ch(); return (tok_s){tk_Semi, err_line, err_col, {0}};
        case ',':  next_ch(); return (tok_s){tk_Comma,err_line, err_col, {0}};
        case '/':  next_ch(); return div_or_cmt(err_line, err_col);
        case '\'': next_ch(); return char_lit(the_ch, err_line, err_col);
        case '<':  next_ch(); return follow('=', tk_Leq, tk_Lss,    err_line, err_col);
        case '>':  next_ch(); return follow('=', tk_Geq, tk_Gtr,    err_line, err_col);
        case '=':  next_ch(); return follow('=', tk_Eq,  tk_Assign, err_line, err_col);
        case '!':  next_ch(); return follow('=', tk_Neq, tk_Not,    err_line, err_col);
        case '&':  next_ch(); return follow('&', tk_And, tk_EOI,    err_line, err_col);
        case '|':  next_ch(); return follow('|', tk_Or,  tk_EOI,    err_line, err_col);
        case '"' : return string_lit(the_ch, err_line, err_col);
        default:   return ident_or_int(err_line, err_col);
        case EOF:  return (tok_s){tk_EOI, err_line, err_col, {0}};
    }
}

void run() {    /* tokenize the given input */
    tok_s tok;
    do {
        tok = gettok();
        fprintf(dest_fp, "%5d  %5d %.15s",
            tok.err_ln, tok.err_col,
            &"End_of_input    Op_multiply     Op_divide       Op_mod          Op_add          "
             "Op_subtract     Op_negate       Op_not          Op_less         Op_lessequal    "
             "Op_greater      Op_greaterequal Op_equal        Op_notequal     Op_assign       "
             "Op_and          Op_or           Keyword_if      Keyword_else    Keyword_while   "
             "Keyword_print   Keyword_putc    LeftParen       RightParen      LeftBrace       "
             "RightBrace      Semicolon       Comma           Identifier      Integer         "
             "String          "
            [tok.tok * 16]);
        if (tok.tok == tk_Integer)     fprintf(dest_fp, "  %4d",   tok.n);
        else if (tok.tok == tk_Ident)  fprintf(dest_fp, " %s",     tok.text);
        else if (tok.tok == tk_String) fprintf(dest_fp, " \"%s\"", tok.text);
        fprintf(dest_fp, "\n");
    } while (tok.tok != tk_EOI);
    if (dest_fp != stdout)
        fclose(dest_fp);
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
    run();
    return 0;
}
```


{{out|case=test case 3}}
<b>

```txt

    5     16 Keyword_print
    5     40 Op_subtract
    6     16 Keyword_putc
    6     40 Op_less
    7     16 Keyword_if
    7     40 Op_greater
    8     16 Keyword_else
    8     40 Op_lessequal
    9     16 Keyword_while
    9     40 Op_greaterequal
   10     16 LeftBrace
   10     40 Op_equal
   11     16 RightBrace
   11     40 Op_notequal
   12     16 LeftParen
   12     40 Op_and
   13     16 RightParen
   13     40 Op_or
   14     16 Op_subtract
   14     40 Semicolon
   15     16 Op_not
   15     40 Comma
   16     16 Op_multiply
   16     40 Op_assign
   17     16 Op_divide
   17     40 Integer            42
   18     16 Op_mod
   18     40 String          "String literal"
   19     16 Op_add
   19     40 Identifier      variable_name
   20     26 Integer            10
   21     26 Integer            92
   22     26 Integer            32
   23      1 End_of_input

```

</b>

=={{header|C sharp|C#}}==
Requires C#6.0 because of the use of null coalescing operators.

```csharp

using System;
using System.IO;
using System.Linq;
using System.Collections.Generic;


namespace Rosetta {

    public enum TokenType {
        End_of_input, Op_multiply, Op_divide, Op_mod, Op_add, Op_subtract,
        Op_negate, Op_not, Op_less, Op_lessequal, Op_greater, Op_greaterequal,
        Op_equal, Op_notequal, Op_assign, Op_and, Op_or, Keyword_if,
        Keyword_else, Keyword_while, Keyword_print, Keyword_putc, LeftParen, RightParen,
        LeftBrace, RightBrace, Semicolon, Comma, Identifier, Integer, String, None
    }

    /// <summary>
    /// Storage class for tokens
    /// </summary>
    public class Token {
        public TokenType Type { get; set; }
        public int Line { get; set; }
        public int Position { get; set; }
        public string Value { get; set; }
        public override string ToString() {
            if (Type == TokenType.Integer || Type == TokenType.Identifier) {
                return String.Format("{0,-5}  {1,-5}   {2,-14}     {3}", Line, Position, Type.ToString(), Value);
            } else if (Type == TokenType.String) {
                return String.Format("{0,-5}  {1,-5}   {2,-14}     \"{3}\"", Line, Position, Type.ToString(), Value.Replace("\n", "\\n"));
            }
            return String.Format("{0,-5}  {1,-5}   {2,-14}", Line, Position, Type.ToString());
        }
    }

    /// <summary>
    /// C# Example of Lexical scanner for Rosetta Compiler
    /// </summary>
    public class LexicalScanner {

        // character classes
        private const string _letters = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_";
        private const string _numbers = "0123456789";
        private const string _identifier = _letters + _numbers + "_";
        private const string _whitespace = " \t\n\r";

        // mappings from string keywords to token type
        private Dictionary<string, TokenType> _keywordTokenTypeMap = new Dictionary<string, TokenType>() {
            { "if", TokenType.Keyword_if },
            { "else", TokenType.Keyword_else },
            { "while", TokenType.Keyword_while },
            { "print", TokenType.Keyword_print },
            { "putc", TokenType.Keyword_putc }
        };

        // mappings from simple operators to token type
        private Dictionary<string, TokenType> _operatorTokenTypeMap = new Dictionary<string, TokenType>() {
            { "+", TokenType.Op_add },
            { "-", TokenType.Op_subtract },
            { "*", TokenType.Op_multiply },
            { "/", TokenType.Op_divide },
            { "%", TokenType.Op_mod },
            { "=", TokenType.Op_assign },
            { "<", TokenType.Op_less },
            { ">", TokenType.Op_greater },
            { "!", TokenType.Op_not },
        };

        private List<string> _keywords;
        private string _operators = "+-*/%=<>!%";

        private string _code;
        private List<Token> tokens = new List<Token>();

        private int _line = 1;
        private int _position = 1;

        public string CurrentCharacter {
            get {
                try {
                    return _code.Substring(0, 1);
                } catch (ArgumentOutOfRangeException) {
                    return "";
                }
            }
        }

        /// <summary>
        /// Lexical scanner initialiser
        /// </summary>
        /// <param name="code">Code to be tokenised</param>
        public LexicalScanner (string code) {
            _code = code;
            _keywords = _keywordTokenTypeMap.Keys.ToList();
        }

        /// <summary>
        /// Advance the cursor forward given number of characters
        /// </summary>
        /// <param name="characters">Number of characters to advance</param>
        private void advance(int characters=1) {
            try {
                // reset position when there is a newline
                if (CurrentCharacter == "\n") {
                    _position = 0;
                    _line++;
                }

                _code = _code.Substring(characters, _code.Length - characters);
                _position += characters;
            } catch (ArgumentOutOfRangeException) {
                _code = "";
            }
        }

        /// <summary>
        /// Outputs error message to the console and exits
        /// </summary>
        /// <param name="message">Error message to display to user</param>
        /// <param name="line">Line error occurred on</param>
        /// <param name="position">Line column that the error occurred at</param>
        public void error(string message, int line, int position) {
            // output error to the console and exit
            Console.WriteLine(String.Format("{0} @ {1}:{2}", message, line, position));
            Environment.Exit(1);
        }

        /// <summary>
        /// Pattern matching using first & follow matching
        /// </summary>
        /// <param name="recogniseClass">String of characters that identifies the token type
        /// or the exact match the be made if exact:true</param>
        /// <param name="matchClass">String of characters to match against remaining target characters</param>
        /// <param name="tokenType">Type of token the match represents.</param>
        /// <param name="notNextClass">Optional class of characters that cannot follow the match</param>
        /// <param name="maxLen">Optional maximum length of token value</param>
        /// <param name="exact">Denotes whether recogniseClass represents an exact match or class match.
        /// Default: false</param>
        /// <param name="discard">Denotes whether the token is kept or discarded. Default: false</param>
        /// <param name="offset">Optiona line position offset to account for discarded tokens</param>
        /// <returns>Boolean indicating if a match was made </returns>
        public bool match(string recogniseClass, string matchClass, TokenType tokenType,
                          string notNextClass=null, int maxLen=Int32.MaxValue, bool exact=false,
                          bool discard=false, int offset=0) {

            // if we've hit the end of the file, there's no more matching to be done
            if (CurrentCharacter == "")
                return false;

            // store _current_ line and position so that our vectors point at the start
            // of each token
            int line = _line;
            int position = _position;

            // special case exact tokens to avoid needing to worry about backtracking
            if (exact) {
                if (_code.StartsWith(recogniseClass)) {
                    if (!discard)
                        tokens.Add(new Token() { Type = tokenType, Value = recogniseClass, Line = line, Position = position - offset});
                    advance(recogniseClass.Length);
                    return true;
                }
                return false;
            }

            // first match - denotes the token type usually
            if (!recogniseClass.Contains(CurrentCharacter))
                return false;

            string tokenValue = CurrentCharacter;
            advance();

            // follow match while we haven't exceeded maxLen and there are still characters
            // in the code stream
            while ((matchClass ?? "").Contains(CurrentCharacter) && tokenValue.Length <= maxLen && CurrentCharacter != "") {
                tokenValue += CurrentCharacter;
                advance();
            }

            // ensure that any incompatible characters are not next to the token
            // eg 42fred is invalid, and neither recognized as a number nor an identifier.
            // _letters would be the notNextClass
            if (notNextClass != null && notNextClass.Contains(CurrentCharacter))
                error("Unrecognised character: " + CurrentCharacter, _line, _position);

            // only add tokens to the stack that aren't marked as discard - dont want
            // things like open and close quotes/comments
            if (!discard) {
                Token token = new Token() { Type = tokenType, Value = tokenValue, Line = line, Position = position - offset };
                tokens.Add(token);
            }

            return true;
        }

        /// <summary>
        /// Tokenise the input code
        /// </summary>
        /// <returns>List of Tokens</returns>
        public List<Token> scan() {

            while (CurrentCharacter != "") {
                // match whitespace
                match(_whitespace, _whitespace, TokenType.None, discard: true);

                // match integers
                match(_numbers, _numbers, TokenType.Integer, notNextClass:_letters);

                // match identifiers and keywords
                if (match(_letters, _identifier, TokenType.Identifier)) {
                    Token match = tokens.Last();
                    if (_keywords.Contains(match.Value))
                        match.Type = _keywordTokenTypeMap[match.Value];
                }

                // match string similarly to comments without allowing newlines
                // this token doesn't get discarded though
                if (match("\"", null, TokenType.String, discard:true)) {
                    string value = "";
                    int position = _position;
                    while (!match("\"", null, TokenType.String, discard:true)) {
                        // not allowed newlines in strings
                        if (CurrentCharacter == "\n")
                            error("End-of-line while scanning string literal. Closing string character not found before end-of-line", _line, _position);
                        // end of file reached before finding end of string
                        if (CurrentCharacter == "")
                            error("End-of-file while scanning string literal. Closing string character not found", _line, _position);

                        value += CurrentCharacter;

                        // deal with escape sequences - we only accept newline (\n)
                        if (value.Length >= 2) {
                            string lastCharacters = value.Substring(value.Length - 2, 2);
                            if (lastCharacters[0] == '\\') {
                                if (lastCharacters[1] != 'n') {
                                    error("Unknown escape sequence. ", _line, position);
                                }
                                value = value.Substring(0, value.Length - 2).ToString() + "\n";
                            }
                        }

                        advance();
                    }
                    tokens.Add(new Token() { Type = TokenType.String, Value = value, Line = _line, Position = position - 1});
                }

                // match string literals
                if (match("'", null, TokenType.Integer, discard:true)) {
                    int value;
                    int position = _position;
                    value = CurrentCharacter.ToCharArray()[0];
                    advance();

                    // deal with empty literals ''
                    if (value == '\'')
                        error("Empty character literal", _line, _position);

                    // deal with escaped characters, only need to worry about \n and \\
                    // throw werror on any other
                    if (value == '\\') {
                        if (CurrentCharacter == "n") {
                            value = '\n';
                        } else if (CurrentCharacter == "\\") {
                            value = '\\';
                        } else {
                            error("Unknown escape sequence. ", _line, _position - 1);
                        }
                        advance();
                    }

                    // if we haven't hit a closing ' here, there are two many characters
                    // in the literal
                    if (!match("'", null, TokenType.Integer, discard: true))
                        error("Multi-character constant", _line, _position);

                    tokens.Add(new Rosetta.Token() { Type = TokenType.Integer, Value = value.ToString(), Line = _line, Position = position - 1 });
                }

                // match comments by checking for starting token, then advancing
                // until closing token is matched
                if (match("/*", null, TokenType.None, exact: true, discard: true)) {
                    while (!match("*/", null, TokenType.None, exact: true, discard: true)) {
                        // reached the end of the file without closing comment!
                        if (CurrentCharacter == "")
                            error("End-of-file in comment. Closing comment characters not found.", _line, _position);
                        advance();
                    }
                    continue;
                }

                // match complex operators
                match("<=", null, TokenType.Op_lessequal, exact: true);
                match(">=", null, TokenType.Op_greaterequal, exact: true);
                match("==", null, TokenType.Op_equal, exact: true);
                match("!=", null, TokenType.Op_notequal, exact: true);
                match("&&", null, TokenType.Op_and, exact: true);
                match("||", null, TokenType.Op_or, exact: true);

                // match simple operators
                if (match(_operators, null, TokenType.None, maxLen:1)) {
                    Token match = tokens.Last();
                    match.Type = _operatorTokenTypeMap[match.Value];
                }

                // brackets, braces and separators
                match("(", null, TokenType.LeftParen, exact: true);
                match(")", null, TokenType.RightParen, exact: true);
                match("{", null, TokenType.LeftBrace, exact: true);
                match("}", null, TokenType.RightBrace, exact: true);
                match(";", null, TokenType.Semicolon, exact: true);
                match(",", null, TokenType.Comma, exact: true);

            }

            // end of file token
            tokens.Add(new Rosetta.Token() { Type = TokenType.End_of_input, Line = _line, Position = _position });

            return tokens;
        }

        static void Main (string[] args) {
            StreamReader inputFile;

            // if we passed in a filename, read code from that, else
            // read code from stdin
            if (args.Length > 0) {
                string path = args[0];
                try {
                    inputFile = new StreamReader(path);
                } catch (IOException) {
                    inputFile = new StreamReader(Console.OpenStandardInput(8192));
                }
            } else {
                inputFile = new StreamReader(Console.OpenStandardInput(8192));
            }

            string code = inputFile.ReadToEnd();

            // strip windows line endings out
            code = code.Replace("\r", "");

            LexicalScanner scanner = new LexicalScanner(code);
            List<Token> tokens = scanner.scan();

            foreach(Token token in tokens) {
                Console.WriteLine(token.ToString());
            }
        }
    }
}

```


{{out|case=test case 3}}
<b>

```txt

5      16      Keyword_print
5      40      Op_subtract
6      16      Keyword_putc
6      40      Op_less
7      16      Keyword_if
7      40      Op_greater
8      16      Keyword_else
8      40      Op_lessequal
9      16      Keyword_while
9      40      Op_greaterequal
10     16      LeftBrace
10     40      Op_equal
11     16      RightBrace
11     40      Op_notequal
12     16      LeftParen
12     40      Op_and
13     16      RightParen
13     40      Op_or
14     16      Op_subtract
14     40      Semicolon
15     16      Op_not
15     40      Comma
16     16      Op_multiply
16     40      Op_assign
17     16      Op_divide
17     40      Integer            42
18     16      Op_mod
18     40      String             "String literal"
19     16      Op_add
19     40      Identifier         variable_name
20     26      Integer            10
21     26      Integer            92
22     26      Integer            32
23     1       End_of_input

```

</b>


## COBOL

Using GnuCOBOL 2. By Steve Williams (with one change to get around a Rosetta Code code highlighter problem).


```cobol>        >
SOURCE FORMAT IS FREE
*> this code is dedicated to the public domain
*> (GnuCOBOL) 2.3-dev.0
identification division.
program-id. lexer.
environment division.
configuration section.
repository. function all intrinsic.
input-output section.
file-control.
    select input-file assign using input-name
        status input-status
        organization line sequential.
data division.

file section.
fd  input-file.
01  input-record pic x(98).

working-storage section.
01  input-name pic x(32).
01  input-status pic xx.
01  input-length pic 99.

01  output-name pic x(32) value spaces.
01  output-status pic xx.
01  output-record pic x(64).

01  line-no pic 999 value 0.
01  col-no pic 99.
01  col-no-max pic 99.
01  col-increment pic 9 value 1.
01  start-col pic 99.
01  outx pic 99.
01  out-lim pic 99 value 48.

01  output-line value spaces.
    03  out-line pic zzzz9.
    03  out-column pic zzzzzz9.
    03  message-area.
        05  filler pic xxx.
        05  token pic x(16).
        05  out-value pic x(48).
        05  out-integer redefines out-value pic zzzzz9.
        05  out-integer1 redefines out-value pic zzzzzz9. *> to match the python lexer

01  error-record.
    03  error-line pic zzzz9 value 0.
    03  error-col pic zzzzzz9 value 0.
    03  error-message pic x(68) value spaces.

01  scan-state pic x(16) value spaces.
01  current-character pic x.
01  previous-character pic x.

procedure division chaining input-name.
start-lexer.
    if input-name <> spaces
        open input input-file
        if input-status = '35'
            string 'in lexer ' trim(input-name) ' not found' into error-message
            perform report-error
        end-if
    end-if
    perform read-input-file
    perform until input-status <> '00'
        add 1 to line-no
        move line-no to out-line
        move length(trim(input-record,trailing)) to col-no-max
        move 1 to col-no
        move space to previous-character
        perform until col-no > col-no-max
            move col-no to out-column
            move input-record(col-no:1) to current-character
            evaluate scan-state

            when 'identifier'
                if current-character >= 'A' and <= 'Z'
                or (current-character >= 'a' and <= 'z')
                or (current-character >= '0' and <= '9')
                or current-character = '_'
                    perform increment-outx
                    move current-character to out-value(outx:1)
                    if col-no = col-no-max
                        perform process-identifier
                    end-if
                else
                    perform process-identifier
                    if current-character <> space
                        move 0 to col-increment
                    end-if
                end-if

            when 'integer'
                evaluate true
                when current-character >= '0' and <= '9'
                    perform increment-outx
                    move current-character to out-value(outx:1)
                    if col-no = col-no-max
                        move numval(out-value) to out-integer
                        move 'Integer' to token
                    end-if
                when current-character >= 'A' and <= 'Z'
                when current-character >= 'a' and <= 'z'
                    move 'in lexer invalid integer' to error-message
                    perform report-error
                when other
                    if outx > 5
                        move numval(out-value) to out-integer1 *> to match the python lexer
                    else
                        move numval(out-value) to out-integer
                    end-if
                    move 'Integer' to token
                    if current-character <> space
                        move 0 to col-increment
                    end-if
                end-evaluate

            when 'comment'
                if previous-character = '*' and current-character = '/'
                    move 'comment' to token
                end-if

            when 'quote'
                evaluate current-character also outx
                when '"' also 0
                    string 'in lexer empty string' into error-message
                    perform report-error
                when '"' also any
                    perform increment-outx
                    move current-character to out-value(outx:1)
                    move 'String' to token
                when other
                    if col-no = col-no-max
                        string 'in lexer missing close quote' into error-message
                        perform report-error
                    else
                        perform increment-outx
                        move current-character to out-value(outx:1)
                    end-if
                end-evaluate

            when 'character'
                evaluate current-character also outx
                when "'" also 0
                    string 'in lexer empty character constant' into error-message
                    perform report-error
                when "'" also 1
                    subtract 1 from ord(out-value(1:1)) giving out-integer
                    move 'Integer' to token
                when "'" also 2
                    evaluate true
                    when out-value(1:2) = '\n'
                        move 10 to out-integer
                    when out-value(1:2) = '\\'
                        subtract 1 from ord('\') giving out-integer      *> ' (workaround a Rosetta Code highlighter problem)
                    when other
                        string 'in lexer unknown escape sequence ' out-value(1:2)
                            into error-message
                        perform report-error
                    end-evaluate
                    move 'Integer' to token
                when "'" also any
                    string 'in lexer multicharacter constant' into error-message
                    perform report-error
                when other
                    if col-no = col-no-max
                        string 'in lexer missing close quote' into error-message
                        perform report-error
                    end-if
                    perform increment-outx
                    move current-character to out-value(outx:1)
                end-evaluate

            when 'and'
                evaluate previous-character also current-character
                when '&' also '&'
                    move 'Op_and' to token
                when other
                    string 'in lexer AND error' into error-message
                    perform report-error
                end-evaluate

            when 'or'
                evaluate previous-character also current-character
                when '|' also '|'
                    move 'Op_or' to token
                when other
                    string 'in lexer OR error' into error-message
                    perform report-error
                end-evaluate

            when 'ambiguous'
                evaluate previous-character also current-character
                when '/' also '*'
                    move 'comment' to scan-state
                    subtract 1 from col-no giving start-col
                when '/' also any
                    move 'Op_divide' to token
                    move 0 to col-increment

                when '=' also '='
                    move 'Op_equal' to token
                when '=' also any
                    move 'Op_assign' to token
                    move 0 to col-increment

                when '<' also '='
                    move 'Op_lessequal' to token
                when '<' also any
                    move 'Op_less' to token
                    move 0 to col-increment

                when '>' also '='
                    move 'Op_greaterequal' to token
                when '>'also any
                    move 'Op_greater' to token
                    move 0 to col-increment

                when '!' also '='
                    move 'Op_notequal' to token
                when '!' also any
                    move 'Op_not' to token
                    move 0 to col-increment

                when other
                    display input-record
                    string 'in lexer ' trim(scan-state)
                        ' unknown character "' current-character '"'
                        ' with previous character "' previous-character '"'
                        into error-message
                    perform report-error
                end-evaluate

            when other
                move col-no to start-col
                evaluate current-character
                when space
                    continue
                when >= 'A' and <= 'Z'
                when >= 'a' and <= 'z'
                    move 'identifier' to scan-state
                    move 1 to outx
                    move current-character to out-value
                when >= '0' and <= '9'
                    move 'integer' to scan-state
                    move 1 to outx
                    move current-character to out-value
                when '&'
                    move 'and' to scan-state
                when '|'
                    move 'or' to scan-state
                when '"'
                    move 'quote' to scan-state
                    move 1 to outx
                    move current-character to out-value
                when "'"
                    move 'character' to scan-state
                    move 0 to outx
                when '{'
                    move 'LeftBrace' to token
                when '}'
                    move 'RightBrace' to token
                when '('
                    move 'LeftParen' to token
                when ')'
                    move 'RightParen' to token
                when '+'
                    move 'Op_add' to token
                when '-'
                    move 'Op_subtract' to token
                when '*'
                    move 'Op_multiply' to token
                when '%'
                    move 'Op_mod' to token
                when ';'
                    move 'Semicolon' to token
                when ','
                    move 'Comma' to token
                when '/'
                when '<'
                when '>'
                when '='
                when '='
                when '<'
                when '>'
                when '!'
                    move 'ambiguous' to scan-state
                when other
                    string 'in lexer unknown character "' current-character '"'
                        into error-message
                    perform report-error
                end-evaluate
            end-evaluate

            if token <> spaces
                perform process-token
            end-if

            move current-character to previous-character
            add col-increment to col-no
            move 1 to col-increment
        end-perform
        if scan-state = 'ambiguous'
            evaluate previous-character
            when '/'
                move 'Op_divide' to token
                perform process-token

            when '='
                move 'Op_assign' to token
                perform process-token

            when '<'
                move 'Op_less' to token
                perform process-token

            when '>'
                move 'Op_greater' to token
                perform process-token

            when '!'
                move 'Op_not' to token
                perform process-token

            when other
                string 'in lexer unresolved ambiguous
                    "' previous-character '" at end of line'
                into error-message
                perform report-error
            end-evaluate
        end-if
        perform read-input-file
    end-perform

    evaluate true
    when input-status <> '10'
        string 'in lexer ' trim(input-name) ' invalid input status ' input-status
            into error-message
        perform report-error
    when scan-state = 'comment'
        string 'in lexer unclosed comment at end of input' into error-message
        perform report-error
     end-evaluate

    move 'End_of_input' to token
    move 1 to out-column
    move 1 to start-col
    add 1 to line-no
    perform process-token

    close input-file
    stop run
    .
process-identifier.
    evaluate true
    when out-value = 'print'
        move 'Keyword_print' to token
        move spaces to out-value
    when out-value = 'while'
        move 'Keyword_while' to token
        move spaces to out-value
    when out-value = 'if'
        move 'Keyword_if' to token
        move spaces to out-value
    when out-value = 'else'
        move 'Keyword_else' to token
        move spaces to out-value
    when out-value = 'putc'
        move 'Keyword_putc' to token
        move spaces to out-value
    when other
        move 'Identifier' to token
    end-evaluate
    .
increment-outx.
    if outx >= out-lim
        string 'in lexer token value length exceeds ' out-lim into error-message
        perform report-error
    end-if
    add 1 to outx
    .
process-token.
    if token <> 'comment'
        move start-col to out-column
        move line-no to out-line
        display output-line
    end-if
    move 0 to start-col
    move spaces to scan-state message-area
    .
report-error.
    move line-no to error-line
    move start-col to error-col
    display error-record
    close input-file
    stop run with error status -1
    .
read-input-file.
    if input-name = spaces
        move '00' to input-status
        accept input-record on exception move '10' to input-status end-accept
    else
        read input-file
    end-if
    .
end program lexer.
```


{{out|case=test case 3}}

```txt
prompt$ ./lexer <testcase3
    5     16   Keyword_print
    5     40   Op_subtract
    6     16   Keyword_putc
    6     40   Op_less
    7     16   Keyword_if
    7     40   Op_greater
    8     16   Keyword_else
    8     40   Op_lessequal
    9     16   Keyword_while
    9     40   Op_greaterequal
   10     16   LeftBrace
   10     40   Op_equal
   11     16   RightBrace
   11     40   Op_notequal
   12     16   LeftParen
   12     40   Op_and
   13     16   RightParen
   13     40   Op_or
   14     16   Op_subtract
   14     40   Semicolon
   15     16   Op_not
   15     40   Comma
   16     16   Op_multiply
   16     40   Op_assign
   17     16   Op_divide
   17     40   Integer             42
   18     16   Op_mod
   18     40   String          "String literal"
   19     16   Op_add
   19     40   Identifier      variable_name
   20     26   Integer             10
   21     26   Integer             92
   22     26   Integer             32
   23      1   End_of_input
```



## Common Lisp

Lisp has a built-in reader and you can customize the reader by modifying its readtable.  I'm also using the Gray stream, which is an almost standard feature of Common Lisp, for counting lines and columns.


```lisp
(defpackage #:lexical-analyzer
  (:use #:cl #:sb-gray)
  (:export #:main))

(in-package #:lexical-analyzer)

(defconstant +lex-symbols-package+ (or (find-package :lex-symbols)
                                       (make-package :lex-symbols)))

(defclass counting-character-input-stream (fundamental-character-input-stream)
  ((stream :type stream :initarg :stream :reader stream-of)
   (line :type fixnum :initform 1 :accessor line-of)
   (column :type fixnum :initform 0 :accessor column-of)
   (prev-column :type (or null fixnum) :initform nil :accessor prev-column-of))
  (:documentation "Character input stream that counts lines and columns."))

(defmethod stream-read-char ((stream counting-character-input-stream))
  (let ((ch (read-char (stream-of stream) nil :eof)))
    (case ch
      (#\Newline
       (incf (line-of stream))
       (setf (prev-column-of stream) (column-of stream)
             (column-of stream) 0))
      (t
       (incf (column-of stream))))
    ch))

(defmethod stream-unread-char ((stream counting-character-input-stream) char)
  (unread-char char (stream-of stream))
  (case char
      (#\Newline
       (decf (line-of stream))
       (setf (column-of stream) (prev-column-of stream)))
      (t
       (decf (column-of stream)))))

(defstruct token
  (name nil :type symbol)
  (value nil :type t)
  (line nil :type fixnum)
  (column nil :type fixnum))

(defun lexer-error (format-control &rest args)
  (apply #'error format-control args))

(defun handle-divide-or-comment (stream char)
  (declare (ignore char))
  (case (peek-char nil stream t nil t)
    (#\* (loop with may-end = nil
                 initially (read-char stream t nil t)
               for ch = (read-char stream t nil t)
               until (and may-end (char= ch #\/))
               do (setf may-end (char= ch #\*))
               finally (return (read stream t nil t))))
    (t (make-token :name :op-divide :line (line-of stream) :column (column-of stream)))))

(defun make-constant-handler (token-name)
  (lambda (stream char)
    (declare (ignore char))
    (make-token :name token-name :line (line-of stream) :column (column-of stream))))

(defun make-this-or-that-handler (expect then &optional else)
  (lambda (stream char)
    (declare (ignore char))
    (let ((line (line-of stream))
          (column (column-of stream))
          (next (peek-char nil stream nil nil t)))
      (cond ((and expect (char= next expect))
             (read-char stream nil nil t)
             (make-token :name then :line line :column column))
            (else
             (make-token :name else :line line :column column))
            (t
             (lexer-error "Unrecognized character '~A'" next))))))

(defun identifier? (symbol)
  (and (symbolp symbol)
       (not (keywordp symbol))
       (let ((name (symbol-name symbol)))
         (and (find (char name 0) "_abcdefghijklmnopqrstuvwxyz" :test #'char-equal)
              (or (< (length name) 2)
                  (not (find-if-not (lambda (ch)
                                      (find ch "_abcdefghijklmnopqrstuvwxyz0123456789"
                                            :test #'char-equal))
                                    name :start 1)))))))

(defun id->keyword (id line column)
  (case id
    (lex-symbols::|if|    (make-token :name :keyword-if :line line :column column))
    (lex-symbols::|else|  (make-token :name :keyword-else :line line :column column))
    (lex-symbols::|while| (make-token :name :keyword-while :line line :column column))
    (lex-symbols::|print| (make-token :name :keyword-print :line line :column column))
    (lex-symbols::|putc|  (make-token :name :keyword-putc :line line :column column))
    (t nil)))

(defun handle-identifier (stream char)
  (let ((*readtable* (copy-readtable)))
    (set-syntax-from-char char #\z)
    (let ((line (line-of stream))
          (column (column-of stream)))
      (unread-char char stream)
      (let ((obj (read stream t nil t)))
        (if (identifier? obj)
            (or (id->keyword obj line column)
                (make-token :name :identifier :value obj :line line :column column))
            (lexer-error "Invalid identifier name: ~A" obj))))))

(defun handle-integer (stream char)
  (let ((*readtable* (copy-readtable)))
    (set-syntax-from-char char #\z)
    (let ((line (line-of stream))
          (column (column-of stream)))
      (unread-char char stream)
      (let ((obj (read stream t nil t)))
        (if (integerp obj)
            (make-token :name :integer :value obj :line line :column column)
            (lexer-error "Invalid integer: ~A" obj))))))

(defun handle-char-literal (stream char)
  (declare (ignore char))
  (let* ((line (line-of stream))
         (column (column-of stream))
         (ch (read-char stream t nil t))
         (parsed (case ch
                   (#\' (lexer-error "Empty character constant"))
                   (#\Newline (lexer-error "New line in character literal"))
                   (#\\ (let ((next-ch (read-char stream t nil t)))
                          (case next-ch
                            (#\n #\Newline)
                            (#\\ #\\)
                            (t (lexer-error "Unknown escape sequence: \\~A" next-ch)))))
                   (t ch))))
    (if (char= #\' (read-char stream t nil t))
        (make-token :name :integer :value (char-code parsed) :line line :column column)
        (lexer-error "Only one character is allowed in character literal"))))

(defun handle-string (stream char)
  (declare (ignore char))
  (loop with result = (make-array 0 :element-type 'character :adjustable t :fill-pointer t)
        with line = (line-of stream)
        with column = (column-of stream)
        for ch = (read-char stream t nil t)
        until (char= ch #\")
        do (setf ch (case ch
                      (#\Newline (lexer-error "New line in string"))
                      (#\\ (let ((next-ch (read-char stream t nil t)))
                             (case next-ch
                               (#\n #\Newline)
                               (#\\ #\\)
                               (t (lexer-error "Unknown escape sequence: \\~A" next-ch)))))
                      (t ch)))
           (vector-push-extend ch result)
        finally (return (make-token :name :string :value result :line line :column column))))

(defun make-lexer-readtable ()
  (let ((*readtable* (copy-readtable nil)))
    (setf (readtable-case *readtable*) :preserve)
    (set-syntax-from-char #\\ #\z)
    (set-syntax-from-char #\# #\z)
    (set-syntax-from-char #\` #\z)

    ;; operators
    (set-macro-character #\* (make-constant-handler :op-multiply))
    (set-macro-character #\/ #'handle-divide-or-comment)
    (set-macro-character #\% (make-constant-handler :op-mod))
    (set-macro-character #\+ (make-constant-handler :op-add))
    (set-macro-character #\- (make-constant-handler :op-subtract))
    (set-macro-character #\< (make-this-or-that-handler #\= :op-lessequal :op-less))
    (set-macro-character #\> (make-this-or-that-handler #\= :op-greaterequal :op-greater))
    (set-macro-character #\= (make-this-or-that-handler #\= :op-equal :op-assign))
    (set-macro-character #\! (make-this-or-that-handler #\= :op-notequal :op-not))
    (set-macro-character #\& (make-this-or-that-handler #\& :op-and))
    (set-macro-character #\| (make-this-or-that-handler #\| :op-or))

    ;; symbols
    (set-macro-character #\( (make-constant-handler :leftparen))
    (set-macro-character #\) (make-constant-handler :rightparen))
    (set-macro-character #\{ (make-constant-handler :leftbrace))
    (set-macro-character #\} (make-constant-handler :rightbrace))
    (set-macro-character #\; (make-constant-handler :semicolon))
    (set-macro-character #\, (make-constant-handler :comma))

    ;; identifiers & keywords
    (set-macro-character #\_ #'handle-identifier t)
    (loop for ch across "abcdefghijklmnopqrstuvwxyz"
          do (set-macro-character ch #'handle-identifier t))
    (loop for ch across "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
          do (set-macro-character ch #'handle-identifier t))

    ;; integers
    (loop for ch across "0123456789"
          do (set-macro-character ch #'handle-integer t))
    (set-macro-character #\' #'handle-char-literal)

    ;; strings
    (set-macro-character #\" #'handle-string)

    *readtable*))

(defun lex (stream)
  (loop with *readtable* = (make-lexer-readtable)
        with *package* = +lex-symbols-package+
        with eof = (gensym)
        with counting-stream = (make-instance 'counting-character-input-stream :stream stream)
        for token = (read counting-stream nil eof)
        until (eq token eof)
        do (format t "~5D ~5D ~15A~@[ ~S~]~%"
                   (token-line token) (token-column token) (token-name token) (token-value token))
        finally (format t "~5D ~5D ~15A~%"
                        (line-of counting-stream) (column-of counting-stream) :end-of-input)
                (close counting-stream)))

(defun main ()
  (lex *standard-input*))
```

{{out|case=test case 3}}

```txt
    5    16 KEYWORD-PRINT
    5    40 OP-SUBTRACT
    6    16 KEYWORD-PUTC
    6    40 OP-LESS
    7    16 KEYWORD-IF
    7    40 OP-GREATER
    8    16 KEYWORD-ELSE
    8    40 OP-LESSEQUAL
    9    16 KEYWORD-WHILE
    9    40 OP-GREATEREQUAL
   10    16 LEFTBRACE
   10    40 OP-EQUAL
   11    16 RIGHTBRACE
   11    40 OP-NOTEQUAL
   12    16 LEFTPAREN
   12    40 OP-AND
   13    16 RIGHTPAREN
   13    40 OP-OR
   14    16 OP-SUBTRACT
   14    40 SEMICOLON
   15    16 OP-NOT
   15    40 COMMA
   16    16 OP-MULTIPLY
   16    40 OP-ASSIGN
   17    16 OP-DIVIDE
   17    40 INTEGER         42
   18    16 OP-MOD
   18    40 STRING          "String literal"
   19    16 OP-ADD
   19    40 IDENTIFIER      variable_name
   20    26 INTEGER         10
   21    26 INTEGER         92
   22    26 INTEGER         32
   23     1 END-OF-INPUT
```



## Euphoria

Tested with Euphoria 4.05.

```euphoria
include std/io.e
include std/map.e
include std/types.e
include std/convert.e

constant true = 1, false = 0, EOF = -1

enum tk_EOI, tk_Mul, tk_Div, tk_Mod, tk_Add, tk_Sub, tk_Negate, tk_Not, tk_Lss, tk_Leq,
    tk_Gtr, tk_Geq, tk_Eq, tk_Neq, tk_Assign, tk_And, tk_Or, tk_If, tk_Else, tk_While,
    tk_Print, tk_Putc, tk_Lparen, tk_Rparen, tk_Lbrace, tk_Rbrace, tk_Semi, tk_Comma,
    tk_Ident, tk_Integer, tk_String

constant all_syms = {"End_of_input", "Op_multiply", "Op_divide", "Op_mod", "Op_add",
    "Op_subtract", "Op_negate", "Op_not", "Op_less", "Op_lessequal", "Op_greater",
    "Op_greaterequal", "Op_equal", "Op_notequal", "Op_assign", "Op_and", "Op_or",
    "Keyword_if", "Keyword_else", "Keyword_while", "Keyword_print", "Keyword_putc",
    "LeftParen", "RightParen", "LeftBrace", "RightBrace", "Semicolon", "Comma",
    "Identifier", "Integer", "String"}

integer input_file, the_ch = ' ', the_col = 0, the_line = 1
sequence symbols
map key_words = new()

procedure error(sequence format, sequence data)
    printf(STDOUT, format, data)
    abort(1)
end procedure

-- get the next character from the input
function next_ch()
    the_ch = getc(input_file)
    the_col += 1
    if the_ch = '\n' then
        the_line += 1
        the_col = 0
    end if
    return the_ch
end function

-- 'x' - character constants
function char_lit(integer err_line, integer err_col)
    integer n = next_ch()              -- skip opening quote
    if the_ch = '\'' then
        error("%d %d empty character constant", {err_line, err_col})
    elsif the_ch = '\\' then
        next_ch()
        if the_ch = 'n' then
            n = 10
        elsif the_ch = '\\' then
            n = '\\'
        else
            error("%d %d unknown escape sequence \\%c", {err_line, err_col, the_ch})
        end if
    end if
    if next_ch() != '\'' then
        error("%d %d multi-character constant", {err_line, err_col})
    end if
    next_ch()
    return {tk_Integer, err_line, err_col, n}
end function

-- process divide or comments
function div_or_cmt(integer err_line, integer err_col)
    if next_ch() != '*' then
        return {tk_Div, err_line, err_col}
    end if

    -- comment found
    next_ch()
    while true do
        if the_ch = '*' then
            if next_ch() = '/' then
                next_ch()
                return get_tok()
            end if
        elsif the_ch = EOF then
            error("%d %d EOF in comment", {err_line, err_col})
        else
            next_ch()
        end if
    end while
end function

-- "string"
function string_lit(integer start, integer err_line, integer err_col)
    string text = ""

    while next_ch() != start do
        if the_ch = EOF then
            error("%d %d EOF while scanning string literal", {err_line, err_col})
        end if
        if the_ch = '\n' then
            error("%d %d EOL while scanning string literal", {err_line, err_col})
        end if
        text &= the_ch
    end while

    next_ch()
    return {tk_String, err_line, err_col, text}
end function

-- handle identifiers and integers
function ident_or_int(integer err_line, integer err_col)
    integer n, is_number = true
    string text = ""

    while t_alnum(the_ch) or the_ch = '_' do
        text &= the_ch
        if not t_digit(the_ch) then
            is_number = false
        end if
        next_ch()
    end while

    if length(text) = 0 then
        error("%d %d ident_or_int: unrecognized character: (%d) '%s'", {err_line, err_col, the_ch, the_ch})
    end if

    if t_digit(text[1]) then
        if not is_number then
            error("%d %d invalid number: %s", {err_line, err_col, text})
        end if
        n = to_integer(text)
        return {tk_Integer, err_line, err_col, n}
    end if

    if has(key_words, text) then
        return {get(key_words, text), err_line, err_col}
    end if

    return {tk_Ident, err_line, err_col, text}
end function

-- look ahead for '>=', etc.
function follow(integer expect, integer ifyes, integer ifno, integer err_line, integer err_col)
    if next_ch() = expect then
        next_ch()
        return {ifyes, err_line, err_col}
    end if

    if ifno = tk_EOI then
        error("%d %d follow: unrecognized character: (%d)", {err_line, err_col, the_ch})
    end if

    return {ifno, err_line, err_col}
end function

-- return the next token type
function get_tok()
    while t_space(the_ch) do
        next_ch()
    end while

    integer err_line = the_line
    integer err_col  = the_col

    switch the_ch do
        case EOF  then return {tk_EOI, err_line, err_col}
        case '/'  then return div_or_cmt(err_line, err_col)
        case '\'' then return char_lit(err_line, err_col)

        case '<'  then return follow('=', tk_Leq, tk_Lss,    err_line, err_col)
        case '>'  then return follow('=', tk_Geq, tk_Gtr,    err_line, err_col)
        case '='  then return follow('=', tk_Eq,  tk_Assign, err_line, err_col)
        case '!'  then return follow('=', tk_Neq, tk_Not,    err_line, err_col)
        case '&'  then return follow('&', tk_And, tk_EOI,    err_line, err_col)
        case '|'  then return follow('|', tk_Or,  tk_EOI,    err_line, err_col)

        case '"'  then return string_lit(the_ch, err_line, err_col)
        case else
            integer sym = symbols[the_ch]
            if sym  != tk_EOI then
                next_ch()
                return {sym, err_line, err_col}
            end if
            return ident_or_int(err_line, err_col)
    end switch
end function

procedure init()
    put(key_words, "else",    tk_Else)
    put(key_words, "if",      tk_If)
    put(key_words, "print",   tk_Print)
    put(key_words, "putc",    tk_Putc)
    put(key_words, "while",   tk_While)

    symbols = repeat(tk_EOI, 256)
    symbols['{'] = tk_Lbrace
    symbols['}'] = tk_Rbrace
    symbols['('] = tk_Lparen
    symbols[')'] = tk_Rparen
    symbols['+'] = tk_Add
    symbols['-'] = tk_Sub
    symbols['*'] = tk_Mul
    symbols['%'] = tk_Mod
    symbols[';'] = tk_Semi
    symbols[','] = tk_Comma
end procedure

procedure main(sequence cl)
    sequence file_name

    input_file = STDIN
    if length(cl) > 2 then
        file_name = cl[3]
        input_file = open(file_name, "r")
        if input_file = -1 then
            error("Could not open %s", {file_name})
        end if
    end if
    init()
    sequence t
    loop do
        t = get_tok()
        printf(STDOUT, "%5d  %5d %-8s", {t[2], t[3], all_syms[t[1]]})
        switch t[1] do
            case tk_Integer then printf(STDOUT, "  %5d\n",   {t[4]})
            case tk_Ident   then printf(STDOUT, " %s\n",     {t[4]})
            case tk_String  then printf(STDOUT, " \"%s\"\n", {t[4]})
            case else            printf(STDOUT, "\n")
        end switch
        until t[1] = tk_EOI
    end loop
end procedure

main(command_line())
```


{{out|case=test case 3}}
<b>

```txt

    5     16 Keyword_print
    5     40 Op_subtract
    6     16 Keyword_putc
    6     40 Op_less
    7     16 Keyword_if
    7     40 Op_greater
    8     16 Keyword_else
    8     40 Op_lessequal
    9     16 Keyword_while
    9     40 Op_greaterequal
   10     16 LeftBrace
   10     40 Op_equal
   11     16 RightBrace
   11     40 Op_notequal
   12     16 LeftParen
   12     40 Op_and
   13     16 RightParen
   13     40 Op_or
   14     16 Op_subtract
   14     40 Semicolon
   15     16 Op_not
   15     40 Comma
   16     16 Op_multiply
   16     40 Op_assign
   17     16 Op_divide
   17     40 Integer      42
   18     16 Op_mod
   18     40 String   "String literal"
   19     16 Op_add
   19     40 Identifier variable_name
   20     26 Integer      10
   21     26 Integer      92
   22     26 Integer      32
   23      1 End_of_input

```

</b>


## Flex

Tested with Flex 2.5.4.

```C
%{
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <limits.h>

#define NELEMS(arr) (sizeof(arr) / sizeof(arr[0]))

typedef enum {
    tk_EOI, tk_Mul, tk_Div, tk_Mod, tk_Add, tk_Sub, tk_Negate, tk_Not, tk_Lss, tk_Leq,
    tk_Gtr, tk_Geq, tk_Eq, tk_Neq, tk_Assign, tk_And, tk_Or, tk_If, tk_Else, tk_While,
    tk_Print, tk_Putc, tk_Lparen, tk_Rparen, tk_Lbrace, tk_Rbrace, tk_Semi, tk_Comma,
    tk_Ident, tk_Integer, tk_String
} TokenType;

void yyerror(char msg[]) {
    printf(msg);
    exit(1);
}

static int yynval;

struct yylloc {
    int first_line, first_col;
    int last_line, last_col;
} yylloc;

static void update_loc() {
  static int curr_line = 1;
  static int curr_col  = 1;

  yylloc.first_line = curr_line;
  yylloc.first_col  = curr_col;

  {char *s; for (s = yytext; *s != '\0'; s++) {
    if (*s == '\n') {
      curr_line++;
      curr_col = 1;
    } else {
      curr_col++;
    }
  }}

  yylloc.last_line = curr_line;
  yylloc.last_col  = curr_col-1;
}

#define YY_USER_ACTION update_loc();

static int kwd_cmp(const void *p1, const void *p2) {
    return strcmp(*(char **)p1, *(char **)p2);
}

static TokenType get_ident_type(const char *ident) {
    static struct {
        char *s;
        TokenType sym;
    } kwds[] = {
        {"else",  tk_Else},
        {"if",    tk_If},
        {"print", tk_Print},
        {"putc",  tk_Putc},
        {"while", tk_While},
    }, *kwp;

    return (kwp = bsearch(&ident, kwds, NELEMS(kwds), sizeof(kwds[0]), kwd_cmp)) == NULL ? tk_Ident : kwp->sym;
}

%}

%start COMMENT2

%option noyywrap

digit       [0-9]
ident       [a-zA-Z_][a-zA-Z_0-9]*

number       {digit}+
string       \"[^"\n]*\"
char_const   \'([^'\n]|\\n|\\\\)\'

%%

<COMMENT2>[^*]+  ;
<COMMENT2>\*[^/] ;
<COMMENT2>\*\/	 BEGIN 0;		/* end comment */
"/*"		     BEGIN COMMENT2;

"{"      {return tk_Lbrace;}
"}"      {return tk_Rbrace;}
"("      {return tk_Lparen;}
")"      {return tk_Rparen;}
"*"      {return tk_Mul;}
"/"      {return tk_Div;}
"%"      {return tk_Mod;}
"+"      {return tk_Add;}
"-"      {return tk_Sub;}
"<"      {return tk_Lss;}
">"      {return tk_Gtr;}
"<="     {return tk_Leq;}
">="     {return tk_Geq;}
"!="     {return tk_Neq;}
"!"      {return tk_Not;}
"&&"     {return tk_And;}
"||"     {return tk_Or;}
";"      {return tk_Semi;}
","      {return tk_Comma;}
"=="     {return tk_Eq;}
"="      {return tk_Assign;}
{ident}  {return get_ident_type(yytext);}
{string} {return tk_String;}

[ \t\n]+ ; /* ignore whitespace */

{number}     {
                yynval = strtol(yytext, NULL, 0);
                if (yynval == LONG_MAX && errno == ERANGE)
                    yyerror("Number exceeds maximum value");

                return tk_Integer;
             }

{char_const} {
                int n = yytext[1];
                char *p = yytext;

                if (yyleng < 3)
                    yyerror("empty character constant");
                ++p;
                if (p[0] == '\\') {
                    ++p;
                    if (p[0] == 'n')
                        n = 10;
                    else if (p[0] == '\\')
                        n = '\\';
                    else
                        yyerror("unknown escape sequence");
                }
                yynval = n;
                return tk_Integer;
             }

.            yyerror("Unknown character\n");

%%

int main(int argc, char *argv[]) {
    int tok;

    ++argv, --argc;  /* skip over program name */
    yyin = stdin;
    if (argc > 0)
        yyin = fopen(argv[0], "r");

    do {
        tok = yylex();
        printf("%5d  %5d %.15s", yylloc.first_line, yylloc.first_col,
            &"End_of_input    Op_multiply     Op_divide       Op_mod          Op_add          "
             "Op_subtract     Op_negate       Op_not          Op_less         Op_lessequal    "
             "Op_greater      Op_greaterequal Op_equal        Op_notequal     Op_assign       "
             "Op_and          Op_or           Keyword_if      Keyword_else    Keyword_while   "
             "Keyword_print   Keyword_putc    LeftParen       RightParen      LeftBrace       "
             "RightBrace      Semicolon       Comma           Identifier      Integer         "
             "String          "
            [tok * 16]);

        if (tok == tk_Integer)     printf("   %5d", yynval);
        else if (tok == tk_Ident)  printf("  %s",   yytext);
        else if (tok == tk_String) printf("  %s",   yytext);
        printf("\n");
    } while (tok != tk_EOI);
    return 0;
}
```


{{out|case=test case 3}}
<b>

```txt

    5     16 Keyword_print
    5     40 Op_subtract
    6     16 Keyword_putc
    6     40 Op_less
    7     16 Keyword_if
    7     40 Op_greater
    8     16 Keyword_else
    8     40 Op_lessequal
    9     16 Keyword_while
    9     40 Op_greaterequal
   10     16 LeftBrace
   10     40 Op_equal
   11     16 RightBrace
   11     40 Op_notequal
   12     16 LeftParen
   12     40 Op_and
   13     16 RightParen
   13     40 Op_or
   14     16 Op_subtract
   14     40 Semicolon
   15     16 Op_not
   15     40 Comma
   16     16 Op_multiply
   16     40 Op_assign
   17     16 Op_divide
   17     40 Integer              42
   18     16 Op_mod
   18     40 String           "String literal"
   19     16 Op_add
   19     40 Identifier       variable_name
   20     26 Integer              10
   21     26 Integer              92
   22     26 Integer              32
   22     29 End_of_input

```

</b>


## Forth

Tested with Gforth 0.7.3.

```Forth
CREATE BUF 0 ,              \ single-character look-ahead buffer
CREATE COLUMN# 0 ,
CREATE LINE# 1 ,

: NEWLINE? ( c -- t|f)  DUP 10 = SWAP  13 =  OR ;
: +IN ( c --)
   1 SWAP  NEWLINE?
   IF 0 COLUMN# ! LINE# ELSE COLUMN# THEN
   +!  0 BUF ! ;
: PEEK   BUF @ 0= IF STDIN KEY-FILE BUF ! THEN BUF @ ;
: GETC   PEEK  DUP +IN ;
: SKIP   GETC DROP ;
: .LOCATION   7 .R  4 .R SPACE ;
: WHERE   COLUMN# @ LINE# @ ;
: .WHERE    WHERE .LOCATION ;
: .WHERE+   WHERE  SWAP 1+ SWAP .LOCATION ;

: EXPECT   GETC  OVER OVER =
   IF 2DROP
   ELSE CR ." stdin:" COLUMN# @ 0 LINE# @ 0
      <# #s #> TYPE ." :" <# #s #> TYPE ." : "
      ." unexpected `" EMIT ." ', expecting `" EMIT ." '" CR
      BYE
   THEN ;
: EQ   PEEK [CHAR] = = IF SKIP 2SWAP THEN
       ." Op_" TYPE CR  2DROP ;

CREATE ESC  4 C, CHAR $ C, CHAR $ C, CHAR \ C, 0 C,
: ?ESC?   CR ." Unknown escape sequence `\" EMIT ." '" CR BYE ;
: >ESC   ESC 4 + C!  ESC ;
: $$\n   10 ;
: $$\\   [CHAR] \ ;
: ESCAPE   DUP >ESC FIND IF NIP EXECUTE ELSE DROP ?ESC? THEN ;
: ?ESCAPE   DUP [CHAR] \ = IF DROP GETC ESCAPE THEN ;
: ?EOF   DUP 4 = IF CR ." End-of-file in string" CR BYE THEN ;
: ?EOL   DUP NEWLINE?
         IF CR ." End-of-line in string" CR BYE THEN ;
: STRING   PAD
   BEGIN  GETC ?EOF ?EOL DUP  [CHAR] " <>
   WHILE  OVER C! CHAR+
   REPEAT DROP  PAD TUCK - ;
: "TYPE"   [CHAR] " EMIT  TYPE  [CHAR] " EMIT ;

CREATE TOKEN  4 C, CHAR $ C, CHAR $ C, 0 C, 0 C,
: >HEX   DUP 9 > IF 7 + THEN [CHAR] 0 + ;
: HI!   $F0 AND  2/ 2/ 2/ 2/ >HEX  TOKEN 3 + C! ;
: LO!   $0F AND  >HEX TOKEN 4 + C! ;
: >TOKEN   DUP HI! LO!  TOKEN ;

: ?EOF   DUP 4 = IF CR ." End-of-file in comment" CR BYE THEN ;
: $$2F   PEEK [CHAR] * =
   IF SKIP
       BEGIN
   	GETC ?EOF  [CHAR] * =
   	PEEK [CHAR] / =  AND
       UNTIL  SKIP
   ELSE  .WHERE ." Op_divide" CR THEN ;
: $$22   .WHERE ." String " STRING "TYPE" CR ;
: $$27   .WHERE GETC ?ESCAPE ." Integer " . [CHAR] ' EXPECT CR ;
: $$04   .WHERE ." End_of_input" CR BYE ;
: $$2D   .WHERE ." Op_subtract" CR ;
: $$2B   .WHERE ." Op_add" CR ;
: $$25   .WHERE ." Op_mod" CR ;
: $$2A   .WHERE ." Op_multiply" CR ;
: $$7B   .WHERE ." LeftBrace" CR ;
: $$7D   .WHERE ." RightBrace" CR ;
: $$2C   .WHERE ." Comma" CR ;
: $$29   .WHERE ." RightParen" CR ;
: $$28   .WHERE ." LeftParen" CR ;
: $$3B   .WHERE ." Semicolon" CR ;
: $$3D   .WHERE s" equal" s" assign" EQ ;
: $$21   .WHERE s" notequal" s" not" EQ ;
: $$3C   .WHERE s" lessequal" s" less" EQ ;
: $$3E   .WHERE s" greaterequal" s" greater" EQ ;
: $$26   .WHERE [CHAR] & EXPECT  ." Op_and" CR ;
: $$7C   .WHERE [CHAR] | EXPECT  ." Op_or" CR ;
: $$20   ;   \ space

CREATE KEYWORD  0 C, CHAR $ C, CHAR $ C, 5 CHARS ALLOT
: >KEYWORD   DUP  2 + KEYWORD C!
             KEYWORD 3 + SWAP CMOVE  KEYWORD ;
: FIND-KW   DUP 5 <=
   IF 2DUP >KEYWORD FIND
      IF TRUE 2SWAP 2DROP ELSE DROP FALSE THEN
   ELSE FALSE THEN ;

: $$if   ." Keyword_if" ;
: $$else   ." Keyword_else" ;
: $$while   ." Keyword_while" ;
: $$print   ." Keyword_print" ;
: $$putc   ." Keyword_putc" ;

: DIGIT?   48 58 WITHIN ;
: ALPHA?   DUP  95 = SWAP		  \ underscore?
           DUP 97 123 WITHIN SWAP	  \ lower?
           65 91 WITHIN  OR OR ;	  \ upper?
: ALNUM?   DUP DIGIT? SWAP  ALPHA? OR ;
: INTEGER   0
   BEGIN  PEEK DIGIT?
   WHILE  GETC [CHAR] 0 -  SWAP 10 * +
   REPEAT ;
: ?INTEGER?   CR ." Invalid number" CR BYE ;
: ?INTEGER   PEEK ALPHA? IF ?INTEGER? THEN ;
: DIGIT   .WHERE+ ." Integer " INTEGER ?INTEGER . CR ;
: NAME   PAD
         BEGIN  PEEK ALNUM?
	 WHILE GETC OVER C! CHAR+
	 REPEAT  PAD TUCK - ;
: IDENT   ." Identifier " TYPE ;
: ALPHA   .WHERE+ NAME FIND-KW
          IF EXECUTE ELSE IDENT THEN CR ;
: ?CHAR?   CR ." Character '" EMIT ." ' not recognized" CR BYE ;
: SPACE?   DUP BL = SWAP  9 14 WITHIN  OR ;
: SKIP-SPACE   BEGIN PEEK SPACE? WHILE SKIP REPEAT ;
: CONSUME
   SKIP-SPACE
   PEEK DIGIT? IF DIGIT ELSE
    PEEK ALPHA? IF ALPHA ELSE
     PEEK >TOKEN FIND
     IF SKIP EXECUTE ELSE GETC ?CHAR? BYE THEN
   THEN THEN ;
: TOKENIZE   BEGIN CONSUME AGAIN ;
TOKENIZE
```


{{out}}
Tested against all programs in [[Compiler/Sample programs]].


## FreeBASIC

Tested with FreeBASIC 1.05

```FreeBASIC
enum Token_type
    tk_EOI
    tk_Mul
    tk_Div
    tk_Mod
    tk_Add
    tk_Sub
    tk_Negate
    tk_Not
    tk_Lss
    tk_Leq
    tk_Gtr
    tk_Geq
    tk_Eq
    tk_Neq
    tk_Assign
    tk_And
    tk_Or
    tk_If
    tk_Else
    tk_While
    tk_Print
    tk_Putc
    tk_Lparen
    tk_Rparen
    tk_Lbrace
    tk_Rbrace
    tk_Semi
    tk_Comma
    tk_Ident
    tk_Integer
    tk_String
end enum

const NewLine     = chr(10)
const DoubleQuote = chr(34)
const BackSlash   = chr(92)

' where we store keywords and variables
type Symbol
    s_name as string
    tok as Token_type
end type

dim shared symtab() as Symbol

dim shared cur_line as string
dim shared cur_ch as string
dim shared line_num as integer
dim shared col_num as integer

function is_digit(byval ch as string) as long
    is_digit = ch >= "0" AndAlso ch <= "9"
end function

function is_alnum(byval ch as string) as long
    is_alnum = (ucase(ch) >= "A" AndAlso ucase(ch) <= "Z") OrElse is_digit(ch)
end function

sub error_msg(byval eline as integer, byval ecol as integer, byval msg as string)
    print "("; eline; ":"; ecol; ") "; msg
    print : print "Hit any to end program"
    sleep
    system
end sub

' add an identifier to the symbol table
function install(byval s_name as string, byval tok as Token_type) as integer
    dim n as integer = ubound(symtab) + 1
    redim preserve symtab(n)

    symtab(n).s_name = s_name
    symtab(n).tok    = tok
    return n
end function

' search for an identifier in the symbol table
function lookup(byval s_name as string) as integer
    dim i as integer

    for i = lbound(symtab) to ubound(symtab)
        if symtab(i).s_name = s_name then return i
    next
    return -1
end function

sub next_line()         ' read the next line of input from the source file
    cur_line = ""
    cur_ch  = ""        ' empty cur_ch means end-of-file
    if eof(1) then exit sub
    line input #1, cur_line
    cur_line = cur_line + NewLine
    line_num += + 1
    col_num = 1
end sub

sub next_char()         ' get the next char
    cur_ch = ""
    col_num += 1
    if col_num > len(cur_line) then next_line()
    if col_num <= len(cur_line) then cur_ch = mid(cur_line, col_num, 1)
end sub

function follow(byval err_line as integer, byval err_col as integer, byval expect as string, byval ifyes as Token_type, byval ifno as Token_type) as Token_type
    if cur_ch = expect then
        next_char()
        return ifyes
    end if
    if ifno = tk_eoi then error_msg(err_line, err_col, "follow unrecognized character: " + cur_ch)
    return ifno
end function

sub gettok(byref err_line as integer, byref err_col as integer, byref tok as Token_type, byref v as string)
    ' skip whitespace
    do while (cur_ch = " " or cur_ch = chr(9) or cur_ch = NewLine) and (cur_ch <> "")
        next_char()
    loop

    err_line = line_num
    err_col  = col_num

    select case cur_ch
        case "":  tok = tk_eoi: exit sub
        case "{": tok = tk_lbrace: next_char(): exit sub
        case "}": tok = tk_rbrace: next_char(): exit sub
        case "(": tok = tk_lparen: next_char(): exit sub
        case ")": tok = tk_rparen: next_char(): exit sub
        case "+": tok = tk_add:    next_char(): exit sub
        case "-": tok = tk_sub:    next_char(): exit sub
        case "*": tok = tk_mul:    next_char(): exit sub
        case "%": tok = tk_Mod:    next_char(): exit sub
        case ";": tok = tk_semi:   next_char(): exit sub
        case ",": tok = tk_comma:  next_char(): exit sub
        case "/": ' div or comment
            next_char()
            if cur_ch <> "*" then
                tok = tk_div
                exit sub
            end if
            ' skip comments
            next_char()
            do
                if cur_ch = "*" then
                    next_char()
                    if cur_ch = "/" then
                        next_char()
                        gettok(err_line, err_col, tok, v)
                        exit sub
                    end if
                elseif cur_ch = "" then error_msg(err_line, err_col, "EOF in comment")
                else
                    next_char()
                end if
            loop
        case "'":   ' single char literals
            next_char()
            v = str(asc(cur_ch))
            if cur_ch = "'" then error_msg(err_line, err_col, "empty character constant")
            if cur_ch = BackSlash then
                next_char()
                if cur_ch = "n" then
                    v = "10"
                elseif cur_ch = BackSlash then
                    v = "92"
                else error_msg(err_line, err_col, "unknown escape sequence: " + cur_ch)
                end if
            end if
            next_char()
            if cur_ch <> "'" then error_msg(err_line, err_col, "multi-character constant")
            next_char()
            tok = tk_integer
            exit sub
        case "<": next_char(): tok = follow(err_line, err_col, "=", tk_Leq, tk_Lss): exit sub
        case ">": next_char(): tok = follow(err_line, err_col, "=", tk_Geq, tk_Gtr): exit sub
        case "!": next_char(): tok = follow(err_line, err_col, "=", tk_Neq, tk_Not): exit sub
        case "=": next_char(): tok = follow(err_line, err_col, "=", tk_Eq,  tk_Assign): exit sub
        case "&": next_char(): tok = follow(err_line, err_col, "&", tk_And, tk_EOI): exit sub
        case "|": next_char(): tok = follow(err_line, err_col, "|", tk_Or,  tk_EOI): exit sub
        case DoubleQuote: ' string
            v = cur_ch
            next_char()
            do while cur_ch <> DoubleQuote
                if cur_ch = NewLine then error_msg(err_line, err_col, "EOL in string")
                if cur_ch = "" then error_msg(err_line, err_col, "EOF in string")
                v += cur_ch
                next_char()
            loop
            v += cur_ch
            next_char()
            tok = tk_string
            exit sub
        case else   ' integers or identifiers
            dim is_number as boolean = is_digit(cur_ch)
            v = ""
            do while is_alnum(cur_ch) orelse cur_ch = "_"
                if not is_digit(cur_ch) then is_number = false
                v += cur_ch
                next_char()
            loop
            if len(v) = 0 then error_msg(err_line, err_col, "unknown character: " + cur_ch)
            if is_digit(mid(v, 1, 1)) then
                if not is_number then error_msg(err_line, err_col, "invalid number: " + v)
                tok = tk_integer
                exit sub
            end if
            dim as integer index = lookup(v)
            if index = -1 then
                tok = tk_ident
            else
                tok = symtab(index).tok
            end if
            exit sub
    end select
end sub

sub init_lex(byval filein as string)
    install("else",  tk_else)
    install("if",    tk_if)
    install("print", tk_print)
    install("putc",  tk_putc)
    install("while", tk_while)

    open filein for input as #1

    cur_line = ""
    line_num = 0
    col_num = 0
    next_char()
end sub

sub scanner()
    dim err_line as integer
    dim err_col as integer
    dim tok as Token_type
    dim v as string
    dim tok_list(tk_eoi to tk_string) as string

    tok_list(tk_EOI    ) = "End_of_input"
    tok_list(tk_Mul    ) = "Op_multiply"
    tok_list(tk_Div    ) = "Op_divide"
    tok_list(tk_Mod    ) = "Op_mod"
    tok_list(tk_Add    ) = "Op_add"
    tok_list(tk_Sub    ) = "Op_subtract"
    tok_list(tk_Negate ) = "Op_negate"
    tok_list(tk_Not    ) = "Op_not"
    tok_list(tk_Lss    ) = "Op_less"
    tok_list(tk_Leq    ) = "Op_lessequal"
    tok_list(tk_Gtr    ) = "Op_greater"
    tok_list(tk_Geq    ) = "Op_greaterequal"
    tok_list(tk_Eq     ) = "Op_equal"
    tok_list(tk_Neq    ) = "Op_notequal"
    tok_list(tk_Assign ) = "Op_assign"
    tok_list(tk_And    ) = "Op_and"
    tok_list(tk_Or     ) = "Op_or"
    tok_list(tk_If     ) = "Keyword_if"
    tok_list(tk_Else   ) = "Keyword_else"
    tok_list(tk_While  ) = "Keyword_while"
    tok_list(tk_Print  ) = "Keyword_print"
    tok_list(tk_Putc   ) = "Keyword_putc"
    tok_list(tk_Lparen ) = "LeftParen"
    tok_list(tk_Rparen ) = "RightParen"
    tok_list(tk_Lbrace ) = "LeftBrace"
    tok_list(tk_Rbrace ) = "RightBrace"
    tok_list(tk_Semi   ) = "Semicolon"
    tok_list(tk_Comma  ) = "Comma"
    tok_list(tk_Ident  ) = "Identifier"
    tok_list(tk_Integer) = "Integer"
    tok_list(tk_String ) = "String"

    do
        gettok(err_line, err_col, tok, v)
        print using "#####  ##### \               " + BackSlash; err_line; err_col; tok_list(tok);
        if tok = tk_integer orelse tok = tk_ident orelse tok = tk_string then print " " + v;
        print
    loop until tok = tk_eoi
end sub

sub main()
    if command(1) = "" then print "filename required" : exit sub
    init_lex(command(1))
    scanner()
end sub

main()
print : print "Hit any to end program"
sleep
system
```

{{out|case=test case 3}}
<b>

```txt
    5     16 Keyword_print
    5     40 Op_subtract
    6     16 Keyword_putc
    6     40 Op_less
    7     16 Keyword_if
    7     40 Op_greater
    8     16 Keyword_else
    8     40 Op_lessequal
    9     16 Keyword_while
    9     40 Op_greaterequal
   10     16 LeftBrace
   10     40 Op_equal
   11     16 RightBrace
   11     40 Op_notequal
   12     16 LeftParen
   12     40 Op_and
   13     16 RightParen
   13     40 Op_or
   14     16 Op_subtract
   14     40 Semicolon
   15     16 Op_not
   15     40 Comma
   16     16 Op_multiply
   16     40 Op_assign
   17     16 Op_divide
   17     40 Integer           42
   18     16 Op_mod
   18     40 String            "String literal"
   19     16 Op_add
   19     40 Identifier        variable_name
   20     26 Integer           10
   21     26 Integer           92
   22     26 Integer           32
   22     30 End_of_input
```

</b>

## Java


```java

// Translated from python source

import java.io.File;
import java.io.FileNotFoundException;
import java.util.HashMap;
import java.util.Map;
import java.util.Scanner;

public class Lexer {
    private int line;
    private int pos;
    private int position;
    private char chr;
    private String s;

    Map<String, TokenType> keywords = new HashMap<>();

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
            String result = String.format("%5d  %5d %-15s", this.line, this.pos, this.tokentype);
            switch (this.tokentype) {
                case Integer:
                    result += String.format("  %4s", value);
                    break;
                case Identifier:
                    result += String.format(" %s", value);
                    break;
                case String:
                    result += String.format(" \"%s\"", value);
                    break;
            }
            return result;
        }
    }

    static enum TokenType {
        End_of_input, Op_multiply,  Op_divide, Op_mod, Op_add, Op_subtract,
        Op_negate, Op_not, Op_less, Op_lessequal, Op_greater, Op_greaterequal,
        Op_equal, Op_notequal, Op_assign, Op_and, Op_or, Keyword_if,
        Keyword_else, Keyword_while, Keyword_print, Keyword_putc, LeftParen, RightParen,
        LeftBrace, RightBrace, Semicolon, Comma, Identifier, Integer, String
    }

    static void error(int line, int pos, String msg) {
        if (line > 0 && pos > 0) {
            System.out.printf("%s in line %d, pos %d\n", msg, line, pos);
        } else {
            System.out.println(msg);
        }
        System.exit(1);
    }

    Lexer(String source) {
        this.line = 1;
        this.pos = 0;
        this.position = 0;
        this.s = source;
        this.chr = this.s.charAt(0);
        this.keywords.put("if", TokenType.Keyword_if);
        this.keywords.put("else", TokenType.Keyword_else);
        this.keywords.put("print", TokenType.Keyword_print);
        this.keywords.put("putc", TokenType.Keyword_putc);
        this.keywords.put("while", TokenType.Keyword_while);

    }
    Token follow(char expect, TokenType ifyes, TokenType ifno, int line, int pos) {
        if (getNextChar() == expect) {
            getNextChar();
            return new Token(ifyes, "", line, pos);
        }
        if (ifno == TokenType.End_of_input) {
            error(line, pos, String.format("follow: unrecognized character: (%d) '%c'", (int)this.chr, this.chr));
        }
        return new Token(ifno, "", line, pos);
    }
    Token char_lit(int line, int pos) {
        char c = getNextChar(); // skip opening quote
        int n = (int)c;
        if (c == '\'') {
            error(line, pos, "empty character constant");
        } else if (c == '\\') {
            c = getNextChar();
            if (c == 'n') {
                n = 10;
            } else if (c == '\\') {
                n = '\\';
            } else {
                error(line, pos, String.format("unknown escape sequence \\%c", c));
            }
        }
        if (getNextChar() != '\'') {
            error(line, pos, "multi-character constant");
        }
        getNextChar();
        return new Token(TokenType.Integer, "" + n, line, pos);
    }
    Token string_lit(char start, int line, int pos) {
        String result = "";
        while (getNextChar() != start) {
            if (this.chr == '\u0000') {
                error(line, pos, "EOF while scanning string literal");
            }
            if (this.chr == '\n') {
                error(line, pos, "EOL while scanning string literal");
            }
            result += this.chr;
        }
        getNextChar();
        return new Token(TokenType.String, result, line, pos);
    }
    Token div_or_comment(int line, int pos) {
        if (getNextChar() != '*') {
            return new Token(TokenType.Op_divide, "", line, pos);
        }
        getNextChar();
        while (true) {
            if (this.chr == '\u0000') {
                error(line, pos, "EOF in comment");
            } else if (this.chr == '*') {
                if (getNextChar() == '/') {
                    getNextChar();
                    return getToken();
                }
            } else {
                getNextChar();
            }
        }
    }
    Token identifier_or_integer(int line, int pos) {
        boolean is_number = true;
        String text = "";

        while (Character.isAlphabetic(this.chr) || Character.isDigit(this.chr) || this.chr == '_') {
            text += this.chr;
            if (!Character.isDigit(this.chr)) {
                is_number = false;
            }
            getNextChar();
        }

        if (text.equals("")) {
            error(line, pos, String.format("identifer_or_integer unrecopgnized character: (%d) %c", (int)this.chr, this.chr));
        }

        if (Character.isDigit(text.charAt(0))) {
            if (!is_number) {
                error(line, pos, String.format("invaslid number: %s", text));
            }
            return new Token(TokenType.Integer, text, line, pos);
        }

        if (this.keywords.containsKey(text)) {
            return new Token(this.keywords.get(text), "", line, pos);
        }
        return new Token(TokenType.Identifier, text, line, pos);
    }
    Token getToken() {
        int line, pos;
        while (Character.isWhitespace(this.chr)) {
            getNextChar();
        }
        line = this.line;
        pos = this.pos;

        switch (this.chr) {
            case '\u0000': return new Token(TokenType.End_of_input, "", this.line, this.pos);
            case '/': return div_or_comment(line, pos);
            case '\'': return char_lit(line, pos);
            case '<': return follow('=', TokenType.Op_lessequal, TokenType.Op_less, line, pos);
            case '>': return follow('=', TokenType.Op_greaterequal, TokenType.Op_greater, line, pos);
            case '=': return follow('=', TokenType.Op_equal, TokenType.Op_assign, line, pos);
            case '!': return follow('=', TokenType.Op_notequal, TokenType.Op_not, line, pos);
            case '&': return follow('&', TokenType.Op_and, TokenType.End_of_input, line, pos);
            case '|': return follow('|', TokenType.Op_or, TokenType.End_of_input, line, pos);
            case '"': return string_lit(this.chr, line, pos);
            case '{': getNextChar(); return new Token(TokenType.LeftBrace, "", line, pos);
            case '}': getNextChar(); return new Token(TokenType.RightBrace, "", line, pos);
            case '(': getNextChar(); return new Token(TokenType.LeftParen, "", line, pos);
            case ')': getNextChar(); return new Token(TokenType.RightParen, "", line, pos);
            case '+': getNextChar(); return new Token(TokenType.Op_add, "", line, pos);
            case '-': getNextChar(); return new Token(TokenType.Op_subtract, "", line, pos);
            case '*': getNextChar(); return new Token(TokenType.Op_multiply, "", line, pos);
            case '%': getNextChar(); return new Token(TokenType.Op_mod, "", line, pos);
            case ';': getNextChar(); return new Token(TokenType.Semicolon, "", line, pos);
            case ',': getNextChar(); return new Token(TokenType.Comma, "", line, pos);

            default: return identifier_or_integer(line, pos);
        }
    }

    char getNextChar() {
        this.pos++;
        this.position++;
        if (this.position >= this.s.length()) {
            this.chr = '\u0000';
            return this.chr;
        }
        this.chr = this.s.charAt(this.position);
        if (this.chr == '\n') {
            this.line++;
            this.pos = 0;
        }
        return this.chr;
    }

    void printTokens() {
        Token t;
        while ((t = getToken()).tokentype != TokenType.End_of_input) {
            System.out.println(t);
        }
        System.out.println(t);
    }
    public static void main(String[] args) {
        if (args.length > 0) {
            try {

                File f = new File(args[0]);
                Scanner s = new Scanner(f);
                String source = " ";
                while (s.hasNext()) {
                    source += s.nextLine() + "\n";
                }
                Lexer l = new Lexer(source);
                l.printTokens();
            } catch(FileNotFoundException e) {
                error(-1, -1, "Exception: " + e.getMessage());
            }
        } else {
            error(-1, -1, "No args");
        }
    }
}

```


## JavaScript


```javascript

/*
    Token: type, value, line, pos
*/

const TokenType = {
    Keyword_if: 1, Keyword_else: 2, Keyword_print: 3, Keyword_putc: 4, Keyword_while: 5,
    Op_add: 6, Op_and: 7, Op_assign: 8, Op_divide: 9, Op_equal: 10, Op_greater: 11,
    Op_greaterequal: 12, Op_less: 13, Op_Lessequal: 14, Op_mod: 15, Op_multiply: 16, Op_not: 17,
    Op_notequal: 18, Op_or: 19, Op_subtract: 20,
    Integer: 21, String: 22, Identifier: 23,
    Semicolon: 24, Comma: 25,
    LeftBrace: 26, RightBrace: 27,
    LeftParen: 28, RightParen: 29,
    End_of_input: 99
}

class Lexer {
    constructor(source) {
        this.source = source
        this.pos = 1        // position in line
        this.position = 0   // position in source
        this.line = 1
        this.chr = this.source.charAt(0)
        this.keywords = {
            "if": TokenType.Keyword_if,
            "else": TokenType.Keyword_else,
            "print": TokenType.Keyword_print,
            "putc": TokenType.Keyword_putc,
            "while": TokenType.Keyword_while
        }
    }
    getNextChar() {
        this.pos++
        this.position++

        if (this.position >= this.source.length) {
            this.chr = undefined
            return this.chr
        }
        this.chr = this.source.charAt(this.position)
        if (this.chr === '\n') {
            this.line++
            this.pos = 0
        }
        return this.chr
    }
    error(line, pos, message) {
        if (line > 0 && pos > 0) {
            console.log(message + " in line " + line + ", pos " + pos + "\n")
        } else {
            console.log(message)
        }
        process.exit(1)
    }
    follow(expect, ifyes, ifno, line, pos) {
        if (this.getNextChar() === expect) {
            this.getNextChar()
            return { type: ifyes, value: "", line, pos }
        }
        if (ifno === TokenType.End_of_input) {
            this.error(line, pos, "follow: unrecognized character: (" + this.chr.charCodeAt(0) + ") '" + this.chr + "'")
        }
        return { type: ifno, value: "", line, pos }
    }
    div_or_comment(line, pos) {
        if (this.getNextChar() !== '*') {
            return { type: TokenType.Op_divide, value: "/", line, pos }
        }
        this.getNextChar()
        while (true) {
            if (this.chr === '\u0000') {
                this.error(line, pos, "EOF in comment")
            } else if (this.chr === '*') {
                if (this.getNextChar() === '/') {
                    this.getNextChar()
                    return this.getToken()
                }
            } else {
                this.getNextChar()
            }
        }
    }
    char_lit(line, pos) {
        let c = this.getNextChar() // skip opening quote
        let n = c.charCodeAt(0)
        if (c === "\'") {
            this.error(line, pos, "empty character constant")
        } else if (c === "\\") {
            c = this.getNextChar()
            if (c == "n") {
                n = 10
            } else if (c === "\\") {
                n = 92
            } else {
                this.error(line, pos, "unknown escape sequence \\" + c)
            }
        }
        if (this.getNextChar() !== "\'") {
            this.error(line, pos, "multi-character constant")
        }
        this.getNextChar()
        return { type: TokenType.Integer, value: n, line, pos }
    }
    string_lit(start, line, pos) {
        let value = ""
        while (this.getNextChar() !== start) {
            if (this.chr === undefined) {
                this.error(line, pos, "EOF while scanning string literal")
            }
            if (this.chr === "\n") {
                this.error(line, pos, "EOL while scanning string literal")
            }
            value += this.chr
        }
        this.getNextChar()
        return { type: TokenType.String, value, line, pos }
    }
    identifier_or_integer(line, pos) {
        let is_number = true
        let text = ""

        while (/\w/.test(this.chr) || this.chr === '_') {
            text += this.chr
            if (!/\d/.test(this.chr)) {
                is_number = false
            }
            this.getNextChar()
        }
        if (text === "") {
            this.error(line, pos, "identifer_or_integer unrecopgnized character: follow: unrecognized character: (" + this.chr.charCodeAt(0) + ") '" + this.chr + "'")
        }

        if (/\d/.test(text.charAt(0))) {
            if (!is_number) {
                this.error(line, pos, "invaslid number: " + text)
            }
            return { type: TokenType.Integer, value: text, line, pos }
        }

        if (text in this.keywords) {
            return { type: this.keywords[text], value: "", line, pos }
        }
        return { type: TokenType.Identifier, value: text, line, pos }
    }
    getToken() {
        let pos, line
        // Ignore whitespaces
        while (/\s/.test(this.chr)) { this.getNextChar() }
        line = this.line; pos = this.pos
        switch (this.chr) {
            case undefined: return { type: TokenType.End_of_input, value: "", line: this.line, pos: this.pos }
            case "/":       return this.div_or_comment(line, pos)
            case "\'":      return this.char_lit(line, pos)
            case "\"":      return this.string_lit(this.chr, line, pos)

            case "<":       return this.follow("=", TokenType.Op_lessequal, TokenType.Op_less, line, pos)
            case ">":       return this.follow("=", TokenType.Op_greaterequal, TokenType.Op_greater, line, pos)
            case "=":       return this.follow("=", TokenType.Op_equal, TokenType.Op_assign, line, pos)
            case "!":       return this.follow("=", TokenType.Op_notequal, TokenType.Op_not, line, pos)
            case "&":       return this.follow("&", TokenType.Op_and, TokenType.End_of_input, line, pos)
            case "|":       return this.follow("|", TokenType.Op_or, TokenType.End_of_input, line, pos)

            case "{":       this.getNextChar(); return { type: TokenType.LeftBrace, value: "{", line, pos }
            case "}":       this.getNextChar(); return { type: TokenType.RightBrace, value: "}", line, pos }
            case "(":       this.getNextChar(); return { type: TokenType.LeftParen, value: "(", line, pos }
            case ")":       this.getNextChar(); return { type: TokenType.RightParen, value: ")", line, pos }
            case "+":       this.getNextChar(); return { type: TokenType.Op_add, value: "+", line, pos }
            case "-":       this.getNextChar(); return { type: TokenType.Op_subtract, value: "-", line, pos }
            case "*":       this.getNextChar(); return { type: TokenType.Op_multiply, value: "*", line, pos }
            case "%":       this.getNextChar(); return { type: TokenType.Op_mod, value: "%", line, pos }
            case ";":       this.getNextChar(); return { type: TokenType.Semicolon, value: ";", line, pos }
            case ",":       this.getNextChar(); return { type: TokenType.Comma, value: ",", line, pos }

            default:        return this.identifier_or_integer(line, pos)
        }
    }
    /*
    https://stackoverflow.com/questions/9907419/how-to-get-a-key-in-a-javascript-object-by-its-value
    */
    getTokenType(value) {
        return Object.keys(TokenType).find(key => TokenType[key] === value)
    }
    printToken(t) {
        let result = ("     " + t.line).substr(t.line.toString().length)
        result += ("       " + t.pos).substr(t.pos.toString().length)
        result += (" " + this.getTokenType(t.type) + "           ").substr(0, 16)
        switch (t.type) {
            case TokenType.Integer:
                result += "  " + t.value
                break;
            case TokenType.Identifier:
                result += " " + t.value
                break;
            case TokenType.String:
                result += " \""+ t.value + "\""
                break;
        }
        console.log(result)
    }
    printTokens() {
        let t
        while ((t = this.getToken()).type !== TokenType.End_of_input) {
            this.printToken(t)
        }
        this.printToken(t)
    }
}
const fs = require("fs")
fs.readFile(process.argv[2], "utf8", (err, data) => {
    l = new Lexer(data)
    l.printTokens()
})

```



## Julia


```julia
struct Tokenized
    startline::Int
    startcol::Int
    name::String
    value::Union{Nothing, Int, String}
end

const optokens = Dict("*" => "Op_multiply", "/" => "Op_divide", "%" => "Op_mod", "+" => "Op_add",
                      "-" => "Op_subtract", "!" => "Op_not", "<" => "Op_less", "<=" => "Op_lessequal",
                      ">" => "Op_greater", ">=" => "Op_greaterequal", "==" => "Op_equal", "!=" => "Op_notequal",
                      "!" => "Op_not", "=" => "Op_assign", "&&" => "Op_and", "||" => "Op_or")

const keywordtokens = Dict("if" => "Keyword_if", "else" => "Keyword_else", "while" => "Keyword_while",
                           "print" => "Keyword_print", "putc" => "Keyword_putc")

const symboltokens = Dict("(" => "LeftParen", ")" => "RightParen", "{" => "LeftBrace",
                          "}" => "RightBrace", ";" => "Semicolon", "," => "Comma")

const errors = ["Empty character constant.", "Unknown escape sequence.", "Multi-character constant.",
                "End-of-file in comment. Closing comment characters not found.",
                "End-of-file while scanning string literal. Closing string character not found.",
                "End-of-line while scanning string literal. Closing string character not found before end-of-line.",
                "Unrecognized character.", "Invalid number. Starts like a number, but ends in non-numeric characters."]

asws(s) = (nnl = length(findall(x->x=='\n', s)); " " ^ (length(s) - nnl) * "\n" ^ nnl)
comment2ws(t) = (while occursin("/*", t) t = replace(t, r"\/\* .+? (?: \*\/)"xs => asws; count = 1) end; t)
hasinvalidescapes(t) = ((m = match(r"\\.", t)) != nothing && m.match != "\\\\" && m.match != "\\n")
hasemptycharconstant(t) = (match(r"\'\'", t) != nothing)
hasmulticharconstant(t) = ((m = match(r"\'[^\'][^\']+\'", t)) != nothing && m.match != "\'\\\\\'" && m.match != "\'\\n\'")
hasunbalancedquotes(t) = isodd(length(findall(x -> x == '\"', t)))
hasunrecognizedchar(t) = match(r"[^\w\s\d\*\/\%\+\-\<\>\=\!\&\|\(\)\{\}\;\,\"\'\\]", t) != nothing

function throwiferror(line, n)
    if hasemptycharconstant(line)
        throw("Tokenizer error line $n: " * errors[1])
    end
    if hasinvalidescapes(line)
        throw("Tokenizer error line $n: " * errors[2])
    end
    if hasmulticharconstant(line)
    println("error at ", match(r"\'[^\'][^\']+\'", line).match)
        throw("Tokenizer error line $n: " * errors[3])
    end
    if occursin("/*", line)
        throw("Tokenizer error line $n: " * errors[4])
    end
    if hasunrecognizedchar(line)
        throw("Tokenizer error line $n: " * errors[7])
    end
end

function tokenize(txt)
    tokens = Vector{Tokenized}()
    txt = comment2ws(txt)
    lines = split(txt, "\n")
    if hasunbalancedquotes(txt)
        throw("Tokenizer error: $(errors[5])")
    end
    for (startline, line) in enumerate(lines)
        if strip(line) == ""
            continue
        end
        throwiferror(line, startline)
        lastc = Char(0)
        withintoken = 0
        for (startcol, c) in enumerate(line)
            if withintoken > 0
                withintoken -= 1
                continue
            elseif isspace(c[1])
                continue
            elseif (c == '=') && (startcol > 1) && ((c2 = line[startcol - 1]) in ['<', '>', '=', '!'])
                    tokens[end] = Tokenized(startline, startcol - 1, optokens[c2 * c], nothing)
            elseif (c == '&') || (c == '|')
                if length(line) > startcol && line[startcol + 1] == c
                    push!(tokens, Tokenized(startline, startcol, optokens[c * c], nothing))
                    withintoken = 1
                else
                    throw("Tokenizer error line $startline: $(errors[7])")
                end
            elseif haskey(optokens, string(c))
                push!(tokens, Tokenized(startline, startcol, optokens[string(c)], nothing))
            elseif haskey(symboltokens, string(c))
                push!(tokens, Tokenized(startline, startcol, symboltokens[string(c)], nothing))
            elseif isdigit(c)
                integerstring = match(r"^\d+", line[startcol:end]).match
                pastnumposition = startcol + length(integerstring)
                if (pastnumposition <= length(line)) && isletter(line[pastnumposition])
                    throw("Tokenizer error line $startline: " * errors[8])
                end
                i = parse(Int, integerstring)
                push!(tokens, Tokenized(startline, startcol, "Integer", i))
                withintoken = length(integerstring) - 1
            elseif c == Char(39)  # single quote
                if (m = match(r"([^\\\'\n]|\\n|\\\\)\'", line[startcol+1:end])) != nothing
                    chs = m.captures[1]
                    i = (chs == "\\n") ? Int('\n') : (chs == "\\\\" ? Int('\\') : Int(chs[1]))
                    push!(tokens, Tokenized(startline, startcol, "Integer", i))
                    withintoken = length(chs) + 1
                else
                    println("line $startline: bad match with ", line[startcol+1:end])
                end
            elseif c == Char(34)  # double quote
                if (m = match(r"([^\"\n]+)\"", line[startcol+1:end])) == nothing
                    throw("Tokenizer error line $startline: $(errors[6])")
                end
                litstring = m.captures[1]
                push!(tokens, Tokenized(startline, startcol, "String", "\"$litstring\""))
                withintoken = length(litstring) + 1
            elseif (cols = findfirst(r"[a-zA-Z]+", line[startcol:end])) != nothing
                litstring = line[cols .+ startcol .- 1]
                if haskey(keywordtokens, string(litstring))
                    push!(tokens, Tokenized(startline, startcol, keywordtokens[litstring], nothing))
                else
                    litstring = match(r"[_a-zA-Z0-9]+", line[startcol:end]).match
                    push!(tokens, Tokenized(startline, startcol, "Identifier", string(litstring)))
                end
                withintoken = length(litstring) - 1
            end
            lastc = c
        end
    end
    push!(tokens, Tokenized(length(lines), length(lines[end]) + 1, "End_of_input", nothing))
    tokens
end

const test3txt = raw"""
/*
  All lexical tokens - not syntactically correct, but that will
  have to wait until syntax analysis
 */
/* Print   */  print    /* Sub     */  -
/* Putc    */  putc     /* Lss     */  <
/* If      */  if       /* Gtr     */  >
/* Else    */  else     /* Leq     */  <=
/* While   */  while    /* Geq     */  >=
/* Lbrace  */  {        /* Eq      */  ==
/* Rbrace  */  }        /* Neq     */  !=
/* Lparen  */  (        /* And     */  &&
/* Rparen  */  )        /* Or      */  ||
/* Uminus  */  -        /* Semi    */  ;
/* Not     */  !        /* Comma   */  ,
/* Mul     */  *        /* Assign  */  =
/* Div     */  /        /* Integer */  42
/* Mod     */  %        /* String  */  "String literal"
/* Add     */  +        /* Ident   */  variable_name
/* character literal */  '\n'
/* character literal */  '\\'
/* character literal */  ' '
"""

println("Line Col        Name        Value")
for tok in tokenize(test3txt)
    println(lpad(tok.startline, 3), lpad(tok.startcol, 5), lpad(tok.name, 18), "  ", tok.value != nothing ? tok.value : "")
end

```
{{output}}
```txt

Line Col        Name        Value
  5   16     Keyword_print
  5   40       Op_subtract
  6   16      Keyword_putc
  6   40           Op_less
  7   16        Keyword_if
  7   40        Op_greater
  8   16      Keyword_else
  8   40      Op_lessequal
  9   16     Keyword_while
  9   40   Op_greaterequal
 10   16         LeftBrace
 10   40          Op_equal
 11   16        RightBrace
 11   40       Op_notequal
 12   16         LeftParen
 12   40            Op_and
 13   16        RightParen
 13   40             Op_or
 14   16       Op_subtract
 14   40         Semicolon
 15   16            Op_not
 15   40             Comma
 16   16       Op_multiply
 16   40         Op_assign
 17   16         Op_divide
 17   40           Integer  42
 18   16            Op_mod
 18   40            String  "String literal"
 19   16            Op_add
 19   40        Identifier  variable_name
 20   26           Integer  10
 21   26           Integer  92
 22   26           Integer  32
 23    1      End_of_input

```



## Go

{{trans|FreeBASIC}}

```go
package main

import (
    "bufio"
    "fmt"
    "log"
    "os"
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
    tkEq
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

type Symbol struct {
    name string
    tok  TokenType
}

// symbol table
var symtab []Symbol

var scanner *bufio.Scanner

var (
    curLine = ""
    curCh   byte
    lineNum = 0
    colNum  = 0
)

const etx byte = 4 // used to signify EOI

func isDigit(ch byte) bool {
    return ch >= '0' && ch <= '9'
}

func isAlnum(ch byte) bool {
    return (ch >= 'a' && ch <= 'z') || (ch >= 'A' && ch <= 'Z') || isDigit(ch)
}

func errorMsg(eline, ecol int, msg string) {
    log.Fatalf("(%d:%d) %s", eline, ecol, msg)
}

// add an identifier to the symbol table
func install(name string, tok TokenType) {
    sym := Symbol{name, tok}
    symtab = append(symtab, sym)
}

// search for an identifier in the symbol table
func lookup(name string) int {
    for i := 0; i < len(symtab); i++ {
        if symtab[i].name == name {
            return i
        }
    }
    return -1
}

// read the next line of input from the source file
func nextLine() {
    if scanner.Scan() {
        curLine = scanner.Text()
        lineNum++
        colNum = 0
        if curLine == "" { // skip blank lines
            nextLine()
        }
    } else {
        err := scanner.Err()
        if err == nil { // EOF
            curCh = etx
            curLine = ""
            lineNum++
            colNum = 1
        } else {
            log.Fatal(err)
        }
    }
}

// get the next char
func nextChar() {
    if colNum >= len(curLine) {
        nextLine()
    }
    if colNum < len(curLine) {
        curCh = curLine[colNum]
        colNum++
    }
}

func follow(eline, ecol int, expect byte, ifyes, ifno TokenType) TokenType {
    if curCh == expect {
        nextChar()
        return ifyes
    }
    if ifno == tkEOI {
        errorMsg(eline, ecol, "follow unrecognized character: "+string(curCh))
    }
    return ifno
}

func gettok() (eline, ecol int, tok TokenType, v string) {
    // skip whitespace
    for curCh == ' ' || curCh == '\t' || curCh == '\n' {
        nextChar()
    }
    eline = lineNum
    ecol = colNum
    switch curCh {
    case etx:
        tok = tkEOI
        return
    case '{':
        tok = tkLbrace
        nextChar()
        return
    case '}':
        tok = tkRbrace
        nextChar()
        return
    case '(':
        tok = tkLparen
        nextChar()
        return
    case ')':
        tok = tkRparen
        nextChar()
        return
    case '+':
        tok = tkAdd
        nextChar()
        return
    case '-':
        tok = tkSub
        nextChar()
        return
    case '*':
        tok = tkMul
        nextChar()
        return
    case '%':
        tok = tkMod
        nextChar()
        return
    case ';':
        tok = tkSemi
        nextChar()
        return
    case ',':
        tok = tkComma
        nextChar()
        return
    case '/': // div or comment
        nextChar()
        if curCh != '*' {
            tok = tkDiv
            return
        }
        // skip comments
        nextChar()
        for {
            if curCh == '*' {
                nextChar()
                if curCh == '/' {
                    nextChar()
                    eline, ecol, tok, v = gettok()
                    return
                }
            } else if curCh == etx {
                errorMsg(eline, ecol, "EOF in comment")
            } else {
                nextChar()
            }
        }
    case '\'': // single char literals
        nextChar()
        v = fmt.Sprintf("%d", curCh)
        if curCh == '\'' {
            errorMsg(eline, ecol, "Empty character constant")
        }
        if curCh == '\\' {
            nextChar()
            if curCh == 'n' {
                v = "10"
            } else if curCh == '\\' {
                v = "92"
            } else {
                errorMsg(eline, ecol, "unknown escape sequence: "+string(curCh))
            }
        }
        nextChar()
        if curCh != '\'' {
            errorMsg(eline, ecol, "multi-character constant")
        }
        nextChar()
        tok = tkInteger
        return
    case '<':
        nextChar()
        tok = follow(eline, ecol, '=', tkLeq, tkLss)
        return
    case '>':
        nextChar()
        tok = follow(eline, ecol, '=', tkGeq, tkGtr)
        return
    case '!':
        nextChar()
        tok = follow(eline, ecol, '=', tkNeq, tkNot)
        return
    case '=':
        nextChar()
        tok = follow(eline, ecol, '=', tkEq, tkAssign)
        return
    case '&':
        nextChar()
        tok = follow(eline, ecol, '&', tkAnd, tkEOI)
        return
    case '|':
        nextChar()
        tok = follow(eline, ecol, '|', tkOr, tkEOI)
        return
    case '"': // string
        v = string(curCh)
        nextChar()
        for curCh != '"' {
            if curCh == '\n' {
                errorMsg(eline, ecol, "EOL in string")
            }
            if curCh == etx {
                errorMsg(eline, ecol, "EOF in string")
            }
            v += string(curCh)
            nextChar()
        }
        v += string(curCh)
        nextChar()
        tok = tkString
        return
    default: // integers or identifiers
        isNumber := isDigit(curCh)
        v = ""
        for isAlnum(curCh) || curCh == '_' {
            if !isDigit(curCh) {
                isNumber = false
            }
            v += string(curCh)
            nextChar()
        }
        if len(v) == 0 {
            errorMsg(eline, ecol, "unknown character: "+string(curCh))
        }
        if isDigit(v[0]) {
            if !isNumber {
                errorMsg(eline, ecol, "invalid number: "+string(curCh))
            }
            tok = tkInteger
            return
        }
        index := lookup(v)
        if index == -1 {
            tok = tkIdent
        } else {
            tok = symtab[index].tok
        }
        return
    }
}

func initLex() {
    install("else", tkElse)
    install("if", tkIf)
    install("print", tkPrint)
    install("putc", tkPutc)
    install("while", tkWhile)
    nextChar()
}

func process() {
    tokMap := make(map[TokenType]string)
    tokMap[tkEOI] = "End_of_input"
    tokMap[tkMul] = "Op_multiply"
    tokMap[tkDiv] = "Op_divide"
    tokMap[tkMod] = "Op_mod"
    tokMap[tkAdd] = "Op_add"
    tokMap[tkSub] = "Op_subtract"
    tokMap[tkNegate] = "Op_negate"
    tokMap[tkNot] = "Op_not"
    tokMap[tkLss] = "Op_less"
    tokMap[tkLeq] = "Op_lessequal"
    tokMap[tkGtr] = "Op_greater"
    tokMap[tkGeq] = "Op_greaterequal"
    tokMap[tkEq] = "Op_equal"
    tokMap[tkNeq] = "Op_notequal"
    tokMap[tkAssign] = "Op_assign"
    tokMap[tkAnd] = "Op_and"
    tokMap[tkOr] = "Op_or"
    tokMap[tkIf] = "Keyword_if"
    tokMap[tkElse] = "Keyword_else"
    tokMap[tkWhile] = "Keyword_while"
    tokMap[tkPrint] = "Keyword_print"
    tokMap[tkPutc] = "Keyword_putc"
    tokMap[tkLparen] = "LeftParen"
    tokMap[tkRparen] = "RightParen"
    tokMap[tkLbrace] = "LeftBrace"
    tokMap[tkRbrace] = "RightBrace"
    tokMap[tkSemi] = "Semicolon"
    tokMap[tkComma] = "Comma"
    tokMap[tkIdent] = "Identifier"
    tokMap[tkInteger] = "Integer"
    tokMap[tkString] = "String"

    for {
        eline, ecol, tok, v := gettok()
        fmt.Printf("%5d  %5d %-16s", eline, ecol, tokMap[tok])
        if tok == tkInteger || tok == tkIdent || tok == tkString {
            fmt.Println(v)
        } else {
            fmt.Println()
        }
        if tok == tkEOI {
            return
        }
    }
}

func check(err error) {
    if err != nil {
        log.Fatal(err)
    }
}

func main() {
    if len(os.Args) < 2 {
        fmt.Println("Filename required")
        return
    }
    f, err := os.Open(os.Args[1])
    check(err)
    defer f.Close()
    scanner = bufio.NewScanner(f)
    initLex()
    process()
}
```


{{out}}
Test Case 3:

```txt

    5     16 Keyword_print
    5     40 Op_subtract
    6     16 Keyword_putc
    6     40 Op_less
    7     16 Keyword_if
    7     40 Op_greater
    8     16 Keyword_else
    8     40 Op_lessequal
    9     16 Keyword_while
    9     40 Op_greaterequal
   10     16 LeftBrace
   10     40 Op_equal
   11     16 RightBrace
   11     40 Op_notequal
   12     16 LeftParen
   12     40 Op_and
   13     16 RightParen
   13     40 Op_or
   14     16 Op_subtract
   14     40 Semicolon
   15     16 Op_not
   15     40 Comma
   16     16 Op_multiply
   16     40 Op_assign
   17     16 Op_divide
   17     40 Integer         42
   18     16 Op_mod
   18     40 String          "String literal"
   19     16 Op_add
   19     40 Identifier      variable_name
   20     26 Integer         10
   21     26 Integer         92
   22     26 Integer         32
   23      1 End_of_input

```


## M2000 Interpreter


```M2000 Interpreter

Module lexical_analyzer {
	a$={/*
	  All lexical tokens - not syntactically correct, but that will
	  have to wait until syntax analysis
	 */
	/* Print   */  print    /* Sub     */  -
	/* Putc    */  putc     /* Lss     */  <
	/* If      */  if       /* Gtr     */  >
	/* Else    */  else     /* Leq     */  <=
	/* While   */  while    /* Geq     */  >=
	/* Lbrace  */  {        /* Eq      */  ==
	/* Rbrace  */  }        /* Neq     */  !=
	/* Lparen  */  (        /* And     */  &&
	/* Rparen  */  )        /* Or      */  ||
	/* Uminus  */  -        /* Semi    */  ;
	/* Not     */  !        /* Comma   */  ,
	/* Mul     */  *        /* Assign  */  =
	/* Div     */  /        /* Integer */  42
	/* Mod     */  %        /* String  */  "String literal"
	/* Add     */  +        /* Ident   */  variable_name
	/* character literal */  '\n'
	/* character literal */  '\\'
	/* character literal */  ' '
	}
	lim=Len(a$)
	LineNo=1
	ColumnNo=1
	Document Output$
	Buffer Scanner as Integer*lim
	Return Scanner, 0:=a$
	offset=0
	buffer1$=""
	flag_rem=true
	Ahead=lambda Scanner (a$, offset)->{
		=false
		Try {
			\\ second parameter is the offset in buffer units
			\\ third parameter is length in bytes
			=Eval$(Scanner, offset,2*len(a$))=a$
		}
	}
	Ahead2=lambda Scanner (a$, offset)->{
		=false
		Try {
			=Eval$(Scanner, offset,2) ~ a$
		}
	}
	const nl$=chr$(13)+chr$(10), quo$="""", er$="@", Ansi=3
	Try {
		Do
		If Ahead("/*", offset) Then {
			offset+=2 : 	ColumnNo+=2
			While not Ahead("*/", offset)
				If Ahead(nl$, offset) Then
					lineNo++: ColumnNo=1 : offset+=2
				Else
					offset++ : ColumnNo++
				End If
				if offset>lim then
					Error "End-of-file in comment. Closing comment characters not found"+er$
				End if
			End While
			offset+=2 : ColumnNo+=2
		} Else.if Ahead(nl$, offset) Then{
			LineNo++: ColumnNo=1
			offset+=2
		} Else.if Ahead(quo$, offset) Then {
			Output$=format$("{0::-10}{1::-10} ", LineNo, ColumnNo)
			offset++ : ColumnNo++
			strin=offset
			While not Ahead(quo$, offset)
				If Ahead("/", offset) Then
					offset+=2 : ColumnNo+=2
				else
					offset++ : ColumnNo++
				End if
				checkerror()
			End While
			Output$="String "+quote$(Eval$(Scanner, strin, (offset-strin)*2))+nl$
			offset++ : ColumnNo++
		} Else.if Ahead("'", offset) Then {
			Output$=format$("{0::-10}{1::-10} ", LineNo, ColumnNo)
			offset++ : ColumnNo++
			strin=offset
			While not Ahead("'", offset)
				If Ahead("/", offset) Then
					offset+=2 : ColumnNo+=2
				else
					offset++ : ColumnNo++
				End if
				checkerror()
			End While
			lit$=format$(Eval$(Scanner, strin, (offset-strin)*2))
			select case len(lit$)
			case 1
				Output$="Integer "+str$(asc(lit$),0)+nl$
			case >1
				{Error "Multi-character constant."+er$}
			case 0
				{Error "Empty character constant."+er$}
			end select
			offset++ : ColumnNo++
		} Else.if Ahead2("[a-z]", offset) Then {
			strin=offset
			Output$=format$("{0::-10}{1::-10} ", LineNo, ColumnNo)
			offset++ : ColumnNo++
			While Ahead2("[a-zA-Z0-9_]", offset)
					offset++ : ColumnNo++
			End While
			Keywords(Eval$(Scanner, strin, (offset-strin)*2))
		} Else.if Ahead2("[0-9]", offset) Then {
			strin=offset
			Output$=format$("{0::-10}{1::-10} Integer ", LineNo, ColumnNo)
			offset++ : ColumnNo++
			While Ahead2("[0-9]", offset)
					offset++ : ColumnNo++
			End While
			if Ahead2("[a-zA-Z_]", offset) then
				{Error " Invalid number. Starts like a number, but ends in non-numeric characters."+er$}
			else
				Output$=Eval$(Scanner, strin, (offset-strin)*2)+nl$
			end if
		} Else {
			Symbols(Eval$(Scanner, Offset, 2))
			offset++ : ColumnNo++
		}
		Until offset>=lim
	}
	er1$=leftpart$(error$,er$)
	if er1$<>"" then
		Print
		Report "Error:"+er1$
		Output$="(Error)"+nl$+"Error:"+er1$
	else
		Output$=format$("{0::-10}{1::-10}", LineNo, ColumnNo)+" End_of_Input"+nl$
	end if
	Clipboard Output$
	Save.Doc Output$, "lex.t", Ansi
	document lex$
	Load.Doc lex$,"lex.t", Ansi
	Report lex$

	Sub Keywords(a$)
		select case a$
		case "if"
			a$="Keyword_if"
		case "else"
			a$="Keyword_else"
		case "while"
			a$="Keyword_while"
		case "print"
			a$="Keyword_print"
		case "putc"
			a$="Keyword_putc"
		else case
			a$="Identifier "+a$
		end select
		Output$=a$+nl$
	End sub
	Sub Symbols(a$)
		select case a$
		case " ", chr$(9)
			a$=""
		case "("
			a$="LeftParen"
		case ")"
			a$="RightParen"
		case "{"
			a$="LeftBrace"
		case "}"
			a$="RightBrace"
		case ";"
			a$="Semicolon"
		case ","
			a$="Comma"
		case "*"
			a$="Op_multiply"
		case "/"
			a$="Op_divide"
		case "+"
			a$="Op_add"
		case "-"
			a$="Op_subtract"
		case "%"
			a$="Op_mod"
		case "<"
		{	if Ahead("=", offset+1) Then
				offset++
				a$="Op_lessequal"
				ColumnNo++
			else
				a$="Op_less"
			end if
		}
		case ">"
		{	if Ahead("=", offset+1) Then
				offset++
				ColumnNo++
				a$="Op_greaterequal"
			else
				a$="Op_greater"
			end if
		}
		case "="
		{	if Ahead("=", offset+1) Then
				offset++
				ColumnNo++
				a$="Op_equal"
			else
				a$="Op_assign"
			end if
		}
		case "!"
		{	if Ahead("=", offset+1) Then
				offset++
				ColumnNo++
				a$="Op_notequal"
			else
				a$="Op_not"
			end if
		}
		case "&"
		{	if Ahead("&", offset+1) Then
				offset++
				ColumnNo++
				a$="Op_and"
			else
				a$=""
			end if
		}
		case "|"
		{	if Ahead("|", offset+1) Then
				offset++
				ColumnNo++
				a$="Op_or"
			else
				a$=""
			end if
		}
		else case
			{Error "Unrecognized character."+er$}
		end select
		if a$<>"" then
		Output$=format$("{0::-10}{1::-10} ", LineNo, ColumnNo)+a$+nl$
		end if
	End Sub
	Sub checkerror()
		if offset>lim then {
			Error "End-of-line while scanning string literal. Closing string character not found before end-of-line."+er$
		} else.if  Ahead(nl$,offset) then {
			Error "End-of-file while scanning string literal. Closing string character not found."+er$
		}
	End Sub
}
lexical_analyzer

```


{{out}}
<pre style="height:30ex;overflow:scroll">
         5        16 Keyword_print
         5        40 Op_subtract
         6        16 Keyword_putc
         6        40 Op_less
         7        16 Keyword_if
         7        40 Op_greater
         8        16 Keyword_else
         8        41 Op_lessequal
         9        16 Keyword_while
         9        41 Op_greaterequal
        10        16 LeftBrace
        10        41 Op_equal
        11        16 RightBrace
        11        41 Op_notequal
        12        16 LeftParen
        12        41 Op_and
        13        16 RightParen
        14        16 Op_subtract
        14        40 Semicolon
        15        16 Op_not
        15        40 Comma
        16        16 Op_multiply
        16        40 Op_assign
        17        16 Op_divide
        17        40 Integer 42
        18        16 Op_mod
        18        40 String "String literal"
        19        16 Op_add
        19        40 Identifier variable_name
        20        26 Integer 10
        21        26 Integer 92
        22        26 Integer 32
        23         1 End_of_Input

</pre >


## Nim

Tested with Nim v0.19.4. Both examples are tested against all programs in [[Compiler/Sample programs]].

### Using string with regular expressions


```nim

import re, strformat, strutils

type
  TokenKind = enum
    tkUnknown = "UNKNOWN_TOKEN",
    tkMul = "Op_multiply",
    tkDiv = "Op_divide",
    tkMod = "Op_mod",
    tkAdd = "Op_add",
    tkSub = "Op_subtract",
    tkNeg = "Op_negate",
    tkLt = "Op_less",
    tkLte = "Op_lessequal",
    tkGt = "Op_greater",
    tkGte = "Op_greaterequal",
    tkEq = "Op_equal",
    tkNeq = "Op_notequal",
    tkNot = "Op_not",
    tkAsgn = "Op_assign",
    tkAnd = "Op_and",
    tkOr = "Op_or",
    tkLpar = "LeftParen",
    tkRpar = "RightParen",
    tkLbra = "LeftBrace",
    tkRbra = "RightBrace",
    tkSmc = "Semicolon",
    tkCom = "Comma",
    tkIf = "Keyword_if",
    tkElse = "Keyword_else",
    tkWhile = "Keyword_while",
    tkPrint = "Keyword_print",
    tkPutc = "Keyword_putc",
    tkId = "Identifier",
    tkInt = "Integer",
    tkChar = "Integer",
    tkStr = "String",
    tkEof = "End_of_input"

  Token = object
    kind: TokenKind
    value: string

  TokenAnn = object
    ## Annotated token with messages for compiler
    token: Token
    line, column: int

proc getSymbols(table: openArray[(char, TokenKind)]): seq[char] =
  result = newSeq[char]()
  for ch, tokenKind in items(table):
    result.add ch

const
  tkSymbols = { # single-char tokens
    '*': tkMul,
    '%': tkMod,
    '+': tkAdd,
    '-': tkSub,
    '(': tkLpar,
    ')': tkRpar,
    '{': tkLbra,
    '}': tkRbra,
    ';': tkSmc,
    ',': tkCom,
    '/': tkDiv, # the comment case /* ... */ is handled in `stripUnimportant`
  }
  symbols = getSymbols(tkSymbols)

proc findTokenKind(table: openArray[(char, TokenKind)]; needle: char):
                  TokenKind =
  for ch, tokenKind in items(table):
    if ch == needle: return tokenKind
  tkUnknown

proc stripComment(text: var string, lineNo, colNo: var int) =
  var matches: array[1, string]

  if match(text, re"\A(/\*[\s\S]*?\*/)", matches):
    text = text[matches[0].len..^1]
    for s in matches[0]:
      if s == '\n':
        inc lineNo
        colNo = 1
      else:
        inc colNo

proc stripUnimportant(text: var string; lineNo, colNo: var int) =
  while true:
    if text.len == 0: return
    elif text[0] == '\n':
      inc lineNo
      colNo = 1
      text = text[1..^1]
    elif text[0] == ' ':
      inc colNo
      text = text[1..^1]
    elif text.len >= 2 and text[0] == '/' and text[1] == '*':
      stripComment(text, lineNo, colNo)
    else: return

proc lookAhead(ch1, ch2: char, tk1, tk2: TokenKind): (TokenKind, int) =
  if ch1 == ch2: (tk1, 2)
  else: (tk2, 1)

proc consumeToken(text: var string; tkl: var int): Token =
  ## Return token removing it from the `text` and write its length to
  ## `tkl`.  If the token can not be defined, return `tkUnknown` as a
  ## token, shrink text by 1 and write 1 to its length.

  var
    matches: array[1, string]
    tKind: TokenKind
    val: string

  if text.len == 0:
    (tKind, tkl) = (tkEof, 0)

  # Simple characters
  elif text[0] in symbols: (tKind, tkl) = (tkSymbols.findTokenKind(text[0]), 1)
  elif text[0] == '<': (tKind, tkl) = lookAhead(text[1], '=', tkLte, tkLt)
  elif text[0] == '>': (tKind, tkl) = lookAhead(text[1], '=', tkGte, tkGt)
  elif text[0] == '=': (tKind, tkl) = lookAhead(text[1], '=', tkEq, tkAsgn)
  elif text[0] == '!': (tKind, tkl) = lookAhead(text[1], '=', tkNeq, tkNot)
  elif text[0] == '&': (tKind, tkl) = lookAhead(text[1], '&', tkAnd, tkUnknown)
  elif text[0] == '|': (tKind, tkl) = lookAhead(text[1], '|', tkOr, tkUnknown)

  # Keywords
  elif match(text, re"\Aif\b"): (tKind, tkl) = (tkIf, 2)
  elif match(text, re"\Aelse\b"): (tKind, tkl) = (tkElse, 4)
  elif match(text, re"\Awhile\b"): (tKind, tkl) = (tkWhile, 5)
  elif match(text, re"\Aprint\b"): (tKind, tkl) = (tkPrint, 5)
  elif match(text, re"\Aputc\b"): (tKind, tkl) = (tkPutc, 4)

  # Literals and identifiers
  elif match(text, re"\A([0-9]+)", matches):
    (tKind, tkl) = (tkInt, matches[0].len)
    val = matches[0]
  elif match(text, re"\A([_a-zA-Z][_a-zA-Z0-9]*)", matches):
    (tKind, tkl) = (tkId, matches[0].len)
    val = matches[0]
  elif match(text, re"\A('(?:[^'\n]|\\\\|\\n)')", matches):
    (tKind, tkl) = (tkChar, matches[0].len)
    val = case matches[0]
          of r"' '": $ord(' ')
          of r"'\n'": $ord('\n')
          of r"'\\'": $ord('\\')
          else: $ord(matches[0][1]) # "'a'"[1] == 'a'
  elif match(text, re"\A(""[^""\n]*"")", matches):
    (tKind, tkl) = (tkStr, matches[0].len)
    val = matches[0]
  else: (tKind, tkl) = (tkUnknown, 1)

  text = text[tkl..^1]
  Token(kind: tKind, value: val)

proc tokenize*(text: string): seq[TokenAnn] =
  result = newSeq[TokenAnn]()
  var
    lineNo, colNo: int = 1
    text = text
    token: Token
    tokenLength: int

  while text.len > 0:
    stripUnimportant(text, lineNo, colNo)
    token = consumeToken(text, tokenLength)
    result.add TokenAnn(token: token, line: lineNo, column: colNo)
    inc colNo, tokenLength

proc output*(s: seq[TokenAnn]): string =
  var
    tokenKind: TokenKind
    value: string
    line, column: int

  for tokenAnn in items(s):
    line = tokenAnn.line
    column = tokenAnn.column
    tokenKind = tokenAnn.token.kind
    value = tokenAnn.token.value
    result.add(
      fmt"{line:>5}{column:>7} {tokenKind:<15}{value}"
        .strip(leading = false) & "\n")

when isMainModule:
  import os

  let input = if paramCount() > 0: readFile paramStr(1)
              else: readAll stdin

  echo input.tokenize.output

```


### Using stream with lexer library


```nim

import lexbase, streams
from strutils import Whitespace

type
  TokenKind = enum
    tkInvalid = "Invalid",
    tkOpMultiply = "Op_multiply",
    tkOpDivide = "Op_divide",
    tkOpMod = "Op_mod",
    tkOpAdd = "Op_add",
    tkOpSubtract = "Op_subtract",
    tkOpLess = "Op_less",
    tkOpLessEqual = "Op_lessequal",
    tkOpGreater = "Op_greater",
    tkOpGreaterEqual = "Op_greaterequal",
    tkOpEqual = "Op_equal",
    tkOpNotEqual = "Op_notequal",
    tkOpNot = "Op_not",
    tkOpAssign = "Op_assign",
    tkOpAnd = "Op_and",
    tkOpOr = "Op_or",
    tkLeftParen = "LeftParen",
    tkRightParen = "RightParen",
    tkLeftBrace = "LeftBrace",
    tkRightBrace = "RightBrace",
    tkSemicolon = "Semicolon",
    tkComma = "Comma",
    tkKeywordIf = "Keyword_if",
    tkKeywordElse = "Keyword_else",
    tkKeywordWhile = "Keyword_while",
    tkKeywordPrint = "Keyword_print",
    tkKeywordPutc = "Keyword_putc",
    tkIdentifier = "Identifier",
    tkInteger = "Integer",
    tkString = "String",
    tkEndOfInput = "End_of_input"

  Lexer = object of BaseLexer
    kind: TokenKind
    token, error: string
    startPos: int

template setError(l: var Lexer; err: string): untyped =
  l.kind = tkInvalid
  if l.error.len == 0:
    l.error = err

proc hasError(l: Lexer): bool {.inline.} =
  l.error.len > 0

proc open(l: var Lexer; input: Stream) {.inline.} =
  lexbase.open(l, input)
  l.startPos = 0
  l.kind = tkInvalid
  l.token = ""
  l.error = ""

proc handleNewLine(l: var Lexer) =
  case l.buf[l.bufpos]
  of '\c': l.bufpos = l.handleCR l.bufpos
  of '\n': l.bufpos = l.handleLF l.bufpos
  else: discard

proc skip(l: var Lexer) =
  while true:
    case l.buf[l.bufpos]
    of Whitespace:
      if l.buf[l.bufpos] notin NewLines:
        inc l.bufpos
      else:
        handleNewLine l
    of '/':
      if l.buf[l.bufpos + 1] == '*':
        inc l.bufpos, 2
        while true:
          case l.buf[l.bufpos]
          of '*':
            if l.buf[l.bufpos + 1] == '/':
              inc l.bufpos, 2
              break
            else: inc l.bufpos
          of NewLines:
            handleNewLine l
          of EndOfFile:
            setError l, "EOF reached in comment"
            return
          else:
            inc l.bufpos
      else: break
    else: break

proc handleSpecial(l: var Lexer): char =
  assert l.buf[l.bufpos] == '\\'
  inc l.bufpos
  case l.buf[l.bufpos]
  of 'n':
    l.token.add "\\n"
    result = '\n'
    inc l.bufpos
  of '\\':
    l.token.add "\\\\"
    result = '\\'
    inc l.bufpos
  else:
    setError l, "Unknown escape sequence: '\\" & l.buf[l.bufpos] & "'"
    result = '\0'

proc handleChar(l: var Lexer) =
  assert l.buf[l.bufpos] == '\''
  l.startPos = l.getColNumber l.bufpos
  l.kind = tkInvalid
  inc l.bufpos
  if l.buf[l.bufpos] == '\\':
    l.token = $ord(handleSpecial l)
    if hasError l: return
  elif l.buf[l.bufpos] == '\'':
    setError l, "Empty character constant"
    return
  else:
    l.token = $ord(l.buf[l.bufpos])
    inc l.bufpos
  if l.buf[l.bufpos] == '\'':
    l.kind = tkInteger
    inc l.bufpos
  else:
    setError l, "Multi-character constant"

proc handleString(l: var Lexer) =
  assert l.buf[l.bufpos] == '"'
  l.startPos = l.getColNumber l.bufpos
  l.token = "\""
  inc l.bufpos
  while true:
    case l.buf[l.bufpos]
    of '\\':
      discard handleSpecial l
      if hasError l: return
    of '"':
      l.kind = tkString
      add l.token, '"'
      inc l.bufpos
      break
    of NewLines:
      setError l, "EOL reached before end-of-string"
      return
    of EndOfFile:
      setError l, "EOF reached before end-of-string"
      return
    else:
      add l.token, l.buf[l.bufpos]
      inc l.bufpos

proc handleNumber(l: var Lexer) =
  assert l.buf[l.bufpos] in {'0'..'9'}
  l.startPos = l.getColNumber l.bufpos
  l.token = "0"
  while l.buf[l.bufpos] == '0': inc l.bufpos
  while true:
    case l.buf[l.bufpos]
    of '0'..'9':
      if l.token == "0":
        setLen l.token, 0
      add l.token, l.buf[l.bufpos]
      inc l.bufpos
    of 'a'..'z', 'A'..'Z', '_':
      setError l, "Invalid number"
      return
    else:
      l.kind = tkInteger
      break

proc handleIdent(l: var Lexer) =
  assert l.buf[l.bufpos] in {'a'..'z'}
  l.startPos = l.getColNumber l.bufpos
  setLen l.token, 0
  while true:
    if l.buf[l.bufpos] in {'a'..'z', 'A'..'Z', '0'..'9', '_'}:
      add l.token, l.buf[l.bufpos]
      inc l.bufpos
    else:
      break
  l.kind = case l.token
           of "if": tkKeywordIf
           of "else": tkKeywordElse
           of "while": tkKeywordWhile
           of "print": tkKeywordPrint
           of "putc": tkKeywordPutc
           else: tkIdentifier

proc getToken(l: var Lexer): TokenKind =
  l.kind = tkInvalid
  setLen l.token, 0
  skip l

  case l.buf[l.bufpos]
  of '*':
    l.kind = tkOpMultiply
    l.startPos = l.getColNumber l.bufpos
    inc l.bufpos
  of '/':
    l.kind = tkOpDivide
    l.startPos = l.getColNumber l.bufpos
    inc l.bufpos
  of '%':
    l.kind = tkOpMod
    l.startPos = l.getColNumber l.bufpos
    inc l.bufpos
  of '+':
    l.kind = tkOpAdd
    l.startPos = l.getColNumber l.bufpos
    inc l.bufpos
  of '-':
    l.kind = tkOpSubtract
    l.startPos = l.getColNumber l.bufpos
    inc l.bufpos
  of '<':
    l.kind = tkOpLess
    l.startPos = l.getColNumber l.bufpos
    inc l.bufpos
    if l.buf[l.bufpos] == '=':
      l.kind = tkOpLessEqual
      inc l.bufpos
  of '>':
    l.kind = tkOpGreater
    l.startPos = l.getColNumber l.bufpos
    inc l.bufpos
    if l.buf[l.bufpos] == '=':
      l.kind = tkOpGreaterEqual
      inc l.bufpos
  of '=':
    l.kind = tkOpAssign
    l.startPos = l.getColNumber l.bufpos
    inc l.bufpos
    if l.buf[l.bufpos] == '=':
      l.kind = tkOpEqual
      inc l.bufpos
  of '!':
    l.kind = tkOpNot
    l.startPos = l.getColNumber l.bufpos
    inc l.bufpos
    if l.buf[l.bufpos] == '=':
      l.kind = tkOpNotEqual
      inc l.bufpos
  of '&':
    if l.buf[l.bufpos + 1] == '&':
      l.kind = tkOpAnd
      l.startPos = l.getColNumber l.bufpos
      inc l.bufpos, 2
    else:
      setError l, "Unrecognized character"
  of '|':
    if l.buf[l.bufpos + 1] == '|':
      l.kind = tkOpOr
      l.startPos = l.getColNumber l.bufpos
      inc l.bufpos, 2
    else:
      setError l, "Unrecognized character"
  of '(':
    l.kind = tkLeftParen
    l.startPos = l.getColNumber l.bufpos
    inc l.bufpos
  of ')':
    l.kind = tkRightParen
    l.startPos = l.getColNumber l.bufpos
    inc l.bufpos
  of '{':
    l.kind = tkLeftBrace
    l.startPos = l.getColNumber l.bufpos
    inc l.bufpos
  of '}':
    l.kind = tkRightBrace
    l.startPos = l.getColNumber l.bufpos
    inc l.bufpos
  of ';':
    l.kind = tkSemicolon
    l.startPos = l.getColNumber l.bufpos
    inc l.bufpos
  of ',':
    l.kind = tkComma
    l.startPos = l.getColNumber l.bufpos
    inc l.bufpos
  of '\'': handleChar l
  of '"': handleString l
  of '0'..'9': handleNumber l
  of 'a'..'z', 'A'..'Z': handleIdent l
  of EndOfFile:
    l.startPos = l.getColNumber l.bufpos
    l.kind = tkEndOfInput
  else:
    setError l, "Unrecognized character"
  result = l.kind

when isMainModule:
  import os, strformat
  proc main() =
    var l: Lexer
    if paramCount() < 1:
      open l, newFileStream stdin
    else:
      open l, newFileStream paramStr(1)
    while l.getToken notin {tkInvalid}:
      stdout.write &"{l.lineNumber:5}  {l.startPos + 1:5} {l.kind:<14}"
      if l.kind in {tkIdentifier, tkInteger, tkString}:
        stdout.write &"  {l.token}"
      stdout.write '\n'
      if l.kind == tkEndOfInput:
        break
    if hasError l:
      echo &"({l.lineNumber},{l.getColNumber l.bufpos + 1}) {l.error}"
main()

```



### Using nothing but system and strutils


```nim
import strutils

type
  TokenKind* = enum
    tokMult = "Op_multiply", tokDiv = "Op_divide", tokMod = "Op_mod",
    tokAdd = "Op_add", tokSub = "Op_subtract", tokLess = "Op_less",
    tokLessEq = "Op_lessequal", tokGreater = "Op_greater",
    tokGreaterEq = "Op_greaterequal", tokEq = "Op_equal",
    tokNotEq = "Op_notequal", tokNot = "Op_not", tokAssign = "Op_assign",
    tokAnd = "Op_and", tokOr = "Op_or"
    tokLPar = "LeftParen", tokRPar = "RightParen"
    tokLBrace = "LeftBrace", tokRBrace = "RightBrace"
    tokSemi = "Semicolon", tokComma = "Comma"
    tokIf = "Keyword_if", tokElse = "Keyword_else", tokWhile = "Keyword_while",
    tokPrint = "Keyword_print", tokPutc = "Keyword_putc"
    tokIdent = "Identifier", tokInt = "Integer", tokChar = "Integer",
    tokString = "String"
    tokEnd = "End_of_input"
  Token* = object
    ln*, col*: int
    case kind*: TokenKind
    of tokIdent: ident*: string
    of tokInt: intVal*: int
    of tokChar: charVal*: char
    of tokString: stringVal*: string
    else: discard
  Lexer* = object
    input: string
    pos: int
    ln, col: int
  LexicalError* = object of CatchableError
    ln*, col*: int

proc error(lexer: var Lexer, message: string) =
  var err = newException(LexicalError, message)
  err.ln = lexer.ln
  err.col = lexer.col

template current: char =
  if lexer.pos < lexer.input.len: lexer.input[lexer.pos]
  else: '\x00'
template get(n: int): string =
  if lexer.pos < lexer.input.len:
    lexer.input[min(lexer.pos, lexer.input.len)..
                min(lexer.pos + n - 1, lexer.input.len)]
  else: ""

template next() =
  inc(lexer.pos); inc(lexer.col)
  if current() == '\n':
    inc(lexer.ln)
    lexer.col = 0
  elif current() == '\r':
    lexer.col = 0

proc skip(lexer: var Lexer) =
  while true:
    if current() in Whitespace:
      while current() in Whitespace:
        next()
      continue
    elif get(2) == "/*":
      next(); next()
      while get(2) != "*/":
        if current() == '\x00':
          lexer.error("Unterminated comment")
        next()
      next(); next()
      continue
    else: discard
    break

proc charOrEscape(lexer: var Lexer): char =
  if current() != '\\':
    result = current()
    next()
  else:
    next()
    case current()
    of 'n': result = '\n'
    of '\\': result = '\\'
    else: lexer.error("Unknown escape sequence '\\" & current() & "'")
    next()

proc next*(lexer: var Lexer): Token =
  let
    ln = lexer.ln
    col = lexer.col

  case current()
  of '*': result = Token(kind: tokMult); next()
  of '/': result = Token(kind: tokDiv); next()
  of '%': result = Token(kind: tokMod); next()
  of '+': result = Token(kind: tokAdd); next()
  of '-': result = Token(kind: tokSub); next()
  of '<':
    next()
    if current() == '=': result = Token(kind: tokLessEq)
    else: result = Token(kind: tokLess)
  of '>':
    next()
    if current() == '=': result = Token(kind: tokGreaterEq)
    else: result = Token(kind: tokGreater)
  of '=':
    next()
    if current() == '=': result = Token(kind: tokEq)
    else: result = Token(kind: tokAssign)
  of '!':
    next()
    if current() == '=': result = Token(kind: tokNotEq)
    else: result = Token(kind: tokNot)
  of '&':
    next()
    if current() == '&': result = Token(kind: tokAnd)
    else: lexer.error("'&&' expected")
  of '|':
    next()
    if current() == '|': result = Token(kind: tokOr)
    else: lexer.error("'||' expected")
  of '(': result = Token(kind: tokLPar); next()
  of ')': result = Token(kind: tokRPar); next()
  of '{': result = Token(kind: tokLBrace); next()
  of '}': result = Token(kind: tokRBrace); next()
  of ';': result = Token(kind: tokSemi); next()
  of ',': result = Token(kind: tokComma); next()
  of '\'':
    next()
    if current() == '\'': lexer.error("Empty character literal")
    let ch = lexer.charOrEscape()
    if current() != '\'':
      lexer.error("Character literal must contain a single character or " &
                  "escape sequence")
    result = Token(kind: tokChar, charVal: ch)
  of '0'..'9':
    var number = ""
    while current() in Digits:
      number.add(current())
      next()
    if current() in IdentStartChars:
      lexer.error("Integer literal ends in non-digit characters")
    result = Token(kind: tokInt, intVal: parseInt(number))
  of '"':
    next()
    var str = ""
    while current() notin {'"', '\x00', '\n'}:
      str.add(lexer.charOrEscape())
    if current() == '\x00':
      lexer.error("Unterminated string literal")
    elif current() == '\n':
      lexer.error("Line feed in string literal")
    else:
      next()
      result = Token(kind: tokString, stringVal: str)
  of IdentStartChars:
    var ident = $current()
    next()
    while current() in IdentChars:
      ident.add(current())
      next()
    case ident
    of "if": result = Token(kind: tokIf)
    of "else": result = Token(kind: tokElse)
    of "while": result = Token(kind: tokWhile)
    of "print": result = Token(kind: tokPrint)
    of "putc": result = Token(kind: tokPutc)
    else: result = Token(kind: tokIdent, ident: ident)
  of '\x00':
    result = Token(kind: tokEnd)
  else:
    lexer.error("Unexpected character: '" & current() & "'")

  result.ln = ln
  result.col = col
  lexer.skip()

proc peek*(lexer: var Lexer): Token =
  discard

proc initLexer*(input: string): Lexer =
  result = Lexer(input: input, pos: 0, ln: 1, col: 1)
  result.skip()

when isMainModule:
  let code = readAll(stdin)
  var
    lexer = initLexer(code)
    token: Token
  while true:
    token = lexer.next()
    stdout.write(token.ln, ' ', token.col, ' ', token.kind)
    case token.kind
    of tokInt: stdout.write(' ', token.intVal)
    of tokChar: stdout.write(' ', token.charVal.ord)
    of tokString: stdout.write(" \"", token.stringVal
                    .replace("\\", "\\\\")
                    .replace("\n", "\\n"), '"')
    of tokIdent: stdout.write(' ', token.ident)
    else: discard
    stdout.write('\n')
    if token.kind == tokEnd:
      break
```



## Perl



```perl
#!/usr/bin/env perl

use strict;
use warnings;
no warnings 'once';


#----- Definition of the language to be lexed -----#

my @tokens = (
    # Name            | Format               | Value       #
    # --------------  |----------------------|-------------#
    ['Op_multiply'    , '*'                  ,             ],
    ['Op_divide'      , '/'                  ,             ],
    ['Op_mod'         , '%'                  ,             ],
    ['Op_add'         , '+'                  ,             ],
    ['Op_subtract'    , '-'                  ,             ],
    ['Op_lessequal'   , '<='                 ,             ],
    ['Op_less'        , '<'                  ,             ],
    ['Op_greaterequal', '>='                 ,             ],
    ['Op_greater'     , '>'                  ,             ],
    ['Op_equal'       , '=='                 ,             ],
    ['Op_assign'      , '='                  ,             ],
    ['Op_not'         , '!'                  ,             ],
    ['Op_notequal'    , '!='                 ,             ],
    ['Op_and'         , '&&'                 ,             ],
    ['Op_or'          , '||'                 ,             ],
    ['Keyword_else'   , qr/else\b/           ,             ],
    ['Keyword_if'     , qr/if\b/             ,             ],
    ['Keyword_while'  , qr/while\b/          ,             ],
    ['Keyword_print'  , qr/print\b/          ,             ],
    ['Keyword_putc'   , qr/putc\b/           ,             ],

    ['LeftParen'      , '('                  ,             ],
    ['RightParen'     , ')'                  ,             ],
    ['LeftBrace'      , '{'                  ,             ],
    ['RightBrace'     , '}'                  ,             ],
    ['Semicolon'      , ';'                  ,             ],
    ['Comma'          , ','                  ,             ],

    ['Identifier'     , qr/[_a-z][_a-z0-9]*/i, \&raw       ],
    ['Integer'        , qr/[0-9]+\b/         , \&raw       ],
    ['Integer'        , qr/'([^']*)(')?/     , \&char_val  ],
    ['String'         , qr/"([^"]*)(")?/     , \&string_raw],

    ['End_of_input'   , qr/$/                ,             ],
);

my $comment = qr/\/\* .+? (?: \*\/ | $ (?{die "End-of-file in comment\n"}) )/xs;
my $whitespace = qr/(?: \s | $comment)*/x;
my $unrecognized = qr/\w+ | ./x;

#| Returns the value of a matched char literal, or dies if it is invalid
sub char_val {
    my $str = string_val();
    die "Multiple characters\n" if length $str > 1;
    die "No character\n"        if length $str == 0;
    ord $str;
}

#| Returns the value of a matched string literal, or dies if it is invalid
sub string_val {
    my ($str, $end) = ($1, $2);
    die "End-of-file\n" if not defined $end;
    die "End-of-line\n" if $str =~ /\n/;
    $str =~ s/\\(.)/
          $1 eq 'n'  ? "\n"
        : $1 eq '\\' ? $1
        : $1 eq $end ? $1
        : die "Unknown escape sequence \\$1\n"
    /rge;
}

#| Returns the source string of a matched literal
sub raw { $& }

#| Returns the source string of a matched string literal, or dies if invalid
sub string_raw {
    string_val(); # Just for the error handling side-effects
    $&;
}


#----- Lexer "engine" -----#

# Construct the scanner regex:

my $tokens =
    join "|",
    map {
        my $format = $tokens[$_][1];
        "\n".(ref $format ? $format : quotemeta $format)." (*MARK:$_) ";
    } 0..$#tokens;

my $regex = qr/
    \G (?| $whitespace  \K (?| $tokens )
         | $whitespace? \K ($unrecognized) (*MARK:!) )
/x;


# Run the lexer:

my $input = do { local $/ = undef; <STDIN> };
my $pos = 0;
my $linecol = linecol_accumulator();

while ($input =~ /$regex/g) {
    # Get the line and column number
    my ($line, $col) = $linecol->(substr $input, $pos, $-[0] - $pos);
    $pos = $-[0];

    # Get the token type that was identified by the scanner regex
    my $type = $main::REGMARK;
    die "Unrecognized token $1 at line $line, col $col\n" if $type eq '!';
    my ($name, $evaluator) = @{$tokens[$type]}[0, 2];

    # Get the token value
    my $value;
    if ($evaluator) {
        eval { $value = $evaluator->() };
        if ($@) { chomp $@; die "$@ in $name at line $line, col $col\n" }
    }

    # Print the output line
    print "$line\t$col\t$name".($value ? "\t$value" : '')."\n";
}

#| Returns a closure, which can be fed a string one piece at a time and gives
#| back the cumulative line and column number each time
sub linecol_accumulator {
    my ($line, $col) = (1, 1);
    sub {
        my $str = shift;
        my @lines = split "\n", $str, -1;
        my ($l, $c) = @lines ? (@lines - 1, length $lines[-1]) : (0, 0);
        if ($l) { $line += $l;  $col = 1 + $c }
        else    { $col += $c }
        ($line, $col)
    }
}
```


{{out|case=test case 3}}

```txt

5       16      Keyword_print
5       40      Op_subtract
6       16      Keyword_putc
6       40      Op_less
7       16      Keyword_if
7       40      Op_greater
8       16      Keyword_else
8       40      Op_lessequal
9       16      Keyword_while
9       40      Op_greaterequal
10      16      LeftBrace
10      40      Op_equal
11      16      RightBrace
11      40      Op_not
11      41      Op_assign
12      16      LeftParen
12      40      Op_and
13      16      RightParen
13      40      Op_or
14      16      Op_subtract
14      40      Semicolon
15      16      Op_not
15      40      Comma
16      16      Op_multiply
16      40      Op_assign
17      16      Op_divide
17      40      Integer 42
18      16      Op_mod
18      40      String  "String literal"
19      16      Op_add
19      40      Identifier      variable_name
20      26      Integer 10
21      26      Integer 92
22      26      Integer 32
23      1       End_of_input

```



### Alternate Perl Solution

Tested on perl v5.26.1

```Perl
#!/usr/bin/perl

use strict;   # lex.pl - source to tokens
use warnings; # http://www.rosettacode.org/wiki/Compiler/lexical_analyzer
no warnings qw(qw);

my %keywords = map { $_, "Keyword_$_" } qw( while print if else putc );
my %tokens = qw[ ; Semicolon ( LeftParen ) RightParen { LeftBrace } RightBrace
  + Op_add - Op_subtract * Op_multiply % Op_mod = Op_assign >= Op_greaterequal
  != Op_notequal == Op_equal ! Op_not < Op_less <= Op_lessequal > Op_greater
  , Comma && Op_and || Op_or ];

local $_ = join '', <>;

while( /\G (?|
    \s+              (?{ undef })
  | \d+[_a-zA-Z]\w*  (?{ die "invalid mixed number $&\n" })
  | \d+              (?{ "Integer $&" })
  | \w+              (?{ $keywords{$&} || "Identifier $&" })
  | ( [-;(){}+*%,] | [=!<>]=? | && | \|\| )
                     (?{ $tokens{$1} })
  | \/               (?{ 'Op_divide' }) (?: \* (?: [\s\S]*?\*\/ (?{ undef }) |
                          (?{ die "End-of-file in comment\n" }) ) )?
  | "[^"\n]*"        (?{ "String $&" })
  | "                (?{ die "unterminated string\n" })
  | ''               (?{ die "empty character constant\n" })
  | '([^\n\\])'      (?{ 'Integer ' . ord $1 })
  | '\\n'            (?{ 'Integer 10' })
  | '\\\\'           (?{ 'Integer 92' })
  | '                (?{ die "unterminated or bad character constant\n" }) #'
  | .                (?{ die "invalid character $&\n" })
  ) /gcx )
  {
  defined $^R and printf "%5d %7d   %s\n",
    1 + $` =~ tr/\n//, 1 + length $` =~ s/.*\n//sr, $^R;
  }
printf "%5d %7d   %s\n", 1 + tr/\n//, 1, 'End_of_input';
```



## Perl 6

This is more complicated than strictly necessary for this task. It is set up to be easily adapted to do syntax analysis.

(Note: there are several bogus comments added solely to help with syntax highlighting.)

{{works with|Rakudo|2016.08}}


```perl6
grammar tiny_C {
    rule TOP { ^ <.whitespace>? <tokens> + % <.whitespace> <.whitespace> <eoi> }

    rule whitespace { [ <comment> + % <ws> | <ws> ] }

    token comment    { '/*' ~ '*/' .*? }

    token tokens {
        [
        | <operator>   { make $/<operator>.ast   }
        | <keyword>    { make $/<keyword>.ast    }
        | <symbol>     { make $/<symbol>.ast     }
        | <identifier> { make $/<identifier>.ast }
        | <integer>    { make $/<integer>.ast    }
        | <char>       { make $/<char>.ast       }
        | <string>     { make $/<string>.ast     }
        | <error>
        ]
    }

    proto token operator    {*}
    token operator:sym<*>   { '*'               { make 'Op_multiply'    } }
    token operator:sym</>   { '/'<!before '*'>  { make 'Op_divide'      } }
    token operator:sym<%>   { '%'               { make 'Op_mod'         } }
    token operator:sym<+>   { '+'               { make 'Op_add'         } }
    token operator:sym<->   { '-'               { make 'Op_subtract'    } }
    token operator:sym('<='){ '<='              { make 'Op_lessequal'   } }
    token operator:sym('<') { '<'               { make 'Op_less'        } }
    token operator:sym('>='){ '>='              { make 'Op_greaterequal'} }
    token operator:sym('>') { '>'               { make 'Op_greater'     } }
    token operator:sym<==>  { '=='              { make 'Op_equal'       } }
    token operator:sym<!=>  { '!='              { make 'Op_notequal'    } }
    token operator:sym<!>   { '!'               { make 'Op_not'         } }
    token operator:sym<=>   { '='               { make 'Op_assign'      } }
    token operator:sym<&&>  { '&&'              { make 'Op_and'         } }
    token operator:sym<||>  { '||'              { make 'Op_or'          } }

    proto token keyword      {*}
    token keyword:sym<if>    { 'if'    { make 'Keyword_if'    } }
    token keyword:sym<else>  { 'else'  { make 'Keyword_else'  } }
    token keyword:sym<putc>  { 'putc'  { make 'Keyword_putc'  } }
    token keyword:sym<while> { 'while' { make 'Keyword_while' } }
    token keyword:sym<print> { 'print' { make 'Keyword_print' } }

    proto token symbol  {*}
    token symbol:sym<(> { '(' { make 'LeftParen'  } }
    token symbol:sym<)> { ')' { make 'RightParen' } }
    token symbol:sym<{> { '{' { make 'LeftBrace'  } }
    token symbol:sym<}> { '}' { make 'RightBrace' } }
    token symbol:sym<;> { ';' { make 'Semicolon'   } }
    token symbol:sym<,> { ',' { make 'Comma'       } }

    token identifier { <[_A..Za..z]><[_A..Za..z0..9]>* { make 'Identifier ' ~ $/ } }
    token integer    { <[0..9]>+                       { make 'Integer '    ~ $/ } }

    token char {
        '\'' [<-[']> | '\n' | '\\\\'] '\''
        { make 'Char_Literal ' ~ $/.subst("\\n", "\n").substr(1, *-1).ord }
    }

    token string {
        '"' <-["\n]>* '"' #'
        {
            make 'String ' ~ $/;
            note 'Error: Unknown escape sequence.' and exit if (~$/ ~~ m:r/ <!after <[\\]>>[\\<-[n\\]>]<!before <[\\]>> /);
        }
    }

    token eoi { $ { make 'End_of_input' } }

    token error {
        | '\'''\''                   { note 'Error: Empty character constant.' and exit }
        | '\'' <-[']> ** {2..*} '\'' { note 'Error: Multi-character constant.' and exit }
        | '/*' <-[*]>* $             { note 'Error: End-of-file in comment.'   and exit }
        | '"' <-["]>* $              { note 'Error: End-of-file in string.'    and exit }
        | '"' <-["]>*? \n            { note 'Error: End of line in string.'    and exit } #'
    }
}

sub parse_it ( $c_code ) {
    my $l;
    my @pos = gather for $c_code.lines>>.chars.kv -> $line, $v {
        take [ $line + 1, $_ ] for 1 .. ($v+1); # v+1 for newline
        $l = $line+2;
    }
    @pos.push: [ $l, 1 ]; # capture eoi

    for flat $c_code<tokens>.list, $c_code<eoi> -> $m {
        say join "\t", @pos[$m.from].fmt('%3d'), $m.ast;
    }
}

my $tokenizer = tiny_C.parse(@*ARGS[0].IO.slurp);
parse_it( $tokenizer );
```


{{out|case=test case 3}}

```txt

  5  16 Keyword_print
  5  40 Op_subtract
  6  16 Keyword_putc
  6  40 Op_less
  7  16 Keyword_if
  7  40 Op_greater
  8  16 Keyword_else
  8  40 Op_lessequal
  9  16 Keyword_while
  9  40 Op_greaterequal
 10  16 LeftBrace
 10  40 Op_equal
 11  16 RightBrace
 11  40 Op_notequal
 12  16 LeftParen
 12  40 Op_and
 13  16 RightParen
 13  40 Op_or
 14  16 Op_subtract
 14  40 Semicolon
 15  16 Op_not
 15  40 Comma
 16  16 Op_multiply
 16  40 Op_assign
 17  16 Op_divide
 17  40 Integer 42
 18  16 Op_mod
 18  40 String "String literal"
 19  16 Op_add
 19  40	Identifier variable_name
 20  26	Char_Literal 10
 21  26	Char_Literal 92
 22  26	Char_Literal 32
 23   1	End_of_input

```



## Phix

Deviates from the task requirements in that it is written in a modular form so that the output
from one stage can be used directly in the next, rather than re-loading from a human-readable
form. If required, demo\rosetta\Compiler\extra.e contains some code that achieves the latter.
Code to print the human readable forms is likewise kept separate from any re-usable parts.


```Phix
--
-- demo\\rosetta\\Compiler\\core.e
--
### =========================

--
--  Standard declarations and routines used by lex.exw, parse.exw, cgen.exw, and interp.exw
--  (included in distribution as above, which contains some additional sanity checks)
--
--
global constant EOF = -1, STDIN = 0, STDOUT = 1

global enum type nary NONE=0, UNARY=1, BINARY=2 end type

global sequence tkNames = {}    -- eg/ie {"Op_multiply","Op_divide",..}
global sequence precedences = {}
global sequence narys = {}  -- NONE/UNARY/BINARY
global sequence operators = {} -- eg/ie {"*","/","+","-","<","<=",..}
global sequence opcodes = {}    -- idx to tkNames, matching operators

global constant KEYWORDS = new_dict()   -- eg/ie {"if"=>idx to tkNames}

global enum OPERATOR=1, DIGIT, LETTER   -- character classes

global sequence charmap = repeat(0,255)
                charmap['0'..'9'] = DIGIT
                charmap['A'..'Z'] = LETTER
                charmap['a'..'z'] = LETTER
                charmap['_'] = LETTER

function tkName(string s, nary n = NONE, integer precedence = -1)
    tkNames = append(tkNames,s)
    narys = append(narys,n)
    precedences = append(precedences,precedence)
    return length(tkNames)
end function

function tkOp(string s, string op, nary n, integer precedence)
    integer res = tkName(s, n, precedence)
    operators = append(operators,op)
    opcodes = append(opcodes,res)
    for i=1 to length(op) do
        charmap[op[i]] = OPERATOR
    end for
    return res
end function

function tkKw(string s, string keyword)
    integer res = tkName(s)
    putd(keyword, res, KEYWORDS)
    return res
end function

global constant
    tk_EOI           = tkName("End_of_input"),                      --1
    tk_mul           = tkOp("Op_multiply",      "*", BINARY,13),    --2
    tk_div           = tkOp("Op_divide",        "/", BINARY,13),    --3
    tk_mod           = tkOp("Op_mod",           "%", BINARY,13),    --4
    tk_add           = tkOp("Op_add",           "+", BINARY,12),    --5
    tk_sub           = tkOp("Op_subtract",      "-", BINARY,12),    --6
    tk_neg           = tkName("Op_negate",           UNARY, 14),    --7
    tk_not           = tkOp("Op_not",           "!", UNARY, 14),    --8
    tk_lt            = tkOp("Op_less",          "<", BINARY,10),    --9
    tk_le            = tkOp("Op_lessequal",     "<=",BINARY,10),    --10
    tk_gt            = tkOp("Op_greater",       ">", BINARY,10),    --11
    tk_ge            = tkOp("Op_greaterequal",  ">=",BINARY,10),    --12
    tk_eq            = tkOp("Op_equal",         "==",BINARY, 9),    --13
    tk_ne            = tkOp("Op_notequal",      "!=",BINARY, 9),    --14
    tk_assign        = tkOp("Op_assign",        "=", NONE,  -1),    --15
    tk_and           = tkOp("Op_and",           "&&",BINARY, 5),    --16
    tk_or            = tkOp("Op_or",            "||",BINARY, 4),    --17
    tk_if            = tkKw("Keyword_if",   "if"),                  --18
    tk_else          = tkKw("Keyword_else", "else"),                --19
    tk_while         = tkKw("Keyword_while","while"),               --20
    tk_print         = tkKw("Keyword_print","print"),               --21
    tk_putc          = tkKw("Keyword_putc", "putc"),                --22
    tk_LeftParen     = tkOp("LeftParen",        "(", NONE,  -1),    --23
    tk_RightParen    = tkOp("RightParen",       ")", NONE,  -1),    --24
    tk_LeftBrace     = tkOp("LeftBrace",        "{", NONE,  -1),    --25
    tk_RightBrace    = tkOp("RightBrace",       "}", NONE,  -1),    --26
    tk_Semicolon     = tkOp("Semicolon",        ";", NONE,  -1),    --27
    tk_Comma         = tkOp("Comma",            ",", NONE,  -1),    --28
    tk_Identifier    = tkName("Identifier"),                        --29
    tk_Integer       = tkName("Integer"),                           --30
    tk_String        = tkName("String"),                            --31
    tk_Sequence      = tkName("Sequence"),                          --32
    tk_Prints        = tkName("tk_Prints"),                         --33
    tk_Printi        = tkName("tk_Printi")                          --34

global integer input_file = STDIN,
               output_file = STDOUT

type strint(object o)
    return string(o) or integer(o)
end type

global strint tok_line, -- save of line/col at the start of
              tok_col   -- token/comment, for result/errors

global object oneline = ""

constant errfmt = "Line %s column %s:\n%s%s"

function errline()
    oneline = substitute(trim(oneline,"\r\n"),"\t"," ")
    string padding = repeat(' ',tok_col)
    return sprintf("%s\n%s^ ",{oneline,padding})
end function

global procedure error(sequence msg, sequence args={})
    if length(args) then
        msg = sprintf(msg,args)
    end if
    string el = iff(atom(oneline)?"":errline())
    if integer(tok_line) then tok_line = sprintf("%d",tok_line) end if
    if integer(tok_col) then tok_col = sprintf("%d",tok_col) end if
    printf(STDOUT,errfmt,{tok_line,tok_col,el,msg})
    {} = wait_key()
    abort(1)
end procedure

function open_file(string file_name, string mode)
    integer fn = open(file_name, mode)
    if fn = -1 then
        printf(STDOUT, "Could not open %s", {file_name})
        {} = wait_key()
        abort(1)
    end if
    return fn
end function

global procedure open_files(sequence cl)
    if length(cl)>2 then
        input_file = open_file(cl[3],"r")
        if length(cl)>3 then
            output_file = open_file(cl[4],"w")
        end if
    end if
end procedure

global procedure close_files()
    if input_file!=STDIN then close(input_file) end if
    if output_file!=STDOUT then close(output_file) end if
end procedure

global function enquote(string s)
    return sprintf("\"%s\"",substitute(s,"\n","\\n"))
end function

global function unquote(string s)
    if s[1]!='\"' then ?9/0 end if
    if s[$]!='\"' then ?9/0 end if
    s = substitute(s[2..-2],"\\n","\n")
    return s
end function
```

The main lexer is also written to be reusable by later stages.

```Phix
--
-- demo\\rosetta\\Compiler\\lex.e
--
### ========================

--
--  The reusable part of lex.exw
--  This is only kept separate from core.e for consistency with later modules.

include core.e

integer ch = ' ',
        line = 0,
        col = 0

procedure eof(string s)
    error("%s in %s literal",{iff(ch=EOF?"EOF":"EOL"),s})
end procedure

function next_ch()
    while 1 do
        col += 1
        if oneline=EOF then
            ch = EOF
            exit
        elsif col>length(oneline) then
            line += 1
            col = 0
            oneline = gets(input_file)
        else
            ch = oneline[col]
            exit
        end if
    end while
    return ch
end function

constant whitespace = " \t\r\n\x0B\xA0"
-- (0x0B is Vertical Tab, 0xA0 is Non-breaking space)

procedure skipspacesandcomments()
    while 1 do
        if not find(ch,whitespace) then
            if ch='/' and col<length(oneline) and oneline[col+1]='*' then
                tok_line = line -- (in case of EOF error)
                tok_col = col
                ch = next_ch()  -- (can be EOF)
                ch = next_ch()  -- (    ""    )
                while 1 do
                    if ch='*' then
                        ch = next_ch()
                        if ch='/' then exit end if
                    elsif ch=EOF then
                        error("EOF in comment")
                    else
                        ch = next_ch()
                    end if
                end while
            else
                exit
            end if
        end if
        ch = next_ch()
    end while
end procedure

function escape_char(string s)
    ch = next_ch() -- (discard the '\\')
    if ch='n' then
        ch = '\n'
    elsif ch='\\' then
        ch = '\\'
    elsif ch=EOF
       or ch='\n' then
        eof(s)
    else
        error("unknown escape sequence \\%c", {ch})
    end if
    return ch
end function

function char_lit()
integer startch = ch
integer res = next_ch() -- (skip opening quote, save res)
    if ch=startch then
        error("empty character constant")
    elsif ch='\\' then
        res = escape_char("character")
    end if
    ch = next_ch()
    if ch=EOF
    or ch='\n' then
        eof("character")
    elsif ch!=startch then
        error("multi-character constant")
    end if
    ch = next_ch()
    return {tk_Integer, res}
end function

function string_lit()
integer startch = ch
string text = ""
    while next_ch()!=startch do
        if ch=EOF
        or ch='\n' then
            eof("string")
        elsif ch='\\' then
            ch = escape_char("string")
        end if
        text &= ch
    end while
    ch = next_ch()
    return {tk_String, text}
end function

function op()
sequence operator = {ch}
    ch = next_ch()
    while charmap[ch]=OPERATOR
      and find(operator&ch,operators) do
        -- (^ ie/eg merge ">=", but not ");")
        operator &= ch
        ch = next_ch()
    end while
    integer k = find(operator,operators)
    if k=0 then error("unknown operator") end if
    return {opcodes[k], 0} -- (0 unused)
end function

function int()
integer i = 0
    while charmap[ch]=DIGIT do
        i = i*10 + (ch-'0')
        ch = next_ch()
    end while
    if charmap[ch]=LETTER then
        error("invalid number")
    end if
    return {tk_Integer, i}
end function

function ident()
string text = ""
    while find(charmap[ch],{LETTER,DIGIT}) do
        text &= ch
        ch = next_ch()
    end while
    integer keyword = getd(text,KEYWORDS)
    if keyword!=NULL then
        return {keyword, 0} -- (0 unused)
    end if
    return {tk_Identifier, text}
end function

function get_tok()
    skipspacesandcomments()
    tok_line = line
    tok_col  = col
    switch ch do
        case EOF  then return {tk_EOI, 0} -- (0 unused)
        case '\'' then return char_lit()
        case '"'  then return string_lit()
        else
            switch charmap[ch] do
                case OPERATOR then return op()
                case DIGIT then return int()
                case LETTER then return ident()
                else error("unrecognized character: (%d)", {ch})
            end switch
    end switch
end function

global function lex()
sequence toks = {}
    integer tok = -1
    object v
    while tok!=tk_EOI do
        {tok,v} = get_tok()
        toks = append(toks,{tok_line,tok_col,tok,v})
    end while
    return toks
end function
```

Finally, a simple test driver for the specific task:

```Phix
--
-- demo\\rosetta\\Compiler\\lex.exw
--
### ==========================

--

include lex.e

procedure main(sequence cl)
    open_files(cl)
    sequence toks = lex()
    integer tok
    object v
    for i=1 to length(toks) do
        {tok_line,tok_col,tok,v} = toks[i]
        switch tok do
            case tk_Identifier: v = sprintf(" %s",v)
            case tk_Integer:    v = sprintf(" %5d",v)
            case tk_String:     v = sprintf(" %s",enquote(v))
            else                v = ""
        end switch
        printf(output_file, "%5d  %5d %-10s%s\n", {tok_line,tok_col,tkNames[tok],v})
    end for
    close_files()
end procedure

--main(command_line())
main({0,0,"test4.c"})
```

{{out}}

```txt

    2      1 Keyword_print
    2      6 LeftParen
    2      7 Integer       42
    2      9 RightParen
    2     10 Semicolon
    3      1 Keyword_print
    3      6 LeftParen
    3      7 String     "\nHello World\nGood Bye\nok\n"
    3     38 RightParen
    3     39 Semicolon
    4      1 Keyword_print
    4      6 LeftParen
    4      7 String     "Print a slash n - \n.\n"
    4     33 RightParen
    4     34 Semicolon
    5      1 End_of_input

```



## Python

Tested with Python 2.7 and 3.x

```Python
from __future__ import print_function
import sys

# following two must remain in the same order

tk_EOI, tk_Mul, tk_Div, tk_Mod, tk_Add, tk_Sub, tk_Negate, tk_Not, tk_Lss, tk_Leq, tk_Gtr, \
tk_Geq, tk_Eq, tk_Neq, tk_Assign, tk_And, tk_Or, tk_If, tk_Else, tk_While, tk_Print,       \
tk_Putc, tk_Lparen, tk_Rparen, tk_Lbrace, tk_Rbrace, tk_Semi, tk_Comma, tk_Ident,          \
tk_Integer, tk_String = range(31)

all_syms = ["End_of_input", "Op_multiply", "Op_divide", "Op_mod", "Op_add", "Op_subtract",
    "Op_negate", "Op_not", "Op_less", "Op_lessequal", "Op_greater", "Op_greaterequal",
    "Op_equal", "Op_notequal", "Op_assign", "Op_and", "Op_or", "Keyword_if",
    "Keyword_else", "Keyword_while", "Keyword_print", "Keyword_putc", "LeftParen",
    "RightParen", "LeftBrace", "RightBrace", "Semicolon", "Comma", "Identifier",
    "Integer", "String"]

# single character only symbols
symbols = { '{': tk_Lbrace, '}': tk_Rbrace, '(': tk_Lparen, ')': tk_Rparen, '+': tk_Add, '-': tk_Sub,
    '*': tk_Mul, '%': tk_Mod, ';': tk_Semi, ',': tk_Comma }

key_words = {'if': tk_If, 'else': tk_Else, 'print': tk_Print, 'putc': tk_Putc, 'while': tk_While}

the_ch = " "    # dummy first char - but it must be a space
the_col = 0
the_line = 1
input_file = None

#*** show error and exit
def error(line, col, msg):
    print(line, col, msg)
    exit(1)

#*** get the next character from the input
def next_ch():
    global the_ch, the_col, the_line

    the_ch = input_file.read(1)
    the_col += 1
    if the_ch == '\n':
        the_line += 1
        the_col = 0
    return the_ch

#*** 'x' - character constants
def char_lit(err_line, err_col):
    n = ord(next_ch())              # skip opening quote
    if the_ch == '\'':
        error(err_line, err_col, "empty character constant")
    elif the_ch == '\\':
        next_ch()
        if the_ch == 'n':
            n = 10
        elif the_ch == '\\':
            n = ord('\\')
        else:
            error(err_line, err_col, "unknown escape sequence \\%c" % (the_ch))
    if next_ch() != '\'':
        error(err_line, err_col, "multi-character constant")
    next_ch()
    return tk_Integer, err_line, err_col, n

#*** process divide or comments
def div_or_cmt(err_line, err_col):
    if next_ch() != '*':
        return tk_Div, err_line, err_col

    # comment found
    next_ch()
    while True:
        if the_ch == '*':
            if next_ch() == '/':
                next_ch()
                return gettok()
        elif len(the_ch) == 0:
            error(err_line, err_col, "EOF in comment")
        else:
            next_ch()

#*** "string"
def string_lit(start, err_line, err_col):
    text = ""

    while next_ch() != start:
        if len(the_ch) == 0:
            error(err_line, err_col, "EOF while scanning string literal")
        if the_ch == '\n':
            error(err_line, err_col, "EOL while scanning string literal")
        text += the_ch

    next_ch()
    return tk_String, err_line, err_col, text

#*** handle identifiers and integers
def ident_or_int(err_line, err_col):
    is_number = True
    text = ""

    while the_ch.isalnum() or the_ch == '_':
        text += the_ch
        if not the_ch.isdigit():
            is_number = False
        next_ch()

    if len(text) == 0:
        error(err_line, err_col, "ident_or_int: unrecognized character: (%d) '%c'" % (ord(the_ch), the_ch))

    if text[0].isdigit():
        if not is_number:
            error(err_line, err_col, "invalid number: %s" % (text))
        n = int(text)
        return tk_Integer, err_line, err_col, n

    if text in key_words:
        return key_words[text], err_line, err_col

    return tk_Ident, err_line, err_col, text

#*** look ahead for '>=', etc.
def follow(expect, ifyes, ifno, err_line, err_col):
    if next_ch() == expect:
        next_ch()
        return ifyes, err_line, err_col

    if ifno == tk_EOI:
        error(err_line, err_col, "follow: unrecognized character: (%d) '%c'" % (ord(the_ch), the_ch))

    return ifno, err_line, err_col

#*** return the next token type
def gettok():
    while the_ch.isspace():
        next_ch()

    err_line = the_line
    err_col  = the_col

    if len(the_ch) == 0:    return tk_EOI, err_line, err_col
    elif the_ch == '/':     return div_or_cmt(err_line, err_col)
    elif the_ch == '\'':    return char_lit(err_line, err_col)
    elif the_ch == '<':     return follow('=', tk_Leq, tk_Lss,    err_line, err_col)
    elif the_ch == '>':     return follow('=', tk_Geq, tk_Gtr,    err_line, err_col)
    elif the_ch == '=':     return follow('=', tk_Eq,  tk_Assign, err_line, err_col)
    elif the_ch == '!':     return follow('=', tk_Neq, tk_Not,    err_line, err_col)
    elif the_ch == '&':     return follow('&', tk_And, tk_EOI,    err_line, err_col)
    elif the_ch == '|':     return follow('|', tk_Or,  tk_EOI,    err_line, err_col)
    elif the_ch == '"':     return string_lit(the_ch, err_line, err_col)
    elif the_ch in symbols:
        sym = symbols[the_ch]
        next_ch()
        return sym, err_line, err_col
    else: return ident_or_int(err_line, err_col)

#*** main driver
input_file = sys.stdin
if len(sys.argv) > 1:
    try:
        input_file = open(sys.argv[1], "r", 4096)
    except IOError as e:
        error(0, 0, "Can't open %s" % sys.argv[1])

while True:
    t = gettok()
    tok  = t[0]
    line = t[1]
    col  = t[2]

    print("%5d  %5d   %-14s" % (line, col, all_syms[tok]), end='')

    if tok == tk_Integer:  print("   %5d" % (t[3]))
    elif tok == tk_Ident:  print("  %s" %   (t[3]))
    elif tok == tk_String: print('  "%s"' % (t[3]))
    else:                  print("")

    if tok == tk_EOI:
        break
```


{{out|case=test case 3}}
<b>

```txt

    5     16   Keyword_print
    5     40   Op_subtract
    6     16   Keyword_putc
    6     40   Op_less
    7     16   Keyword_if
    7     40   Op_greater
    8     16   Keyword_else
    8     40   Op_lessequal
    9     16   Keyword_while
    9     40   Op_greaterequal
   10     16   LeftBrace
   10     40   Op_equal
   11     16   RightBrace
   11     40   Op_notequal
   12     16   LeftParen
   12     40   Op_and
   13     16   RightParen
   13     40   Op_or
   14     16   Op_subtract
   14     40   Semicolon
   15     16   Op_not
   15     40   Comma
   16     16   Op_multiply
   16     40   Op_assign
   17     16   Op_divide
   17     40   Integer             42
   18     16   Op_mod
   18     40   String          "String literal"
   19     16   Op_add
   19     40   Identifier      variable_name
   20     26   Integer             10
   21     26   Integer             92
   22     26   Integer             32
   23      1   End_of_input

```

</b>


## Racket


```racket

#lang racket
(require parser-tools/lex)

(define-lex-abbrevs
  [letter         (union (char-range #\a #\z) (char-range #\A #\Z))]
  [digit          (char-range #\0 #\9)]
  [underscore     #\_]
  [identifier     (concatenation (union letter underscore)
                                 (repetition 0 +inf.0 (union letter digit underscore)))]
  [integer        (repetition 1 +inf.0 digit)]
  [char-content   (char-complement (char-set "'\n"))]
  [char-literal   (union (concatenation #\' char-content #\')
                         "'\\n'" "'\\\\'")]
  [string-content (union (char-complement (char-set "\"\n")))]
  [string-literal (union (concatenation #\" (repetition 0 +inf.0 string-content) #\")
                         "\"\\n\"" "\"\\\\\"")]
  [keyword        (union "if" "else" "while" "print" "putc")]
  [operator       (union "*" "/" "%" "+" "-" "-"
                         "<" "<=" ">" ">=" "==" "!="
                         "!" "=" "&&" "||")]
  [symbol         (union "(" ")" "{" "}" ";" ",")]
  [comment        (concatenation "/*" (complement (concatenation any-string "*/" any-string)) "*/")])

(define operators-ht
  (hash "*"  'Op_multiply "/"  'Op_divide    "%" 'Op_mod      "+"  'Op_add           "-"  'Op_subtract
        "<"  'Op_less     "<=" 'Op_lessequal ">" 'Op_greater  ">=" 'Op_greaterequal "==" 'Op_equal
        "!=" 'Op_notequal "!"  'Op_not       "=" 'Op_assign   "&&" 'Op_and          "||" 'Op_or))

(define symbols-ht
  (hash "(" 'LeftParen  ")" 'RightParen
        "{" 'LeftBrace  "}" 'RightBrace
        ";" 'Semicolon  "," 'Comma))

(define (lexeme->keyword  l) (string->symbol (~a "Keyword_" l)))
(define (lexeme->operator l) (hash-ref operators-ht l))
(define (lexeme->symbol   l) (hash-ref symbols-ht   l))
(define (lexeme->char     l) (match l
                               ["'\\\\'" #\\]
                               ["'\\n'"  #\newline]
                               [_       (string-ref l 1)]))

(define (token name [value #f])
  (cons name (if value (list value) '())))

(define (lex ip)
  (port-count-lines! ip)
  (define my-lexer
    (lexer-src-pos
     [integer        (token 'Integer (string->number lexeme))]
     [char-literal   (token 'Integer (char->integer (lexeme->char lexeme)))]
     [string-literal (token 'String  lexeme)]
     [keyword        (token (lexeme->keyword  lexeme))]
     [operator       (token (lexeme->operator lexeme))]
     [symbol         (token (lexeme->symbol   lexeme))]
     [comment        #f]
     [whitespace     #f]
     [identifier     (token 'Identifier lexeme)]
     [(eof)          (token 'End_of_input)]))
  (define (next-token) (my-lexer ip))
  next-token)

(define (string->tokens s)
  (port->tokens (open-input-string s)))

(define (port->tokens ip)
  (define next-token (lex ip))
  (let loop ()
    (match (next-token)
      [(position-token t (position offset line col) _)
       (set! col (+ col 1)) ; output is 1-based
       (match t
         [#f                   (loop)] ; skip whitespace/comments
         [(list 'End_of_input) (list (list line col 'End_of_input))]
         [(list name value)    (cons (list line col name value) (loop))]
         [(list name)          (cons (list line col name)       (loop))]
         [_ (error)])])))

(define test1 #<<TEST
/*
  Hello world
 */
print("Hello, World!\n");

TEST
)

(define test2 #<<TEST
/*
  Show Ident and Integers
 */
phoenix_number = 142857;
print(phoenix_number, "\n");

TEST
  )

(define test3 #<<TEST
/*
  All lexical tokens - not syntactically correct, but that will
  have to wait until syntax analysis
 */
/* Print   */  print    /* Sub     */  -
/* Putc    */  putc     /* Lss     */  <
/* If      */  if       /* Gtr     */  >
/* Else    */  else     /* Leq     */  <=
/* While   */  while    /* Geq     */  >=
/* Lbrace  */  {        /* Eq      */  ==
/* Rbrace  */  }        /* Neq     */  !=
/* Lparen  */  (        /* And     */  &&
/* Rparen  */  )        /* Or      */  ||
/* Uminus  */  -        /* Semi    */  ;
/* Not     */  !        /* Comma   */  ,
/* Mul     */  *        /* Assign  */  =
/* Div     */  /        /* Integer */  42
/* Mod     */  %        /* String  */  "String literal"
/* Add     */  +        /* Ident   */  variable_name
/* character literal */  '\n'
/* character literal */  '\\'
/* character literal */  ' '
TEST
  )

(define test4 #<<TEST
/*** test printing, embedded \n and comments with lots of '*' ***/
print(42);
print("\nHello World\nGood Bye\nok\n");
print("Print a slash n - \\n.\n");
TEST
  )

(define test5 #<<TEST
count = 1;
while (count < 10) {
    print("count is: ", count, "\n");
    count = count + 1;
}
TEST
  )

(define (display-tokens ts)
  (for ([t ts])
    (for ([x t])
      (display x) (display "\t\t"))
    (newline)))

"TEST 1"
(display-tokens (string->tokens test1))
"TEST 2"
(display-tokens (string->tokens test2))
"TEST 3"
(display-tokens (string->tokens test3))
"TEST 4"
(display-tokens (string->tokens test4))
"TEST 5"
(display-tokens (string->tokens test5))

```



## Scheme



```scheme

(import (scheme base)
        (scheme char)
        (scheme file)
        (scheme process-context)
        (scheme write))

(define *symbols* (list (cons #\( 'LeftParen)
                        (cons #\) 'RightParen)
                        (cons #\{ 'LeftBrace)
                        (cons #\} 'RightBrace)
                        (cons #\; 'Semicolon)
                        (cons #\, 'Comma)
                        (cons #\* 'Op_multiply)
                        (cons #\/ 'Op_divide)
                        (cons #\% 'Op_mod)
                        (cons #\+ 'Op_add)
                        (cons #\- 'Op_subtract)))

(define *keywords* (list (cons 'if 'Keyword_if)
                         (cons 'else 'Keyword_else)
                         (cons 'while 'Keyword_while)
                         (cons 'print 'Keyword_print)
                         (cons 'putc 'Keyword_putc)))

;; return list of tokens from current port
(define (read-tokens)
  ; information on position in input
  (define line 1)
  (define col 0)
  (define next-char #f)
  ; get char, updating line/col posn
  (define (get-next-char)
    (if (char? next-char) ; check for returned character
      (let ((c next-char))
        (set! next-char #f)
        c)
      (let ((c (read-char)))
        (cond ((and (not (eof-object? c))
                    (char=? c #\newline))
               (set! col 0)
               (set! line (+ 1 line))
               (get-next-char))
              (else
                (set! col (+ 1 col))
                c)))))
  (define (push-char c)
    (set! next-char c))
  ; step over any whitespace or comments
  (define (skip-whitespace+comment)
    (let loop ()
      (let ((c (get-next-char)))
        (cond ((eof-object? c)
               '())
              ((char-whitespace? c) ; ignore whitespace
               (loop))
              ((char=? c #\/) ; check for comments
               (if (char=? (peek-char) #\*) ; found start of comment
                 (begin ; eat comment
                   (get-next-char)
                   (let m ((c (get-next-char)))
                     (cond ((eof-object? c)
                            (error "End of file in comment"))
                           ((and (char=? c #\*)
                                 (char=? (peek-char) #\/))
                            (get-next-char)) ; eat / and end
                           (else
                             (m (get-next-char)))))
                   (loop)) ; continue looking for whitespace / more comments
                 (push-char #\/))) ; not comment, so put / back and return
              (else ; return to stream, as not a comment or space char
                (push-char c))))))
  ; read next token from input
  (define (next-token)
    (define (read-string) ; returns string value along with " "  marks
      (let loop ((chars '(#\"))) ; " (needed to appease Rosetta code's highlighter)
        (cond ((eof-object? (peek-char))
               (error "End of file while scanning string literal."))
              ((char=? (peek-char) #\newline)
               (error "End of line while scanning string literal."))
              ((char=? (peek-char) #\") ; "
               (get-next-char) ; consume the final quote
               (list->string (reverse (cons #\" chars)))) ; "  highlighter)
              (else
                (loop (cons (get-next-char) chars))))))
    (define (read-identifier initial-c) ; returns identifier as a Scheme symbol
      (do ((chars (list initial-c) (cons c chars))
           (c (get-next-char) (get-next-char)))
        ((or (eof-object? c) ; finish when hit end of file
             (not (or (char-numeric? c) ; or a character not permitted in an identifier
                      (char-alphabetic? c)
                      (char=? c #\_))))
         (push-char c) ; return last character to stream
         (string->symbol (list->string (reverse chars))))))
    (define (read-number initial-c) ; returns integer read as a Scheme integer
      (let loop ((res (digit-value initial-c))
                 (c (get-next-char)))
        (cond ((char-alphabetic? c)
               (error "Invalid number - ends in alphabetic chars"))
              ((char-numeric? c)
               (loop (+ (* res 10) (digit-value c))
                     (get-next-char)))
              (else
                (push-char c) ; return non-number to stream
                res))))
    ; select op symbol based on if there is a following = sign
    (define (check-eq-extend start-line start-col opeq op)
      (if (char=? (peek-char) #\=)
        (begin (get-next-char) ; consume it
               (list start-line start-col opeq))
        (list start-line start-col op)))
    ;
    (let* ((start-line line)   ; save start position of tokens
           (start-col col)
           (c (get-next-char)))
      (cond ((eof-object? c)
             (list start-line start-col 'End_of_input))
            ((char-alphabetic? c) ; read an identifier
             (let ((id (read-identifier c)))
               (if (assq id *keywords*) ; check if identifier is a keyword
                 (list start-line start-col (cdr (assq id *keywords*)))
                 (list start-line start-col 'Identifier id))))
            ((char-numeric? c) ; read a number
             (list start-line start-col 'Integer (read-number c)))
            (else
              (case c
                ((#\( #\) #\{ #\} #\; #\, #\* #\/ #\% #\+ #\-)
                 (list start-line start-col (cdr (assq c *symbols*))))
                ((#\<)
                 (check-eq-extend start-line start-col 'Op_lessequal 'Op_less))
                ((#\>)
                 (check-eq-extend start-line start-col 'Op_greaterequal 'Op_greater))
                ((#\=)
                 (check-eq-extend start-line start-col 'Op_equal 'Op_assign))
                ((#\!)
                 (check-eq-extend start-line start-col 'Op_notequal 'Op_not))
                ((#\& #\|)
                 (if (char=? (peek-char) c) ; looks for && or ||
                   (begin (get-next-char) ; consume second character if valid
                          (list start-line start-col
                                (if (char=? c #\&) 'Op_and 'Op_or)))
                   (push-char c)))
                ((#\") ; "
                 (list start-line start-col 'String (read-string)))
                ((#\')
                 (let* ((c1 (get-next-char))
                        (c2 (get-next-char)))
                   (cond ((or (eof-object? c1)
                              (eof-object? c2))
                          (error "Incomplete character constant"))
                         ((char=? c1 #\')
                          (error "Empty character constant"))
                         ((and (char=? c2 #\') ; case of single character
                               (not (char=? c1 #\\)))
                          (list start-line start-col 'Integer (char->integer c1)))
                         ((and (char=? c1 #\\) ; case of escaped character
                               (char=? (peek-char) #\'))
                          (get-next-char) ; consume the ending '
                          (cond ((char=? c2 #\n)
                                 (list start-line start-col 'Integer 10))
                                ((char=? c2 #\\)
                                 (list start-line start-col 'Integer (char->integer c2)))
                                (else
                                  (error "Unknown escape sequence"))))
                         (else
                           (error "Multi-character constant")))))
                (else
                  (error "Unrecognised character")))))))
  ;
  (let loop ((tokens '())) ; loop, ignoring space/comments, while reading tokens
    (skip-whitespace+comment)
    (let ((tok (next-token)))
      (if (eof-object? (peek-char)) ; check if at end of input
        (reverse (cons tok tokens))
        (loop (cons tok tokens))))))

(define (lexer filename)
  (with-input-from-file filename
                        (lambda () (read-tokens))))

;; output tokens to stdout, tab separated
;; line number, column number, token type, optional value
(define (display-tokens tokens)
  (for-each
    (lambda (token)
      (display (list-ref token 0))
      (display #\tab) (display (list-ref token 1))
      (display #\tab) (display (list-ref token 2))
      (when (= 4 (length token))
        (display #\tab) (display (list-ref token 3)))
      (newline))
    tokens))

;; read from filename passed on command line
(if (= 2 (length (command-line)))
  (display-tokens (lexer (cadr (command-line))))
  (display "Error: provide program filename\n"))

```


{{out}}
Output shown for "hello.c" example.  Tested against all programs in [[Compiler/Sample programs]].


```txt
4	1	Keyword_print
4	6	LeftParen
4	7	String	"Hello, World!\n"
4	24	RightParen
4	25	Semicolon
5	1	End_of_input

```

