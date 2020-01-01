+++
title = "Parsing/RPN to infix conversion"
description = ""
date = 2019-06-29T16:29:28Z
aliases = []
[extra]
id = 10993
[taxonomies]
categories = []
tags = []
+++

{{clarified-review}}
{{task}}

;Task:
Create a program that takes an [[wp:Reverse Polish notation|RPN]] representation of an expression formatted as a space separated sequence of tokens and generates the equivalent expression in [[wp:Infix notation|infix notation]].

* Assume an input of a correct, space separated, string of tokens
* Generate a space separated output string representing the same expression in infix notation
* Show how the major datastructure of your algorithm changes with each new token parsed.
* Test with the following input RPN strings then print and display the output here.
:{| class="wikitable"
! RPN input !! sample output
|- || align="center"
| <code>3 4 2 * 1 5 - 2 3 ^ ^ / +</code>|| <code>3 + 4 * 2 / ( 1 - 5 ) ^ 2 ^ 3</code>
|- || align="center"
| <code>1 2 + 3 4 + ^ 5 6 + ^</code>|| <code>( ( 1 + 2 ) ^ ( 3 + 4 ) ) ^ ( 5 + 6 )</code>
|}

* Operator precedence and operator associativity is given in this table:
:{| class="wikitable"

! operator !! [[wp:Order_of_operations|precedence]] !! [[wp:Operator_associativity|associativity]] !! operation
|- || align="center"
|                   <big><big> ^ </big></big>  ||  4  ||  right  ||  exponentiation
|- || align="center"
|                   <big><big> * </big></big>  ||  3  ||   left  ||  multiplication
|- || align="center"
|                   <big><big> / </big></big>  ||  3  ||   left  ||     division
|- || align="center"
|                   <big><big> + </big></big>  ||  2  ||   left  ||     addition
|- || align="center"
|                   <big><big> - </big></big>  ||  2  ||   left  ||   subtraction
|}


;See also:
*   [[Parsing/Shunting-yard algorithm]]   for a method of generating an RPN from an infix expression.
*   [[Parsing/RPN calculator algorithm]]   for a method of calculating a final value from this output RPN expression.
*   [http://www.rubyquiz.com/quiz148.html Postfix to infix]   from the RubyQuiz site.





## Ada

Using the solution of the task [[stack]]:

```Ada

   type Priority is range 1..4;
   type Infix is record
      Precedence : Priority;
      Expression : Unbounded_String;
   end record;
   package Expression_Stack is new Generic_Stack (Infix);
   use Expression_Stack;

   function Convert (RPN : String) return String is
      Arguments : Stack;
      procedure Pop
                (  Operation   : Character;
                   Precedence  : Priority;
                   Association : Priority
                )  is
         Right, Left : Infix;
         Result      : Infix;
      begin
         Pop (Right, Arguments);
         Pop (Left,  Arguments);
         Result.Precedence := Association;
         if Left.Precedence < Precedence then
            Append (Result.Expression, '(');
            Append (Result.Expression, Left.Expression);
            Append (Result.Expression, ')');
         else
            Append (Result.Expression, Left.Expression);
         end if;
         Append (Result.Expression, ' ');
         Append (Result.Expression, Operation);
         Append (Result.Expression, ' ');
         if Right.Precedence < Precedence then
            Append (Result.Expression, '(');
            Append (Result.Expression, Right.Expression);
            Append (Result.Expression, ')');
         else
            Append (Result.Expression, Right.Expression);
         end if;
         Push (Result, Arguments);
      end Pop;
      Pointer : Integer := RPN'First;
   begin
      while Pointer <= RPN'Last loop
         case RPN (Pointer) is
            when ' ' =>
               Pointer := Pointer + 1;
            when '0'..'9' =>
               declare
                  Start : constant Integer := Pointer;
               begin
                  loop
                     Pointer := Pointer + 1;
                     exit when Pointer > RPN'Last
                       or else RPN (Pointer) not in '0'..'9';
                  end loop;
                  Push
                  (  (  4,
                        To_Unbounded_String (RPN (Start..Pointer - 1))
                     ),
                     Arguments
                  );
               end;
            when '+' | '-' =>
               Pop (RPN (Pointer), 1, 1);
               Pointer := Pointer + 1;
            when '*' | '/' =>
               Pop (RPN (Pointer), 2, 2);
               Pointer := Pointer + 1;
            when '^' =>
               Pop (RPN (Pointer), 4, 3);
               Pointer := Pointer + 1;
            when others =>
               raise Constraint_Error with "Syntax";
         end case;
      end loop;
      declare
         Result : Infix;
      begin
         Pop (Result, Arguments);
         return To_String (Result.Expression);
      end;
   end Convert;

```

The test program:

```Ada

with Ada.Strings.Unbounded;  use Ada.Strings.Unbounded;
with Ada.Text_IO;            use Ada.Text_IO;
with Generic_Stack;

procedure RPN_to_Infix is
   -- The code above
begin
   Put_Line ("3 4 2 * 1 5 - 2 3 ^ ^ / + = ");
   Put_Line (Convert ("3 4 2 * 1 5 - 2 3 ^ ^ / +"));
   Put_Line ("1 2 + 3 4 + ^ 5 6 + ^ = ");
   Put_Line (Convert ("1 2 + 3 4 + ^ 5 6 + ^"));
end RPN_to_Infix;

```

should produce the following output

```txt

3 4 2 * 1 5 - 2 3 ^ ^ / + =
3 + 4 * 2 / (1 - 5) ^ (2 ^ 3)
1 2 + 3 4 + ^ 5 6 + ^ =
((1 + 2) ^ (3 + 4)) ^ (5 + 6)

```



## ALGOL 68

{{works with|ALGOL 68G|Any - tested with release 2.8.win32}}
Recursively parses the RPN string backwards to build a parse tree which is then printed.

```algol68

# rpn to infix - parses an RPN expression and generates the equivalent        #
#               infix expression                                              #
PROC rpn to infix = ( STRING rpn )STRING:
BEGIN

    # we parse the string backwards using recursive descent                   #
    INT    rpn pos   := UPB rpn;
    BOOL   had error := FALSE;

    # mode to hold nodes of the parse tree                                    #
    MODE NODE = STRUCT( INT op
                      , UNION( REF NODE, STRING ) left
                      , REF NODE right
                      );

    REF NODE nil node = NIL;


    # op codes                                                                #
    INT error            = 1;
    INT factor           = 2;
    INT add              = 3;
    INT sub              = 4;
    INT mul              = 5;
    INT div              = 6;
    INT pwr              = 7;

    []STRING op name     = ( "error", "factor", "+", "-", "*", "/", "^" );
    []BOOL   right associative
                         = ( FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE );
    []INT    priority    = ( 1, 1, 2, 2, 3, 3, 4 );



    # returns TRUE  if we have reached the end of the rpn string,             #
    #         FALSE otherwise                                                 #
    PROC at end = BOOL: rpn pos < LWB rpn;

    # positions to the previous character, if there is one                    #
    PROC next = VOID: rpn pos -:= 1;

    # skip spaces in the rpn string                                           #
    PROC skip spaces = VOID:
        WHILE have( " " )
        DO
            next
        OD # skip spaces # ;

    # returns TRUE  if the rpn character at rpn pos is c,                     #
    #         FALSE if the character is not c or there is no character        #
    #               at rpn pos                                                #
    PROC have = ( CHAR c )BOOL:
        IF at end
        THEN
            # no character at rpn pos #
            FALSE
        ELSE
            # have a character - check it is the required one #
            rpn[ rpn pos ] = c
        FI # have # ;

    # gets an operand from the rpn string                                     #
    # an operand is either a number or a sub-expression                       #
    PROC get operand = ( STRING rpn, STRING operand name )REF NODE:
    BEGIN

        # handle the operator or operand, if there is one                     #

        skip spaces;

        print( ( ( "parsing "
                 + operand name
                 + " from: "
                 + IF at end THEN "" ELSE rpn[ LWB rpn : rpn pos ] FI
                 )
               , newline
               )
             );

        REF NODE result :=
            IF   at end
            THEN
                # no operand #
                had error       := TRUE;
                HEAP NODE       := ( error, "!! Missing operand !!", NIL )
            ELIF have( "+" )
            THEN
                # addition #
                next;
                HEAP NODE right := get operand( rpn, "+ right operand" );
                HEAP NODE left  := get operand( rpn, "+ left operand"  );
                HEAP NODE       := ( add, left, right )
            ELIF have( "-" )
            THEN
                # subtraction #
                next;
                HEAP NODE right := get operand( rpn, "- right operand" );
                HEAP NODE left  := get operand( rpn, "- left operand"  );
                HEAP NODE       := ( sub, left, right )
            ELIF have( "*" )
            THEN
                # multiplication #
                next;
                HEAP NODE right := get operand( rpn, "* right operand" );
                HEAP NODE left  := get operand( rpn, "* left operand"  );
                HEAP NODE       := ( mul, left, right )
            ELIF have( "/" )
            THEN
                # division #
                next;
                HEAP NODE right := get operand( rpn, "/ right operand" );
                HEAP NODE left  := get operand( rpn, "/ left operand"  );
                HEAP NODE       := ( div, left, right )
            ELIF have( "^" )
            THEN
                # exponentiation #
                next;
                HEAP NODE right := get operand( rpn, "^ right operand" );
                HEAP NODE left  := get operand( rpn, "^ left operand"  );
                HEAP NODE       := ( pwr, left, right )
            ELSE
                # must be an operand #
                STRING value := "";

                WHILE NOT at end
                  AND NOT have( " " )
                DO
                    rpn[ rpn pos ] +=: value;
                    next
                OD;

                HEAP NODE := ( factor, value, NIL )
            FI;

        print( ( operand name + ": " + TOSTRING result, newline ) );

        result
    END # get operand # ;


    # converts the parse tree to a string with apppropriate parenthesis       #
    OP TOSTRING = ( REF NODE operand )STRING:
    BEGIN

        # converts a node of the parse tree to a string, inserting            #
        # parenthesis if necessary                                            #
        PROC possible parenthesis = ( INT op, REF NODE expr )STRING:
            IF op OF expr = error
            OR op OF expr = factor
            THEN
                # operand is an error/factor - parenthisis not needed #
                TOSTRING expr
            ELIF priority( op OF expr ) < priority( op )
            THEN
                # the expression is a higher precedence operator than the     #
                # one we are building the expression for - need parenthesis   #
                ( "( " + TOSTRING expr + " )" )
            ELIF right associative[ op OF operand ]
             AND op OF left( operand ) = op OF operand
            THEN
                # right associative operator                                  #
                ( "( " + TOSTRING expr + " )" )
            ELSE
                # lower precedence expression - parenthesis not needed        #
                TOSTRING expr
            FI # possible parenthesis # ;

        # gets the left branch of a node, which must be a node                #
        PROC left  = ( REF NODE operand )REF NODE:
            CASE left OF operand
            IN ( REF NODE o ): o
            ,  ( STRING   s ): HEAP NODE := ( error, s, NIL )
            ESAC # left # ;

        IF   had error
        THEN
            # an error occured parsing the expression #
            "Invalid expression"
        ELIF operand IS nil node
        THEN
           # no operand? #
            "<empty>"
        ELIF op OF operand = error
          OR op OF operand = factor
        THEN
            # error parsing the expression #
            # or a factor #
            CASE left OF operand
            IN ( REF NODE o ): "Error: String expected: (" + TOSTRING o + ")"
            ,  ( STRING   s ): s
            ESAC
        ELSE
            # general operand #
            ( possible parenthesis( op OF operand, left(    operand ) )
            + " " + op name[ op OF operand ] + " "
            + possible parenthesis( op OF operand, right OF operand   )
            )
        FI
    END # TOSTRING # ;

    STRING result = TOSTRING get operand( rpn, "expression" );

    # ensure there are no more tokens in the string #
    skip spaces;
    IF at end
    THEN
        # OK - there was only one expression #
        result
    ELSE
        # extraneous tokens #
        ( "Error - unexpected text before expression: ("
        + rpn[ LWB rpn : rpn pos ]
        + ")"
        )
    FI
END # rpn to infix # ;



main: (

    # test the RPN to Infix comnverter                                        #
    STRING rpn;

    rpn := "3 4 2 * 1 5 - 2 3 ^ ^ / +";
    print( ( rpn, ":  ", rpn to infix( rpn ), newline, newline ) );

    rpn := "1 2 + 3 4 + ^ 5 6 + ^";
    print( ( rpn, ":  ", rpn to infix( rpn ), newline ) )

)

```

{{out}}
```txt

 parsing expression from: 3 4 2 * 1 5 - 2 3 ^ ^ / +
 parsing + right operand from: 3 4 2 * 1 5 - 2 3 ^ ^ /
 parsing / right operand from: 3 4 2 * 1 5 - 2 3 ^ ^
 parsing ^ right operand from: 3 4 2 * 1 5 - 2 3 ^
 parsing ^ right operand from: 3 4 2 * 1 5 - 2 3
 ^ right operand: 3
 parsing ^ left operand from: 3 4 2 * 1 5 - 2
 ^ left operand: 2
 ^ right operand: 2 ^ 3
 parsing ^ left operand from: 3 4 2 * 1 5 -
 parsing - right operand from: 3 4 2 * 1 5
 - right operand: 5
 parsing - left operand from: 3 4 2 * 1
 - left operand: 1
 ^ left operand: 1 - 5
 / right operand: ( 1 - 5 ) ^ 2 ^ 3
 parsing / left operand from: 3 4 2 *
 parsing * right operand from: 3 4 2
 * right operand: 2
 parsing * left operand from: 3 4
 * left operand: 4
 / left operand: 4 * 2
 + right operand: 4 * 2 / ( 1 - 5 ) ^ 2 ^ 3
 parsing + left operand from: 3
 + left operand: 3
 expression: 3 + 4 * 2 / ( 1 - 5 ) ^ 2 ^ 3
 3 4 2 * 1 5 - 2 3 ^ ^ / +:  3 + 4 * 2 / ( 1 - 5 ) ^ 2 ^ 3

 parsing expression from: 1 2 + 3 4 + ^ 5 6 + ^
 parsing ^ right operand from: 1 2 + 3 4 + ^ 5 6 +
 parsing + right operand from: 1 2 + 3 4 + ^ 5 6
 + right operand: 6
 parsing + left operand from: 1 2 + 3 4 + ^ 5
 + left operand: 5
 ^ right operand: 5 + 6
 parsing ^ left operand from: 1 2 + 3 4 + ^
 parsing ^ right operand from: 1 2 + 3 4 +
 parsing + right operand from: 1 2 + 3 4
 + right operand: 4
 parsing + left operand from: 1 2 + 3
 + left operand: 3
 ^ right operand: 3 + 4
 parsing ^ left operand from: 1 2 +
 parsing + right operand from: 1 2
 + right operand: 2
 parsing + left operand from: 1
 + left operand: 1
 ^ left operand: 1 + 2
 ^ left operand: ( 1 + 2 ) ^ ( 3 + 4 )
 expression: ( ( 1 + 2 ) ^ ( 3 + 4 ) ) ^ ( 5 + 6 )
 1 2 + 3 4 + ^ 5 6 + ^:  ( ( 1 + 2 ) ^ ( 3 + 4 ) ) ^ ( 5 + 6 )

```



## AWK


Slavishly (mostly) follows TCL example, but instead of lists it uses strings. Except for the stack, which uses an array, of course.

The kludge is prepending the precedence on the front of the expressions stored on the stack. This shows up when the tail() function is used, and when 'x' is prepended as a placeholder when adding parenthesis.


```awk
#!/usr/bin/awk -f

BEGIN {
    initStack()
    initOpers()
    print "Infix: " toInfix("3 4 2 * 1 5 - 2 3 ^ ^ / +")
    print ""
    print "Infix: " toInfix("1 2 + 3 4 + ^ 5 6 + ^")
    print ""
    print "Infix: " toInfix("moon stars mud + * fire soup * ^")
    exit
}

function initStack() {
    delete stack
    stackPtr = 0
}

function initOpers() {
    VALPREC = "9"
    LEFT = "l"
    RIGHT = "r"
    operToks  = "+"  "-"  "/"  "*"  "^"
    operPrec  = "2"  "2"  "3"  "3"  "4"
    operAssoc = LEFT LEFT LEFT LEFT RIGHT
}

function toInfix(rpn,      t, toks, tok, a, ap, b, bp, tp, ta) {
    print "Postfix: " rpn
    split(rpn, toks, / +/)
    for (t = 1; t <= length(toks); t++) {
        tok = toks[t]
        if (!isOper(tok)) {
            push(VALPREC tok)
        }
         else {
            b = pop()
            bp = prec(b)
            b = tail(b)
            a = pop()
            ap = prec(a)
            a = tail(a)
            tp = tokPrec(tok)
            ta = tokAssoc(tok)
            if (ap < tp || (ap == tp && ta == RIGHT)) {
                a = "(" a ")"
            }
            if (bp < tp || (bp == tp && ta == LEFT)) {
                b = "(" b ")"
            }
            push(tp a " "  tok " " b)
        }
        print "    " tok " -> " stackToStr()
    }
    return tail(pop())
}

function push(expr) {
    stack[stackPtr] = expr
    stackPtr++
}

function pop() {
    stackPtr--
    return stack[stackPtr]
}

function isOper(tok) {
    return index(operToks, tok) != 0
}

function prec(expr) {
    return substr(expr, 1, 1)
}

function tokPrec(tok) {
    return substr(operPrec, operIdx(tok), 1)
}

function tokAssoc(tok) {
    return substr(operAssoc, operIdx(tok), 1)
}

function operIdx(tok) {
    return index(operToks, tok)
}

function tail(s) {
    return substr(s, 2)
}

function stackToStr(    s, i, t, p) {
    s = ""
    for (i = 0; i < stackPtr; i++) {
        t = stack[i]
        p = prec(t)
        if (index(t, " ")) t = "{" tail(t) "}"
        else t = tail(t)
        s = s "{" p " " t "} "
    }
    return s
}

```


Output:

 Postfix: 3 4 2 * 1 5 - 2 3 ^ ^ / +
     3 -> {9 3}
     4 -> {9 3} {9 4}
     2 -> {9 3} {9 4} {9 2}
     * -> {9 3} {3 {4 * 2}}
     1 -> {9 3} {3 {4 * 2}} {9 1}
     5 -> {9 3} {3 {4 * 2}} {9 1} {9 5}
     - -> {9 3} {3 {4 * 2}} {2 {1 - 5}}
     2 -> {9 3} {3 {4 * 2}} {2 {1 - 5}} {9 2}
     3 -> {9 3} {3 {4 * 2}} {2 {1 - 5}} {9 2} {9 3}
     ^ -> {9 3} {3 {4 * 2}} {2 {1 - 5}} {4 {2 ^ 3}}
     ^ -> {9 3} {3 {4 * 2}} {4 {(1 - 5) ^ 2 ^ 3}}
     / -> {9 3} {3 {4 * 2 / (1 - 5) ^ 2 ^ 3}}
     + -> {2 {3 + 4 * 2 / (1 - 5) ^ 2 ^ 3}}
 Infix: 3 + 4 * 2 / (1 - 5) ^ 2 ^ 3

 Postfix: 1 2 + 3 4 + ^ 5 6 + ^
     1 -> {9 1}
     2 -> {9 1} {9 2}
     + -> {2 {1 + 2}}
     3 -> {2 {1 + 2}} {9 3}
     4 -> {2 {1 + 2}} {9 3} {9 4}
     + -> {2 {1 + 2}} {2 {3 + 4}}
     ^ -> {4 {(1 + 2) ^ (3 + 4)}}
     5 -> {4 {(1 + 2) ^ (3 + 4)}} {9 5}
     6 -> {4 {(1 + 2) ^ (3 + 4)}} {9 5} {9 6}
     + -> {4 {(1 + 2) ^ (3 + 4)}} {2 {5 + 6}}
     ^ -> {4 {((1 + 2) ^ (3 + 4)) ^ (5 + 6)}}
 Infix: ((1 + 2) ^ (3 + 4)) ^ (5 + 6)

 Postfix: moon stars mud + * fire soup * ^
    moon -> {9 moon}
    stars -> {9 moon} {9 stars}
    mud -> {9 moon} {9 stars} {9 mud}
    + -> {9 moon} {2 {stars + mud}}
    * -> {3 {moon * (stars + mud)}}
    fire -> {3 {moon * (stars + mud)}} {9 fire}
    soup -> {3 {moon * (stars + mud)}} {9 fire} {9 soup}
    * -> {3 {moon * (stars + mud)}} {3 {fire * soup}}
    ^ -> {4 {(moon * (stars + mud)) ^ (fire * soup)}}
 Infix: (moon * (stars + mud)) ^ (fire * soup)


## AutoHotkey

{{works with|AutoHotkey_L}}

```AHK
expr := "3 4 2 * 1 5 - 2 3 ^ ^ / +"

stack := {push: func("ObjInsert"), pop: func("ObjRemove")}
out := "TOKEN`tACTION                  STACK (comma separated)`r`n"
Loop Parse, expr, %A_Space%
{
	token := A_LoopField
	if token is number
		stack.push([0, token])
	if isOp(token)
	{
		b := stack.pop(), a := stack.pop(), p := b.1 > a.1 ? b.1 : a.1
		p := Precedence(token) > p ? precedence(token) : p
		if (a.1 < b.1) and isRight(token)
			stack.push([p, "( " . a.2 " ) " token " " b.2])
		else if (a.1 > b.1) and isLeft(token)
			stack.push([p, a.2 token " ( " b.2 " ) "])
		else
			stack.push([p, a.2 . " " . token . " " . b.2])
	}
	out .= token "`t" (isOp(token) ? "Push Partial expression "
			   : "Push num" space(16)) disp(stack) "`r`n"
}
out .= "`r`n The final output infix expression is: '" disp(stack) "'"
clipboard := out
isOp(t){
       return (!!InStr("+-*/^", t) && t)
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
		if val[2]
			o .= ", " val[2]
       return  SubStr(o,3)
}
Space(n){
       return n>0 ? A_Space Space(n-1) : ""
}
```

;Output
<pre style="height:30ex;overflow:scroll;">TOKEN	ACTION                  STACK (comma separated)
3	Push num                3
4	Push num                3, 4
2	Push num                3, 4, 2
*	Push Partial expression 3, 4 * 2
1	Push num                3, 4 * 2, 1
5	Push num                3, 4 * 2, 1, 5
-	Push Partial expression 3, 4 * 2, 1 - 5
2	Push num                3, 4 * 2, 1 - 5, 2
3	Push num                3, 4 * 2, 1 - 5, 2, 3
^	Push Partial expression 3, 4 * 2, 1 - 5, 2 ^ 3
^	Push Partial expression 3, 4 * 2, ( 1 - 5 ) ^ 2 ^ 3
/	Push Partial expression 3, 4 * 2 / ( 1 - 5 ) ^ 2 ^ 3
+	Push Partial expression 3 + 4 * 2 / ( 1 - 5 ) ^ 2 ^ 3

 The final output infix expression is: '3 + 4 * 2 / ( 1 - 5 ) ^ 2 ^ 3'
```


## C

Takes RPN string from command line, string must be enclosed in double quotes. This is necessary since / and ^ are control characters for the command line. The second string, which can be any valid string, is optional and if supplied, the expression tree is printed out as it is built. The final expression is printed out in both cases.

```C

#include<stdlib.h>
#include<string.h>
#include<stdio.h>

char** components;
int counter = 0;

typedef struct elem{
	char data[10];
	struct elem* left;
	struct elem* right;
}node;

typedef node* tree;

int precedenceCheck(char oper1,char oper2){
	return (oper1==oper2)? 0:(oper1=='^')? 1:(oper2=='^')? 2:(oper1=='/')? 1:(oper2=='/')? 2:(oper1=='*')? 1:(oper2=='*')? 2:(oper1=='+')? 1:(oper2=='+')? 2:(oper1=='-')? 1:2;
}

int isOperator(char c){
	return (c=='+'||c=='-'||c=='*'||c=='/'||c=='^');
}

void inorder(tree t){
	if(t!=NULL){
		if(t->left!=NULL && isOperator(t->left->data[0])==1 && (precedenceCheck(t->data[0],t->left->data[0])==1 || (precedenceCheck(t->data[0],t->left->data[0])==0 && t->data[0]=='^'))){
			printf("(");
			inorder(t->left);
			printf(")");
		}
		else
			inorder(t->left);

		printf(" %s ",t->data);

		if(t->right!=NULL && isOperator(t->right->data[0])==1 && (precedenceCheck(t->data[0],t->right->data[0])==1 || (precedenceCheck(t->data[0],t->right->data[0])==0 && t->data[0]!='^'))){
			printf("(");
			inorder(t->right);
			printf(")");
		}
		else
			inorder(t->right);
	}
}

char* getNextString(){
	if(counter<0){
		printf("\nInvalid RPN !");
		exit(0);
	}
	return components[counter--];
}

tree buildTree(char* obj,char* trace){
	tree t = (tree)malloc(sizeof(node));

	strcpy(t->data,obj);

	t->right = (isOperator(obj[0])==1)?buildTree(getNextString(),trace):NULL;
	t->left = (isOperator(obj[0])==1)?buildTree(getNextString(),trace):NULL;

	if(trace!=NULL){
			printf("\n");
			inorder(t);
	}

	return t;
}

int checkRPN(){
	int i, operSum = 0, numberSum = 0;

	if(isOperator(components[counter][0])==0)
		return 0;

	for(i=0;i<=counter;i++)
		(isOperator(components[i][0])==1)?operSum++:numberSum++;

	return (numberSum - operSum == 1);
}

void buildStack(char* str){
	int i;
	char* token;

	for(i=0;str[i]!=00;i++)
		if(str[i]==' ')
			counter++;

	components = (char**)malloc((counter + 1)*sizeof(char*));

	token = strtok(str," ");

	i = 0;

	while(token!=NULL){
		components[i] = (char*)malloc(strlen(token)*sizeof(char));
		strcpy(components[i],token);
		token = strtok(NULL," ");
		i++;
	}
}

int main(int argC,char* argV[]){
	int i;
	tree t;

	if(argC==1)
		printf("Usage : %s <RPN expression enclosed by quotes> <optional parameter to trace the build process>",argV[0]);
	else{
		buildStack(argV[1]);

		if(checkRPN()==0){
			printf("\nInvalid RPN !");
			return 0;
		}

		t = buildTree(getNextString(),argV[2]);

		printf("\nFinal infix expression : ");
		inorder(t);
	}

	return 0;
}

```

Output, both final and traced outputs are shown:

```txt

C:\rosettaCode>rpn2Infix.exe "3 4 2 * 1 5 - 2 3 ^ ^ / +"

Final infix expression :  3  + ( 4  *  2 ) / ( 1  -  5 ) ^  2  ^  3
C:\rosettaCode>rpn2Infix.exe "1 2 + 3 4 + ^ 5 6 + ^"

Final infix expression : (( 1  +  2 ) ^ ( 3  +  4 )) ^ ( 5  +  6 )
C:\rosettaCode>rpn2Infix.exe "3 4 2 * 1 5 - 2 3 ^ ^ / +" yes

 3
 2
 2  ^  3
 5
 1
 1  -  5
( 1  -  5 ) ^  2  ^  3
 2
 4
 4  *  2
( 4  *  2 ) / ( 1  -  5 ) ^  2  ^  3
 3
 3  + ( 4  *  2 ) / ( 1  -  5 ) ^  2  ^  3
Final infix expression :  3  + ( 4  *  2 ) / ( 1  -  5 ) ^  2  ^  3
C:\rosettaCode>rpn2Infix.exe "1 2 + 3 4 + ^ 5 6 + ^" yes

 6
 5
 5  +  6
 4
 3
 3  +  4
 2
 1
 1  +  2
( 1  +  2 ) ^ ( 3  +  4 )
(( 1  +  2 ) ^ ( 3  +  4 )) ^ ( 5  +  6 )
Final infix expression : (( 1  +  2 ) ^ ( 3  +  4 )) ^ ( 5  +  6 )

```



## C#

{{trans|Java}}

```c#
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text.RegularExpressions;

namespace PostfixToInfix
{
    class Program
    {
        class Operator
        {
            public Operator(char t, int p, bool i = false)
            {
                Token = t;
                Precedence = p;
                IsRightAssociative = i;
            }

            public char Token { get; private set; }
            public int Precedence { get; private set; }
            public bool IsRightAssociative { get; private set; }
        }

        static IReadOnlyDictionary<char, Operator> operators = new Dictionary<char, Operator>
        {
            { '+', new Operator('+', 2) },
            { '-', new Operator('-', 2) },
            { '/', new Operator('/', 3) },
            { '*', new Operator('*', 3) },
            { '^', new Operator('^', 4, true) }
        };

        class Expression
        {
            public String ex;
            public Operator op;

            public Expression(String e)
            {
                ex = e;
            }

            public Expression(String e1, String e2, Operator o)
            {
                ex = String.Format("{0} {1} {2}", e1, o.Token, e2);
                op = o;
            }
        }

        static String PostfixToInfix(String postfix)
        {
            var stack = new Stack<Expression>();

            foreach (var token in Regex.Split(postfix, @"\s+"))
            {
                char c = token[0];

                var op = operators.FirstOrDefault(kv => kv.Key == c).Value;
                if (op != null && token.Length == 1)
                {
                    Expression rhs = stack.Pop();
                    Expression lhs = stack.Pop();

                    int opPrec = op.Precedence;

                    int lhsPrec = lhs.op != null ? lhs.op.Precedence : int.MaxValue;
                    int rhsPrec = rhs.op != null ? rhs.op.Precedence : int.MaxValue;

                    if ((lhsPrec < opPrec || (lhsPrec == opPrec && c == '^')))
                        lhs.ex = '(' + lhs.ex + ')';

                    if ((rhsPrec < opPrec || (rhsPrec == opPrec && c != '^')))
                        rhs.ex = '(' + rhs.ex + ')';

                    stack.Push(new Expression(lhs.ex, rhs.ex, op));
                }
                else
                {
                    stack.Push(new Expression(token));
                }

                // print intermediate result
                Console.WriteLine("{0} -> [{1}]", token,
                    string.Join(", ", stack.Reverse().Select(e => e.ex)));
            }
            return stack.Peek().ex;
        }

        static void Main(string[] args)
        {
            string[] inputs = { "3 4 2 * 1 5 - 2 3 ^ ^ / +", "1 2 + 3 4 + ^ 5 6 + ^" };
            foreach (var e in inputs)
            {
                Console.WriteLine("Postfix : {0}", e);
                Console.WriteLine("Infix : {0}", PostfixToInfix(e));
                Console.WriteLine(); ;
            }
            Console.ReadLine();
        }
    }
}
```


```txt
3 -> [3]
4 -> [3, 4]
2 -> [3, 4, 2]
* -> [3, 4 * 2]
1 -> [3, 4 * 2, 1]
5 -> [3, 4 * 2, 1, 5]
- -> [3, 4 * 2, 1 - 5]
2 -> [3, 4 * 2, 1 - 5, 2]
3 -> [3, 4 * 2, 1 - 5, 2, 3]
^ -> [3, 4 * 2, 1 - 5, 2 ^ 3]
^ -> [3, 4 * 2, (1 - 5) ^ 2 ^ 3]
/ -> [3, 4 * 2 / (1 - 5) ^ 2 ^ 3]
+ -> [3 + 4 * 2 / (1 - 5) ^ 2 ^ 3]
Infix : 3 + 4 * 2 / (1 - 5) ^ 2 ^ 3

Postfix : 1 2 + 3 4 + ^ 5 6 + ^
1 -> [1]
2 -> [1, 2]
+ -> [1 + 2]
3 -> [1 + 2, 3]
4 -> [1 + 2, 3, 4]
+ -> [1 + 2, 3 + 4]
^ -> [(1 + 2) ^ (3 + 4)]
5 -> [(1 + 2) ^ (3 + 4), 5]
6 -> [(1 + 2) ^ (3 + 4), 5, 6]
+ -> [(1 + 2) ^ (3 + 4), 5 + 6]
^ -> [((1 + 2) ^ (3 + 4)) ^ (5 + 6)]
Infix : ((1 + 2) ^ (3 + 4)) ^ (5 + 6)
```



## C++

Very primitive implementation, doesn't use any parsing libraries which would shorten this greatly.

```Cpp

#include <iostream>
#include <stack>
#include <string>
#include <map>
#include <set>

using namespace std;

struct Entry_
{
	string expr_;
	string op_;
};

bool PrecedenceLess(const string& lhs, const string& rhs, bool assoc)
{
	static const map<string, int> KNOWN({ { "+", 1 }, { "-", 1 }, { "*", 2 }, { "/", 2 }, { "^", 3 } });
	static const set<string> ASSOCIATIVE({ "+", "*" });
	return (KNOWN.count(lhs) ? KNOWN.find(lhs)->second : 0) < (KNOWN.count(rhs) ? KNOWN.find(rhs)->second : 0) + (assoc && !ASSOCIATIVE.count(rhs) ? 1 : 0);
}
void Parenthesize(Entry_* old, const string& token, bool assoc)
{
	if (!old->op_.empty() && PrecedenceLess(old->op_, token, assoc))
		old->expr_ = '(' + old->expr_ + ')';
}

void AddToken(stack<Entry_>* stack, const string& token)
{
	if (token.find_first_of("0123456789") != string::npos)
		stack->push(Entry_({ token, string() }));	// it's a number, no operator
	else
	{	// it's an operator
		if (stack->size() < 2)
			cout<<"Stack underflow";
		auto rhs = stack->top();
		Parenthesize(&rhs, token, false);
		stack->pop();
		auto lhs = stack->top();
		Parenthesize(&lhs, token, true);
		stack->top().expr_ = lhs.expr_ + ' ' + token + ' ' + rhs.expr_;
		stack->top().op_ = token;
	}
}


string ToInfix(const string& src)
{
	stack<Entry_> stack;
	for (auto start = src.begin(), p = src.begin(); ; ++p)
	{
		if (p == src.end() || *p == ' ')
		{
			if (p > start)
				AddToken(&stack, string(start, p));
			if (p == src.end())
				break;
			start = p + 1;
		}
	}
	if (stack.size() != 1)
		cout<<"Incomplete expression";
	return stack.top().expr_;
}

int main(void)
{
	try
	{
		cout << ToInfix("3 4 2 * 1 5 - 2 3 ^ ^ / +") << "\n";
		cout << ToInfix("1 2 + 3 4 + ^ 5 6 + ^") << "\n";
		return 0;
	}
	catch (...)
	{
		cout << "Failed\n";
		return -1;
	}
}

```

Output :

```txt

3 + (4 * 2) / (1 - 5) ^ 2 ^ 3
((1 + 2) ^ (3 + 4)) ^ (5 + 6)

```



## Common Lisp

Tested on ABCL.

```lisp

;;;; Parsing/RPN to infix conversion
(defstruct (node (:print-function print-node)) opr infix)
(defun print-node (node stream depth)
  (format stream "opr:=~A infix:=\"~A\"" (node-opr node) (node-infix node)))

(defconstant OPERATORS '((#\^ . 4) (#\* . 3) (#\/ . 3) (#\+ . 2) (#\- . 2)))

;;; (char,char[,boolean])->boolean
(defun higher-p (opp opc &optional (left-node-p nil))
  (or (> (cdr (assoc opp OPERATORS)) (cdr (assoc opc OPERATORS)))
      (and left-node-p (char= opp #\^) (char= opc #\^))))

;;; string->list
(defun string-split (expr)
  (let ((p (position #\Space expr)))
    (if (null p) (list expr)
        (append (list (subseq expr 0 p))
                (string-split (subseq expr (1+ p)))))))

;;; string->string
(defun parse (expr)
  (let ((stack '()))
    (format t "TOKEN   STACK~%")
    (dolist (tok (string-split expr))
      (if (assoc (char tok 0) OPERATORS) ; operator?
          (push (make-node :opr (char tok 0) :infix (infix (char tok 0) (pop stack) (pop stack))) stack)
          (push tok stack))

      ;; print stack at each token
      (format t "~3,A" tok)
      (dotimes (i (length stack)) (format t "~8,T[~D] ~A~%" i (nth i stack))))

    ;; print final infix expression
    (if (= (length stack) 1)
        (format nil "~A" (node-infix (first stack)))
        (format nil "syntax error in ~A" expr))))

;;; (char,node,node)->string
(defun infix (operator rightn leftn)

  ;; (char,node[,boolean]->string
  (defun string-node (operator anode &optional (left-node-p nil))
    (if (stringp anode) anode
        (if (higher-p operator (node-opr anode) left-node-p)
            (format nil "( ~A )" (node-infix anode)) (node-infix anode))))

  (concatenate 'string
               (string-node operator leftn t)
               (format nil " ~A " operator)
               (string-node operator rightn)))

;;; nil->[printed infix expressions]
(defun main ()
  (let ((expressions '("3 4 2 * 1 5 - 2 3 ^ ^ / +"
                       "1 2 + 3 4 + ^ 5 6 + ^"
                       "3 4 ^ 2 9 ^ ^ 2 5 ^ ^")))
    (dolist (expr expressions)
      (format t "~%Parsing:\"~A\"~%" expr)
      (format t "RPN:\"~A\" INFIX:\"~A\"~%" expr (parse expr)))))

```


{{out}}

```txt

(main)

Parsing:"3 4 2 * 1 5 - 2 3 ^ ^ / +"
TOKEN   STACK
3       [0] 3
4       [0] 4
        [1] 3
2       [0] 2
        [1] 4
        [2] 3
*       [0] opr:=* infix:="4 * 2"
        [1] 3
1       [0] 1
        [1] opr:=* infix:="4 * 2"
        [2] 3
5       [0] 5
        [1] 1
        [2] opr:=* infix:="4 * 2"
        [3] 3
-       [0] opr:=- infix:="1 - 5"
        [1] opr:=* infix:="4 * 2"
        [2] 3
2       [0] 2
        [1] opr:=- infix:="1 - 5"
        [2] opr:=* infix:="4 * 2"
        [3] 3
3       [0] 3
        [1] 2
        [2] opr:=- infix:="1 - 5"
        [3] opr:=* infix:="4 * 2"
        [4] 3
^       [0] opr:=^ infix:="2 ^ 3"
        [1] opr:=- infix:="1 - 5"
        [2] opr:=* infix:="4 * 2"
        [3] 3
^       [0] opr:=^ infix:="( 1 - 5 ) ^ 2 ^ 3"
        [1] opr:=* infix:="4 * 2"
        [2] 3
/       [0] opr:=/ infix:="4 * 2 / ( 1 - 5 ) ^ 2 ^ 3"
        [1] 3
+       [0] opr:=+ infix:="3 + 4 * 2 / ( 1 - 5 ) ^ 2 ^ 3"
RPN:"3 4 2 * 1 5 - 2 3 ^ ^ / +" INFIX:"3 + 4 * 2 / ( 1 - 5 ) ^ 2 ^ 3"

Parsing:"1 2 + 3 4 + ^ 5 6 + ^"
TOKEN   STACK
1       [0] 1
2       [0] 2
        [1] 1
+       [0] opr:=+ infix:="1 + 2"
3       [0] 3
        [1] opr:=+ infix:="1 + 2"
4       [0] 4
        [1] 3
        [2] opr:=+ infix:="1 + 2"
+       [0] opr:=+ infix:="3 + 4"
        [1] opr:=+ infix:="1 + 2"
^       [0] opr:=^ infix:="( 1 + 2 ) ^ ( 3 + 4 )"
5       [0] 5
        [1] opr:=^ infix:="( 1 + 2 ) ^ ( 3 + 4 )"
6       [0] 6
        [1] 5
        [2] opr:=^ infix:="( 1 + 2 ) ^ ( 3 + 4 )"
+       [0] opr:=+ infix:="5 + 6"
        [1] opr:=^ infix:="( 1 + 2 ) ^ ( 3 + 4 )"
^       [0] opr:=^ infix:="( ( 1 + 2 ) ^ ( 3 + 4 ) ) ^ ( 5 + 6 )"
RPN:"1 2 + 3 4 + ^ 5 6 + ^" INFIX:"( ( 1 + 2 ) ^ ( 3 + 4 ) ) ^ ( 5 + 6 )"

Parsing:"3 4 ^ 2 9 ^ ^ 2 5 ^ ^"
TOKEN   STACK
3       [0] 3
4       [0] 4
        [1] 3
^       [0] opr:=^ infix:="3 ^ 4"
2       [0] 2
        [1] opr:=^ infix:="3 ^ 4"
9       [0] 9
        [1] 2
        [2] opr:=^ infix:="3 ^ 4"
^       [0] opr:=^ infix:="2 ^ 9"
        [1] opr:=^ infix:="3 ^ 4"
^       [0] opr:=^ infix:="( 3 ^ 4 ) ^ 2 ^ 9"
2       [0] 2
        [1] opr:=^ infix:="( 3 ^ 4 ) ^ 2 ^ 9"
5       [0] 5
        [1] 2
        [2] opr:=^ infix:="( 3 ^ 4 ) ^ 2 ^ 9"
^       [0] opr:=^ infix:="2 ^ 5"
        [1] opr:=^ infix:="( 3 ^ 4 ) ^ 2 ^ 9"
^       [0] opr:=^ infix:="( ( 3 ^ 4 ) ^ 2 ^ 9 ) ^ 2 ^ 5"
RPN:"3 4 ^ 2 9 ^ ^ 2 5 ^ ^" INFIX:"( ( 3 ^ 4 ) ^ 2 ^ 9 ) ^ 2 ^ 5"
NIL

```



## D

{{trans|Go}}

```d
import std.stdio, std.string, std.array;

void parseRPN(in string e) {
    enum nPrec = 9;
    static struct Info { int prec; bool rAssoc; }
    immutable /*static*/ opa = ["^": Info(4, true),
                                "*": Info(3, false),
                                "/": Info(3, false),
                                "+": Info(2, false),
                                "-": Info(2, false)];

    writeln("\nPostfix input: ", e);
    static struct Sf { int prec; string expr; }
    Sf[] stack;
    foreach (immutable tok; e.split()) {
        writeln("Token: ", tok);
        if (tok in opa) {
            immutable op = opa[tok];
            immutable rhs = stack.back;
            stack.popBack();
            auto lhs = &stack.back;
            if (lhs.prec < op.prec ||
                (lhs.prec == op.prec && op.rAssoc))
                lhs.expr = "(" ~ lhs.expr ~ ")";
            lhs.expr ~= " " ~ tok ~ " ";
            lhs.expr ~= (rhs.prec < op.prec ||
                         (rhs.prec == op.prec && !op.rAssoc)) ?
                        "(" ~ rhs.expr ~ ")" :
                        rhs.expr;
            lhs.prec = op.prec;
        } else
            stack ~= Sf(nPrec, tok);
        foreach (immutable f; stack)
            writefln(`    %d "%s"`, f.prec, f.expr);
    }
    writeln("Infix result: ", stack[0].expr);
}

void main() {
    foreach (immutable test; ["3 4 2 * 1 5 - 2 3 ^ ^ / +",
                              "1 2 + 3 4 + ^ 5 6 + ^"])
        parseRPN(test);
}
```

{{out}}

```txt

Postfix input: 3 4 2 * 1 5 - 2 3 ^ ^ / +
Token: 3
    9 "3"
Token: 4
    9 "3"
    9 "4"
Token: 2
    9 "3"
    9 "4"
    9 "2"
Token: *
    9 "3"
    3 "4 * 2"
Token: 1
    9 "3"
    3 "4 * 2"
    9 "1"
Token: 5
    9 "3"
    3 "4 * 2"
    9 "1"
    9 "5"
Token: -
    9 "3"
    3 "4 * 2"
    2 "1 - 5"
Token: 2
    9 "3"
    3 "4 * 2"
    2 "1 - 5"
    9 "2"
Token: 3
    9 "3"
    3 "4 * 2"
    2 "1 - 5"
    9 "2"
    9 "3"
Token: ^
    9 "3"
    3 "4 * 2"
    2 "1 - 5"
    4 "2 ^ 3"
Token: ^
    9 "3"
    3 "4 * 2"
    4 "(1 - 5) ^ 2 ^ 3"
Token: /
    9 "3"
    3 "4 * 2 / (1 - 5) ^ 2 ^ 3"
Token: +
    2 "3 + 4 * 2 / (1 - 5) ^ 2 ^ 3"
Infix result: 3 + 4 * 2 / (1 - 5) ^ 2 ^ 3

Postfix input: 1 2 + 3 4 + ^ 5 6 + ^
Token: 1
    9 "1"
Token: 2
    9 "1"
    9 "2"
Token: +
    2 "1 + 2"
Token: 3
    2 "1 + 2"
    9 "3"
Token: 4
    2 "1 + 2"
    9 "3"
    9 "4"
Token: +
    2 "1 + 2"
    2 "3 + 4"
Token: ^
    4 "(1 + 2) ^ (3 + 4)"
Token: 5
    4 "(1 + 2) ^ (3 + 4)"
    9 "5"
Token: 6
    4 "(1 + 2) ^ (3 + 4)"
    9 "5"
    9 "6"
Token: +
    4 "(1 + 2) ^ (3 + 4)"
    2 "5 + 6"
Token: ^
    4 "((1 + 2) ^ (3 + 4)) ^ (5 + 6)"
Infix result: ((1 + 2) ^ (3 + 4)) ^ (5 + 6)
```



### Alternative Version

{{trans|Perl 6}}

```d
import std.stdio, std.string, std.array, std.algorithm;

void rpmToInfix(in string str) @safe {
    static struct Exp { int p; string e; }
    immutable P = (in Exp pair, in int prec) pure =>
        pair.p < prec ? format("( %s )", pair.e) : pair.e;
    immutable F = (in string[] s...) pure nothrow => s.join(' ');

    writefln("
### ===========
\n%s", str);
    Exp[] stack;
    foreach (const w; str.split) {
        if (w.isNumeric)
            stack ~= Exp(9, w);
        else {
            const y = stack.back; stack.popBack;
            const x = stack.back; stack.popBack;
            switch (w) {
                case "^": stack ~= Exp(4, F(P(x, 5), w, P(y, 4)));
                    break;
                case "*", "/": stack ~= Exp(3, F(P(x, 3), w, P(y, 3)));
                    break;
                case "+", "-": stack ~= Exp(2, F(P(x, 2), w, P(y, 2)));
                    break;
                default: throw new Error("Wrong part: " ~ w);
            }
        }
        stack.map!q{ a.e }.writeln;
    }
    writeln("-----------------\n", stack.back.e);
}

void main() {
    "3 4 2 * 1 5 - 2 3 ^ ^ / +".rpmToInfix;
    "1 2 + 3 4 + ^ 5 6 + ^".rpmToInfix;
}
```

{{out}}

```txt

### ===========

3 4 2 * 1 5 - 2 3 ^ ^ / +
["3"]
["3", "4"]
["3", "4", "2"]
["3", "4 * 2"]
["3", "4 * 2", "1"]
["3", "4 * 2", "1", "5"]
["3", "4 * 2", "1 - 5"]
["3", "4 * 2", "1 - 5", "2"]
["3", "4 * 2", "1 - 5", "2", "3"]
["3", "4 * 2", "1 - 5", "2 ^ 3"]
["3", "4 * 2", "( 1 - 5 ) ^ 2 ^ 3"]
["3", "4 * 2 / ( 1 - 5 ) ^ 2 ^ 3"]
["3 + 4 * 2 / ( 1 - 5 ) ^ 2 ^ 3"]
-----------------
3 + 4 * 2 / ( 1 - 5 ) ^ 2 ^ 3

### ===========

1 2 + 3 4 + ^ 5 6 + ^
["1"]
["1", "2"]
["1 + 2"]
["1 + 2", "3"]
["1 + 2", "3", "4"]
["1 + 2", "3 + 4"]
["( 1 + 2 ) ^ ( 3 + 4 )"]
["( 1 + 2 ) ^ ( 3 + 4 )", "5"]
["( 1 + 2 ) ^ ( 3 + 4 )", "5", "6"]
["( 1 + 2 ) ^ ( 3 + 4 )", "5 + 6"]
["( ( 1 + 2 ) ^ ( 3 + 4 ) ) ^ ( 5 + 6 )"]
-----------------
( ( 1 + 2 ) ^ ( 3 + 4 ) ) ^ ( 5 + 6 )
```




## EchoLisp

For convenience, modularity, reusability, and the fun of it,  we split the task into two parts. '''rpn->infix''' checks the rpn expression and builds an infix - lisp - tree (which can be the input of an infix calculator). '''infix->string''' takes a tree in input and builds the required string.


```scheme

(require 'hash)
(string-delimiter "")

(define (^ a b ) (expt a b)) ;; add this not-native function
(define-syntax-rule (r-assoc? op) (= op  "^"))
(define-syntax-rule (l-assoc? op) (not ( = op "^" )))
(define PRECEDENCES (list->hash
	'(("+" . 2) ("-" . 2) ("*" . 3) ("/" . 3) ("//" . 3) ("^" . 4))
	(make-hash)))

;; RPN vector or list -> infix tree  ->  (a op (b op c) d) ..
(define (rpn->infix rpn)
(define S (stack 'S))
	(for ((token rpn))
	(if (procedure? token)
		(let [(op2 (pop S)) (op1 (pop S))]
			(unless (and op1 op2)   (error "cannot translate expression" rpn))
	    	(push S (list op1 token op2))
	    	)
	    (push S token ))
	(writeln 'token (string token) 'stack (stack->list S)))
	(begin0
		(pop S) ;; return (top S)
		(unless (stack-empty? S) (error "ill-formed rpn" rpn)))
	)

;; a node  tree is (left op right) or a  number
(define-syntax-id _.left (first _)) ; mynode.left expands to (first mynode)
(define-syntax-id _.right (third _))
(define-syntax-id _.op  (string (second _ )))
(define-syntax-rule (precedence node) (hash-ref PRECEDENCES (string (second node))))

(define (left-par? node) ; does lhs needs ( lhs ) ?
	(cond
	[(number? node.left) #f]
	[(< (precedence node.left) (precedence node)) #t]
	[(and
		 	(r-assoc? node.op)
		 	(= (precedence node.left) (precedence node))) #t]
	[else #f]))

(define (right-par? node)
	(cond
	[(number? node.right) #f]
	[(< (precedence node.right) (precedence node)) #t]
	[(and
		 	(l-assoc? node.op)
		 	(= (precedence node.right) (precedence node))) #t]
	[else #f]))

;; infix tree -> char string
(define (infix->string node)
	(cond
	[(number? node) (string node)]
	[else (let
		[(lhs (infix->string node.left))
		(rhs (infix->string node.right))]
		(when (left-par? node) (set! lhs (string-append "(" lhs ")")))
		(when (right-par? node) (set! rhs (string-append "(" rhs ")")))
		(string-append lhs " " node.op " " rhs))]))

```

{{out}}

```txt

(infix->string (rpn->infix #(3 4 2 * 1 5 - 2 3 ^ ^ / +)))
token     3     stack     (3)
token     4     stack     (3 4)
token     2     stack     (3 4 2)
token     *     stack     (3 (4 #* 2))
token     1     stack     (3 (4 #* 2) 1)
token     5     stack     (3 (4 #* 2) 1 5)
token     -     stack     (3 (4 #* 2) (1 #- 5))
token     2     stack     (3 (4 #* 2) (1 #- 5) 2)
token     3     stack     (3 (4 #* 2) (1 #- 5) 2 3)
token     ^     stack     (3 (4 #* 2) (1 #- 5) (2 ^ 3))
token     ^     stack     (3 (4 #* 2) ((1 #- 5) ^ (2 ^ 3)))
token     /     stack     (3 ((4 #* 2) #/ ((1 #- 5) ^ (2 ^ 3))))
token     +     stack     ((3 #+ ((4 #* 2) #/ ((1 #- 5) ^ (2 ^ 3)))))
    → 3 + 4 * 2 / (1 - 5) ^ 2 ^ 3

(infix->string (rpn->infix #(1 2 + 3 4 + ^ 5 6 + ^)))
token     1     stack     (1)
token     2     stack     (1 2)
token     +     stack     ((1 #+ 2))
token     3     stack     ((1 #+ 2) 3)
token     4     stack     ((1 #+ 2) 3 4)
token     +     stack     ((1 #+ 2) (3 #+ 4))
token     ^     stack     (((1 #+ 2) ^ (3 #+ 4)))
token     5     stack     (((1 #+ 2) ^ (3 #+ 4)) 5)
token     6     stack     (((1 #+ 2) ^ (3 #+ 4)) 5 6)
token     +     stack     (((1 #+ 2) ^ (3 #+ 4)) (5 #+ 6))
token     ^     stack     ((((1 #+ 2) ^ (3 #+ 4)) ^ (5 #+ 6)))
    → ((1 + 2) ^ (3 + 4)) ^ (5 + 6)

(infix->string (rpn->infix #( 5 6 * * + +)))
token     5     stack     (5)
token     6     stack     (5 6)
token     *     stack     ((5 #* 6))
⛔️ error: cannot translate expression #( 5 6 #* #* #+ #+)

```


=={{header|F Sharp|F#}}==

```fsharp
type ast =
    | Num  of int
    | Add  of ast * ast | Sub  of ast * ast
    | Mul  of ast * ast | Div  of ast * ast
    | Exp  of ast * ast

let (|Int|_|) = System.Int32.TryParse >> function | (true, v) -> Some(v) | (false, _) -> None

let rec parse =
    function
    | [] -> failwith "Not enough tokens"
    | (Int head)::tail -> (Num(head), tail)
    | op::tail ->
        let (left, rest1) = parse tail
        let (right, rest2) = parse rest1
        match op with
        | "+" -> (Add (right, left)), rest2
        | "-" -> (Sub (right, left)), rest2
        | "*" -> (Mul (right, left)), rest2
        | "/" -> (Div (right, left)), rest2
        | "^" -> (Exp (right, left)), rest2
        | _ -> failwith ("unknown op: " + op)

let rec infix p x  =
    let brackets (x : ast) = seq { yield "("; yield! infix 0 x; yield ")" }
    let left op context l r = seq { yield! infix context l; yield op; yield! infix context r }
    let right op context l r = seq { yield! brackets l; yield op; yield! infix context r }
    seq {
        match x with
        | Num (n) -> yield n.ToString()
        | Add (l, r) when p <= 2 -> yield! left "+" 2 l r
        | Sub (l, r) when p <= 2 -> yield! left "-" 2 l r
        | Mul (l, r) when p <= 3 -> yield! left "*" 3 l r
        | Div (l, r) when p <= 3 -> yield! left "/" 3 l r
        | Exp (Exp(ll, lr), r)   -> yield! right "^" 4 (Exp(ll,lr)) r
        | Exp (l, r)             -> yield! left "^" 4 l r
        | _ -> yield! brackets x
    }

[<EntryPoint>]
let main argv =
    let (tree, rest) =
        argv |> Array.rev |> Seq.toList |> parse
    match rest with
    | [] -> printfn "%A" tree
    | _ -> failwith ("not a valid RPN expression (excess tokens): " + System.String.Join(" ", argv))
    Seq.iter (printf " %s") (infix 0 tree); printfn ""
    0

```

Input is given via the command line.
Output includes the abstract syntax tree generated for the input.
Output for the 2 test cases given above:

```txt
Add (Num 3,Div (Mul (Num 4,Num 2),Exp (Sub (Num 1,Num 5),Exp (Num 2,Num 3))))
 3 + 4 * 2 / ( 1 - 5 ) ^ 2 ^ 3

Exp (Exp (Add (Num 1,Num 2),Add (Num 3,Num 4)),Add (Num 5,Num 6))
 ( ( 1 + 2 ) ^ ( 3 + 4 ) ) ^ ( 5 + 6 )
```



## Go

No error checking.

```go
package main

import (
    "fmt"
    "strings"
)

var tests = []string{
    "3 4 2 * 1 5 - 2 3 ^ ^ / +",
    "1 2 + 3 4 + ^ 5 6 + ^",
}

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

const nPrec = 9

func main() {
    for _, t := range tests {
        parseRPN(t)
    }
}

func parseRPN(e string) {
    fmt.Println("\npostfix:", e)
    type sf struct {
        prec int
        expr string
    }
    var stack []sf
    for _, tok := range strings.Fields(e) {
        fmt.Println("token:", tok)
        if op, isOp := opa[tok]; isOp {
            rhs := &stack[len(stack)-1]
            stack = stack[:len(stack)-1]
            lhs := &stack[len(stack)-1]
            if lhs.prec < op.prec || (lhs.prec == op.prec && op.rAssoc) {
                lhs.expr = "(" + lhs.expr + ")"
            }
            lhs.expr += " " + tok + " "
            if rhs.prec < op.prec || (rhs.prec == op.prec && !op.rAssoc) {
                lhs.expr += "(" + rhs.expr + ")"
            } else {
                lhs.expr += rhs.expr
            }
            lhs.prec = op.prec
        } else {
            stack = append(stack, sf{nPrec, tok})
        }
        for _, f := range stack {
            fmt.Printf("    %d %q\n", f.prec, f.expr)
        }
    }
    fmt.Println("infix:", stack[0].expr)
}
```

Output:

```txt

postfix: 3 4 2 * 1 5 - 2 3 ^ ^ / +
token: 3
    9 "3"
token: 4
    9 "3"
    9 "4"
token: 2
    9 "3"
    9 "4"
    9 "2"
token: *
    9 "3"
    3 "4 * 2"
token: 1
    9 "3"
    3 "4 * 2"
    9 "1"
token: 5
    9 "3"
    3 "4 * 2"
    9 "1"
    9 "5"
token: -
    9 "3"
    3 "4 * 2"
    2 "1 - 5"
token: 2
    9 "3"
    3 "4 * 2"
    2 "1 - 5"
    9 "2"
token: 3
    9 "3"
    3 "4 * 2"
    2 "1 - 5"
    9 "2"
    9 "3"
token: ^
    9 "3"
    3 "4 * 2"
    2 "1 - 5"
    4 "2 ^ 3"
token: ^
    9 "3"
    3 "4 * 2"
    4 "(1 - 5) ^ 2 ^ 3"
token: /
    9 "3"
    3 "4 * 2 / (1 - 5) ^ 2 ^ 3"
token: +
    2 "3 + 4 * 2 / (1 - 5) ^ 2 ^ 3"
infix: 3 + 4 * 2 / (1 - 5) ^ 2 ^ 3

postfix: 1 2 + 3 4 + ^ 5 6 + ^
token: 1
    9 "1"
token: 2
    9 "1"
    9 "2"
token: +
    2 "1 + 2"
token: 3
    2 "1 + 2"
    9 "3"
token: 4
    2 "1 + 2"
    9 "3"
    9 "4"
token: +
    2 "1 + 2"
    2 "3 + 4"
token: ^
    4 "(1 + 2) ^ (3 + 4)"
token: 5
    4 "(1 + 2) ^ (3 + 4)"
    9 "5"
token: 6
    4 "(1 + 2) ^ (3 + 4)"
    9 "5"
    9 "6"
token: +
    4 "(1 + 2) ^ (3 + 4)"
    2 "5 + 6"
token: ^
    4 "((1 + 2) ^ (3 + 4)) ^ (5 + 6)"
infix: ((1 + 2) ^ (3 + 4)) ^ (5 + 6)

```



## Groovy

{{trans|Java}}

```groovy
class PostfixToInfix {
    static class Expression {
        final static String ops = "-+/*^"

        String op, ex
        int precedence = 3

        Expression(String e) {
            ex = e
        }

        Expression(String e1, String e2, String o) {
            ex = String.format "%s %s %s", e1, o, e2
            op = o
            precedence = (ops.indexOf(o) / 2) as int
        }

        @Override
        String toString() {
            return ex
        }
    }

    static String postfixToInfix(final String postfix) {
        Stack<Expression> expr = new Stack<>()

        for (String token in postfix.split("\\s+")) {
            char c = token.charAt(0)
            int idx = Expression.ops.indexOf(c as int)
            if (idx != -1 && token.length() == 1) {

                Expression r = expr.pop()
                Expression l = expr.pop()

                int opPrecedence = (idx / 2) as int

                if (l.precedence < opPrecedence || (l.precedence == opPrecedence && c == '^' as char))
                    l.ex = '(' + l.ex + ')'

                if (r.precedence < opPrecedence || (r.precedence == opPrecedence && c != '^' as char))
                    r.ex = '(' + r.ex + ')'

                expr << new Expression(l.ex, r.ex, token)
            } else {
                expr << new Expression(token)
            }
            printf "%s -> %s%n", token, expr
        }
        expr.peek().ex
    }

    static void main(String[] args) {
        (["3 4 2 * 1 5 - 2 3 ^ ^ / +", "1 2 + 3 4 + ^ 5 6 + ^"]).each { String e ->
            printf "Postfix : %s%n", e
            printf "Infix : %s%n", postfixToInfix(e)
            println()
        }
    }
}
```

{{out}}

```txt
Postfix : 3 4 2 * 1 5 - 2 3 ^ ^ / +
3 -> [3]
4 -> [3, 4]
2 -> [3, 4, 2]
* -> [3, 4 * 2]
1 -> [3, 4 * 2, 1]
5 -> [3, 4 * 2, 1, 5]
- -> [3, 4 * 2, 1 - 5]
2 -> [3, 4 * 2, 1 - 5, 2]
3 -> [3, 4 * 2, 1 - 5, 2, 3]
^ -> [3, 4 * 2, 1 - 5, 2 ^ 3]
^ -> [3, 4 * 2, (1 - 5) ^ 2 ^ 3]
/ -> [3, 4 * 2 / (1 - 5) ^ 2 ^ 3]
+ -> [3 + 4 * 2 / (1 - 5) ^ 2 ^ 3]
Infix : 3 + 4 * 2 / (1 - 5) ^ 2 ^ 3

Postfix : 1 2 + 3 4 + ^ 5 6 + ^
1 -> [1]
2 -> [1, 2]
+ -> [1 + 2]
3 -> [1 + 2, 3]
4 -> [1 + 2, 3, 4]
+ -> [1 + 2, 3 + 4]
^ -> [(1 + 2) ^ (3 + 4)]
5 -> [(1 + 2) ^ (3 + 4), 5]
6 -> [(1 + 2) ^ (3 + 4), 5, 6]
+ -> [(1 + 2) ^ (3 + 4), 5 + 6]
^ -> [((1 + 2) ^ (3 + 4)) ^ (5 + 6)]
Infix : ((1 + 2) ^ (3 + 4)) ^ (5 + 6)
```



## Haskell

With a fold, using the accumulator as an expression stack, you can compile a tree that represents the entire expression. Then you can recursively put together the infix string, comparing precedences and left/right associativity to determine when parenthesis are necessary. Note that in this solution, we define addition and multiplication as having no associativity, since no matter which way you associate, they produce the same answer.

This solution is a rough translation of the solution provided on RubyQuiz, as linked above.


```Haskell

data Expression = Const String | Exp Expression String Expression

precedence :: Expression -> Int
precedence (Const _) = 5
precedence (Exp _ op _)
	| op `elem` ["^"]     = 4
	| op `elem` ["*","/"] = 3
	| op `elem` ["+","-"] = 2
	| otherwise = 0

leftAssoc :: Expression -> Bool
leftAssoc (Const _) = False
leftAssoc (Exp _ op _) = op `notElem` ["^","*","+"]

rightAssoc :: Expression -> Bool
rightAssoc (Const _) = False
rightAssoc (Exp _ op _) = op `elem` ["^"]

instance Show Expression where
	show (Const x) = x
	show exp@(Exp l op r) = left++" "++op++" "++right
		where left  = if leftNeedParen then "( "++(show l)++" )" else show l
		      right = if rightNeedParen the "( "++(show r)++" )" else show r
		      leftNeedParen = (leftPrec < opPrec) || ((leftPrec == opPrec) && (rightAssoc exp))
		      rightNeedParen = (rightPrec < opPrec) || ((rightPrec == opPrec) && (leftAssoc exp))
		      leftPrec  = precedence l
		      rightPrec = precedence r
		      opPrec    = precedence exp

buildExp :: [Expression] -> String -> [Expression]
buildExp stack x
	| not.isOp $ x = Const x : stack
	| otherwise    = Exp l x r : rest
		where r:l:rest = stack
		      isOp = (`elem` ["^","*","/","+","-"])

main = do
	contents <- getContents
	mapM_ (print.head.(foldl buildExp []).words) (lines contents)

```


Input:

```txt

3 4 2 * 1 5 - 2 3 ^ ^ / +
1 2 + 3 4 + ^ 5 6 + ^
1 4 + 5 3 + 2 3 * * *
1 2 * 3 4 * *
1 2 + 3 4 + +

```


Output:

```txt

3 + 4 * 2 / ( 1 - 5 ) ^ 2 ^ 3
( ( 1 + 2 ) ^ ( 3 + 4 ) ) ^ ( 5 + 6 )
( 1 + 4 ) * ( 5 + 3 ) * 2 * 3
1 * 2 * 3 * 4
1 + 2 + 3 + 4

```


=={{header|Icon}} and {{header|Unicon}}==

```Icon
procedure main()
every rpn := ![
   "3 4 2 * 1 5 - 2 3 ^ ^ / +",  # reqd
   "1 2 + 3 4 + 5 6 + ^ ^",
   "1 2 + 3 4 + 5 6 + ^ ^",
   "1 2 + 3 4 + ^ 5 6 + ^"       # reqd
   ] do {
      printf("\nRPN   = %i\n",rpn)
      printf("infix = %i\n",RPN2Infix(rpn,show))
      show := 1 # turn off inner working display
      }
end

link printf

procedure RPN2Infix(expr,show)            #: RPN to Infix conversion
static oi
initial {
   oi := table([9,"'"])                   # precedence & associativity
   every oi[!"+-"]   := [2,"l"]             # 2L
   every oi[!"*/"]   := [3,"l"]             # 3L
   oi["^"]           := [4,"r"]             # 4R
   }

   show := if /show then printf else 1    # to show inner workings or not
   stack := []
   expr ? until pos(0) do {               # Reformat as a tree
      tab(many(' '))                         # consume previous seperator
      token := tab(upto(' ')|0)              # get token
      if token := numeric(token) then {      # ... numeric
         push(stack,oi[token]|||[token])
         show("pushed numeric   %i : %s\n",token,list2string(stack))
         }
      else {                                 # ... operator
         every b|a := pop(stack)             # pop & reverse operands
         pr := oi[token,1]
         as := oi[token,2]
         if a[1] < pr | (a[1] = pr & oi[token,2] == "r") then
            a[3] := sprintf("( %s )",a[3])
         if b[1] < pr | (b[1] == pr & oi[token,2] == "l") then
            b[3] := sprintf("( %s )",b[3])
         s := sprintf("%s %s %s",a[3],token,b[3])
         push(stack,[pr,as,s])               # stack [prec, assoc, expr]
         show("applied operator %s : %s\n",token,list2string(stack))
         }
   }

   if *stack ~= 1 then stop("Parser failure")
   return stack[1,3]
end

procedure list2string(L)                  #: format list as a string
   s := "[ "
   every x := !L do
      s ||:= ( if type(x) == "list" then list2string(x)
               else x) || " "
   return s || "]"
end
```


{{libheader|Icon Programming Library}}
[http://www.cs.arizona.edu/icon/library/src/procs/printf.icn printf.icn provides formatting]

Output:
```txt
RPN   = "3 4 2 * 1 5 - 2 3 ^ ^ / +"
pushed numeric   3 : [ [ 9 ' 3 ] ]
pushed numeric   4 : [ [ 9 ' 4 ] [ 9 ' 3 ] ]
pushed numeric   2 : [ [ 9 ' 2 ] [ 9 ' 4 ] [ 9 ' 3 ] ]
applied operator * : [ [ 3 l 4 * 2 ] [ 9 ' 3 ] ]
pushed numeric   1 : [ [ 9 ' 1 ] [ 3 l 4 * 2 ] [ 9 ' 3 ] ]
pushed numeric   5 : [ [ 9 ' 5 ] [ 9 ' 1 ] [ 3 l 4 * 2 ] [ 9 ' 3 ] ]
applied operator - : [ [ 2 l 1 - 5 ] [ 3 l 4 * 2 ] [ 9 ' 3 ] ]
pushed numeric   2 : [ [ 9 ' 2 ] [ 2 l 1 - 5 ] [ 3 l 4 * 2 ] [ 9 ' 3 ] ]
pushed numeric   3 : [ [ 9 ' 3 ] [ 9 ' 2 ] [ 2 l 1 - 5 ] [ 3 l 4 * 2 ] [ 9 ' 3 ] ]
applied operator ^ : [ [ 4 r 2 ^ 3 ] [ 2 l 1 - 5 ] [ 3 l 4 * 2 ] [ 9 ' 3 ] ]
applied operator ^ : [ [ 4 r ( 1 - 5 ) ^ 2 ^ 3 ] [ 3 l 4 * 2 ] [ 9 ' 3 ] ]
applied operator / : [ [ 3 l 4 * 2 / ( 1 - 5 ) ^ 2 ^ 3 ] [ 9 ' 3 ] ]
applied operator + : [ [ 2 l 3 + 4 * 2 / ( 1 - 5 ) ^ 2 ^ 3 ] ]
infix = "3 + 4 * 2 / ( 1 - 5 ) ^ 2 ^ 3"

RPN   = "1 2 + 3 4 + 5 6 + ^ ^"
infix = "( 1 + 2 ) ^ ( 3 + 4 ) ^ ( 5 + 6 )"

RPN   = "1 2 + 3 4 + 5 6 + ^ ^"
infix = "( 1 + 2 ) ^ ( 3 + 4 ) ^ ( 5 + 6 )"

RPN   = "1 2 + 3 4 + ^ 5 6 + ^"
infix = "( ( 1 + 2 ) ^ ( 3 + 4 ) ) ^ ( 5 + 6 )"

```



## J


Implementation:


```j
tokenize=: ' ' <;._1@, deb

ops=: ;:'+ - * / ^'
doOp=: plus`minus`times`divide`exponent`push@.(ops&i.)
parse=:3 :0
  stack=: i.0 2
  for_token.tokenize y do.doOp token end.
  1{:: ,stack
)

parens=:4 :0
  if. y do. '( ',x,' )' else. x end.
)

NB. m: precedence, n: is right associative, y: token
op=:2 :0
  L=. m -   n
  R=. m - -.n
  smoutput;'operation: ';y
  'Lprec left Rprec right'=. ,_2{.stack
  expr=. ;(left parens L > Lprec);' ';y,' ';right parens R > Rprec
  stack=: (_2}.stack),m;expr
  smoutput stack
)

plus=:     2 op 0
minus=:    2 op 0
times=:    3 op 0
divide=:   3 op 0
exponent=: 4 op 1

push=:3 :0
  smoutput;'pushing: ';y
  stack=: stack,_;y
  smoutput stack
)
```


The stack structure has two elements for each stack entry:  expression precedence and expression characters.

Required example:


```j
   parse '3 4 2 * 1 5 - 2 3 ^ ^ / +'
pushing: 3
+-+-+
|_|3|
+-+-+
pushing: 4
+-+-+
|_|3|
+-+-+
|_|4|
+-+-+
pushing: 2
+-+-+
|_|3|
+-+-+
|_|4|
+-+-+
|_|2|
+-+-+
operation: *
+-+-----+
|_|3    |
+-+-----+
|3|4 * 2|
+-+-----+
pushing: 1
+-+-----+
|_|3    |
+-+-----+
|3|4 * 2|
+-+-----+
|_|1    |
+-+-----+
pushing: 5
+-+-----+
|_|3    |
+-+-----+
|3|4 * 2|
+-+-----+
|_|1    |
+-+-----+
|_|5    |
+-+-----+
operation: -
+-+-----+
|_|3    |
+-+-----+
|3|4 * 2|
+-+-----+
|2|1 - 5|
+-+-----+
pushing: 2
+-+-----+
|_|3    |
+-+-----+
|3|4 * 2|
+-+-----+
|2|1 - 5|
+-+-----+
|_|2    |
+-+-----+
pushing: 3
+-+-----+
|_|3    |
+-+-----+
|3|4 * 2|
+-+-----+
|2|1 - 5|
+-+-----+
|_|2    |
+-+-----+
|_|3    |
+-+-----+
operation: ^
+-+-----+
|_|3    |
+-+-----+
|3|4 * 2|
+-+-----+
|2|1 - 5|
+-+-----+
|4|2 ^ 3|
+-+-----+
operation: ^
+-+-----------------+
|_|3                |
+-+-----------------+
|3|4 * 2            |
+-+-----------------+
|4|( 1 - 5 ) ^ 2 ^ 3|
+-+-----------------+
operation: /
+-+-------------------------+
|_|3                        |
+-+-------------------------+
|3|4 * 2 / ( 1 - 5 ) ^ 2 ^ 3|
+-+-------------------------+
operation: +
+-+-----------------------------+
|2|3 + 4 * 2 / ( 1 - 5 ) ^ 2 ^ 3|
+-+-----------------------------+
3 + 4 * 2 / ( 1 - 5 ) ^ 2 ^ 3
```



## Java

{{works with|Java|7}}

```java
import java.util.Stack;

public class PostfixToInfix {

    public static void main(String[] args) {
        for (String e : new String[]{"3 4 2 * 1 5 - 2 3 ^ ^ / +",
            "1 2 + 3 4 + ^ 5 6 + ^"}) {
            System.out.printf("Postfix : %s%n", e);
            System.out.printf("Infix : %s%n", postfixToInfix(e));
            System.out.println();
        }
    }

    static String postfixToInfix(final String postfix) {

        class Expression {
            final static String ops = "-+/*^";

            String op, ex;
            int prec = 3;

            Expression(String e) {
                ex = e;
            }

            Expression(String e1, String e2, String o) {
                ex = String.format("%s %s %s", e1, o, e2);
                op = o;
                prec = ops.indexOf(o) / 2;
            }

            @Override
            public String toString() {
                return ex;
            }
        }

        Stack<Expression> expr = new Stack<>();

        for (String token : postfix.split("\\s+")) {
            char c = token.charAt(0);
            int idx = Expression.ops.indexOf(c);
            if (idx != -1 && token.length() == 1) {

                Expression r = expr.pop();
                Expression l = expr.pop();

                int opPrec = idx / 2;

                if (l.prec < opPrec || (l.prec == opPrec && c == '^'))
                    l.ex = '(' + l.ex + ')';

                if (r.prec < opPrec || (r.prec == opPrec && c != '^'))
                    r.ex = '(' + r.ex + ')';

                expr.push(new Expression(l.ex, r.ex, token));
            } else {
                expr.push(new Expression(token));
            }
            System.out.printf("%s -> %s%n", token, expr);
        }
        return expr.peek().ex;
    }
}
```


Output:


```txt
Postfix : 3 4 2 * 1 5 - 2 3 ^ ^ / +
3 -> [3]
4 -> [3, 4]
2 -> [3, 4, 2]
* -> [3, 4 * 2]
1 -> [3, 4 * 2, 1]
5 -> [3, 4 * 2, 1, 5]
- -> [3, 4 * 2, 1 - 5]
2 -> [3, 4 * 2, 1 - 5, 2]
3 -> [3, 4 * 2, 1 - 5, 2, 3]
^ -> [3, 4 * 2, 1 - 5, 2 ^ 3]
^ -> [3, 4 * 2, (1 - 5) ^ 2 ^ 3]
/ -> [3, 4 * 2 / (1 - 5) ^ 2 ^ 3]
+ -> [3 + 4 * 2 / (1 - 5) ^ 2 ^ 3]
Infix : 3 + 4 * 2 / (1 - 5) ^ 2 ^ 3

Postfix : 1 2 + 3 4 + ^ 5 6 + ^
1 -> [1]
2 -> [1, 2]
+ -> [1 + 2]
3 -> [1 + 2, 3]
4 -> [1 + 2, 3, 4]
+ -> [1 + 2, 3 + 4]
^ -> [(1 + 2) ^ (3 + 4)]
5 -> [(1 + 2) ^ (3 + 4), 5]
6 -> [(1 + 2) ^ (3 + 4), 5, 6]
+ -> [(1 + 2) ^ (3 + 4), 5 + 6]
^ -> [((1 + 2) ^ (3 + 4)) ^ (5 + 6)]
Infix : ((1 + 2) ^ (3 + 4)) ^ (5 + 6)
```



## JavaScript

Needs EcmaScript 6 support (e.g. Chrome).


```javascript
const Associativity = {
    /** a / b / c = (a / b) / c */
    left: 0,
    /** a ^ b ^ c = a ^ (b ^ c) */
    right: 1,
    /** a + b + c = (a + b) + c = a + (b + c) */
    both: 2,
};
const operators = {
    '+': { precedence: 2, associativity: Associativity.both },
    '-': { precedence: 2, associativity: Associativity.left },
    '*': { precedence: 3, associativity: Associativity.both },
    '/': { precedence: 3, associativity: Associativity.left },
    '^': { precedence: 4, associativity: Associativity.right },
};
class NumberNode {
    constructor(text) { this.text = text; }
    toString() { return this.text; }
}
class InfixNode {
    constructor(fnname, operands) {
        this.fnname = fnname;
        this.operands = operands;
    }
    toString(parentPrecedence = 0) {
        const op = operators[this.fnname];
        const leftAdd = op.associativity === Associativity.right ? 0.01 : 0;
        const rightAdd = op.associativity === Associativity.left ? 0.01 : 0;
        if (this.operands.length !== 2) throw Error("invalid operand count");
        const result = this.operands[0].toString(op.precedence + leftAdd)
            +` ${this.fnname} ${this.operands[1].toString(op.precedence + rightAdd)}`;
        if (parentPrecedence > op.precedence) return `( ${result} )`;
        else return result;
    }
}
function rpnToTree(tokens) {
    const stack = [];
    console.log(`input = ${tokens}`);
    for (const token of tokens.split(" ")) {
        if (token in operators) {
            const op = operators[token], arity = 2; // all of these operators take 2 arguments
            if (stack.length < arity) throw Error("stack error");
            stack.push(new InfixNode(token, stack.splice(stack.length - arity)));
        }
        else stack.push(new NumberNode(token));
        console.log(`read ${token}, stack = [${stack.join(", ")}]`);
    }
    if (stack.length !== 1) throw Error("stack error " + stack);
    return stack[0];
}
const tests = [
    ["3 4 2 * 1 5 - 2 3 ^ ^ / +", "3 + 4 * 2 / ( 1 - 5 ) ^ 2 ^ 3"],
    ["1 2 + 3 4 + ^ 5 6 + ^", "( ( 1 + 2 ) ^ ( 3 + 4 ) ) ^ ( 5 + 6 )"],
    ["1 2 3 + +", "1 + 2 + 3"] // test associativity (1+(2+3)) == (1+2+3)
];
for (const [inp, oup] of tests) {
    const realOup = rpnToTree(inp).toString();
    console.log(realOup === oup ? "Correct!" : "Incorrect!");
}
```


Output:

```txt
input = 3 4 2 * 1 5 - 2 3 ^ ^ / +
read 3, stack = [3]
read 4, stack = [3, 4]
read 2, stack = [3, 4, 2]
read *, stack = [3, 4 * 2]
read 1, stack = [3, 4 * 2, 1]
read 5, stack = [3, 4 * 2, 1, 5]
read -, stack = [3, 4 * 2, 1 - 5]
read 2, stack = [3, 4 * 2, 1 - 5, 2]
read 3, stack = [3, 4 * 2, 1 - 5, 2, 3]
read ^, stack = [3, 4 * 2, 1 - 5, 2 ^ 3]
read ^, stack = [3, 4 * 2, ( 1 - 5 ) ^ 2 ^ 3]
read /, stack = [3, 4 * 2 / ( 1 - 5 ) ^ 2 ^ 3]
read +, stack = [3 + 4 * 2 / ( 1 - 5 ) ^ 2 ^ 3]
Correct!
input = 1 2 + 3 4 + ^ 5 6 + ^
read 1, stack = [1]
read 2, stack = [1, 2]
read +, stack = [1 + 2]
read 3, stack = [1 + 2, 3]
read 4, stack = [1 + 2, 3, 4]
read +, stack = [1 + 2, 3 + 4]
read ^, stack = [( 1 + 2 ) ^ ( 3 + 4 )]
read 5, stack = [( 1 + 2 ) ^ ( 3 + 4 ), 5]
read 6, stack = [( 1 + 2 ) ^ ( 3 + 4 ), 5, 6]
read +, stack = [( 1 + 2 ) ^ ( 3 + 4 ), 5 + 6]
read ^, stack = [( ( 1 + 2 ) ^ ( 3 + 4 ) ) ^ ( 5 + 6 )]
Correct!
input = 1 2 3 + +
read 1, stack = [1]
read 2, stack = [1, 2]
read 3, stack = [1, 2, 3]
read +, stack = [1, 2 + 3]
read +, stack = [1 + 2 + 3]
Correct!
```



## Julia


```julia

function parseRPNstring(rpns)
    infix = []
    rpn = split(rpns)
    for tok in rpn
        if all(isnumber, tok)
            push!(infix, parse(Int, tok))
        else
            last = pop!(infix)
            prev = pop!(infix)
            push!(infix, Expr(:call, Symbol(tok), prev, last))
            println("Current step: $infix")
        end
    end
    infix
end

unany(s) = replace(string(s), r"Any\[:\((.+)\)\]", s"\1")

println("The final infix result: ", parseRPNstring("3 4 2 * 1 5 - 2 3 ^ ^ / +") |> unany, "\n")
println("The final infix result: ", parseRPNstring("1 2 + 3 4 + ^ 5 6 + ^") |> unany)

```

{{output}}

```txt

Current step: Any[3, :(4 * 2)]
Current step: Any[3, :(4 * 2), :(1 - 5)]
Current step: Any[3, :(4 * 2), :(1 - 5), :(2 ^ 3)]
Current step: Any[3, :(4 * 2), :((1 - 5) ^ (2 ^ 3))]
Current step: Any[3, :((4 * 2) / (1 - 5) ^ (2 ^ 3))]
Current step: Any[:(3 + (4 * 2) / (1 - 5) ^ (2 ^ 3))]
The final infix result: 3 + (4 * 2) / (1 - 5) ^ (2 ^ 3)

Current step: Any[:(1 + 2)]
Current step: Any[:(1 + 2), :(3 + 4)]
Current step: Any[:((1 + 2) ^ (3 + 4))]
Current step: Any[:((1 + 2) ^ (3 + 4)), :(5 + 6)]
Current step: Any[:(((1 + 2) ^ (3 + 4)) ^ (5 + 6))]
The final infix result: ((1 + 2) ^ (3 + 4)) ^ (5 + 6)

```



## Kotlin

{{trans|Java}}

```scala
// version 1.2.0

import java.util.Stack

class Expression(var ex: String, val op: String = "", val prec: Int = 3) {

    constructor(e1: String, e2: String, o: String) :
        this("$e1 $o $e2", o, OPS.indexOf(o) / 2)

    override fun toString() = ex

    companion object {
        const val OPS = "-+/*^"
    }
}

fun postfixToInfix(postfix: String): String {
    val expr = Stack<Expression>()
    val rx = Regex("""\s+""")
    for (token in postfix.split(rx)) {
        val c = token[0]
        val idx = Expression.OPS.indexOf(c)
        if (idx != -1 && token.length == 1) {
            val r = expr.pop()
            val l = expr.pop()
            val opPrec = idx / 2
            if (l.prec < opPrec || (l.prec == opPrec && c == '^')) {
                l.ex = "(${l.ex})"
            }
            if (r.prec < opPrec || (r.prec == opPrec && c != '^')) {
                r.ex = "(${r.ex})"
            }
            expr.push(Expression(l.ex, r.ex, token))
        }
        else {
            expr.push(Expression(token))
        }
        println("$token -> $expr")
    }
    return expr.peek().ex
}

fun main(args: Array<String>) {
    val es = listOf(
        "3 4 2 * 1 5 - 2 3 ^ ^ / +",
        "1 2 + 3 4 + ^ 5 6 + ^"
    )
    for (e in es) {
        println("Postfix : $e")
        println("Infix : ${postfixToInfix(e)}\n")
    }
}
```


{{out}}

```txt

Postfix : 3 4 2 * 1 5 - 2 3 ^ ^ / +
3 -> [3]
4 -> [3, 4]
2 -> [3, 4, 2]
* -> [3, 4 * 2]
1 -> [3, 4 * 2, 1]
5 -> [3, 4 * 2, 1, 5]
- -> [3, 4 * 2, 1 - 5]
2 -> [3, 4 * 2, 1 - 5, 2]
3 -> [3, 4 * 2, 1 - 5, 2, 3]
^ -> [3, 4 * 2, 1 - 5, 2 ^ 3]
^ -> [3, 4 * 2, (1 - 5) ^ 2 ^ 3]
/ -> [3, 4 * 2 / (1 - 5) ^ 2 ^ 3]
+ -> [3 + 4 * 2 / (1 - 5) ^ 2 ^ 3]
Infix : 3 + 4 * 2 / (1 - 5) ^ 2 ^ 3

Postfix : 1 2 + 3 4 + ^ 5 6 + ^
1 -> [1]
2 -> [1, 2]
+ -> [1 + 2]
3 -> [1 + 2, 3]
4 -> [1 + 2, 3, 4]
+ -> [1 + 2, 3 + 4]
^ -> [(1 + 2) ^ (3 + 4)]
5 -> [(1 + 2) ^ (3 + 4), 5]
6 -> [(1 + 2) ^ (3 + 4), 5, 6]
+ -> [(1 + 2) ^ (3 + 4), 5 + 6]
^ -> [((1 + 2) ^ (3 + 4)) ^ (5 + 6)]
Infix : ((1 + 2) ^ (3 + 4)) ^ (5 + 6)

```


## M2000 Interpreter


```M2000 Interpreter

Module Rpn_2_Infix {
	Rem Form 80,60
	function rpn_to_infix$(a$) {
		def m=0
		inventory precendence="-":=2,"+":=2,"*":=3,"/":=3,"^":=4
		dim token$()
		token$()=piece$(a$," ")
		l=len(token$())
		dim type(l)=0, right(l)=0, infix$(l)
		infix=-1
		for i=0 to  l-1
			if exist(precendence, token$(i)) then
				type(i)=precendence(token$(i))
				if type(i)=4 then right(i)=-1
			end if
			if type(i)=0 then
				infix++
				infix$(infix)=token$(i)
				type(infix)=100
			else
				if right(i) then
					if type(infix)<type(i) then infix$(infix)="("+infix$(infix)+")"
					if type(infix-1)<100 then infix$(infix-1)="("+infix$(infix-1)+")"
					infix$(infix-1)=infix$(infix-1)+token$(i)+infix$(infix)
				else
					if type(infix)<type(i) then infix$(infix)="("+infix$(infix)+")"
					if type(infix-1)<type(i) then
						infix$(infix-1)="("+infix$(infix-1)+")"+token$(i)+infix$(infix)
					else
						infix$(infix-1)=infix$(infix-1)+token$(i)+infix$(infix)
					end if
				end if
				type(infix-1)=type(i)
				infix--
			end if
			inf=each(infix$(),1, infix+1)
			while inf
				export$<=token$(i)+" ["+str$(inf^,"")+"] "+ array$(inf)+{
				}
				token$(i)=" "
			end while
		next i
		=infix$(0)
	}
	Global export$
	document export$
	example1=rpn_to_infix$("3 4 2 * 1 5 - 2 3 ^ ^ / +")="3+4*2/(1-5)^2^3"
	example2=rpn_to_infix$("1 2 + 3 4 + ^ 5 6 + ^")="((1+2)^(3+4))^(5+6)"
	\\ a test from Phix example
	example3=rpn_to_infix$("moon stars mud + * fire soup * ^")="(moon*(stars+mud))^(fire*soup)"
	Print example1, example2, example3
	Rem Print #-2, Export$
	ClipBoard Export$

}
Rpn_2_Infix

```


{{out}}
<pre style="height:30ex;overflow:scroll">
3 [0] 3
4 [0] 3
  [1] 4
2 [0] 3
  [1] 4
  [2] 2
* [0] 3
  [1] 4*2
1 [0] 3
  [1] 4*2
  [2] 1
5 [0] 3
  [1] 4*2
  [2] 1
  [3] 5
- [0] 3
  [1] 4*2
  [2] 1-5
2 [0] 3
  [1] 4*2
  [2] 1-5
  [3] 2
3 [0] 3
  [1] 4*2
  [2] 1-5
  [3] 2
  [4] 3
^ [0] 3
  [1] 4*2
  [2] 1-5
  [3] 2^3
^ [0] 3
  [1] 4*2
  [2] (1-5)^2^3
/ [0] 3
  [1] 4*2/(1-5)^2^3
+ [0] 3+4*2/(1-5)^2^3
1 [0] 1
2 [0] 1
  [1] 2
+ [0] 1+2
3 [0] 1+2
  [1] 3
4 [0] 1+2
  [1] 3
  [2] 4
+ [0] 1+2
  [1] 3+4
^ [0] (1+2)^(3+4)
5 [0] (1+2)^(3+4)
  [1] 5
6 [0] (1+2)^(3+4)
  [1] 5
  [2] 6
+ [0] (1+2)^(3+4)
  [1] 5+6
^ [0] ((1+2)^(3+4))^(5+6)
moon [0] moon
stars [0] moon
  [1] stars
mud [0] moon
  [1] stars
  [2] mud
+ [0] moon
  [1] stars+mud
* [0] moon*(stars+mud)
fire [0] moon*(stars+mud)
  [1] fire
soup [0] moon*(stars+mud)
  [1] fire
  [2] soup
* [0] moon*(stars+mud)
  [1] fire*soup
^ [0] (moon*(stars+mud))^(fire*soup)
</pre >


## Nim

{{trans|Go}}

```nim
import tables, strutils

const nPrec = 9

let ops: Table[string, tuple[prec: int, rAssoc: bool]] =
  { "^": (4, true)
  , "*": (3, false)
  , "/": (3, false)
  , "+": (2, false)
  , "-": (2, false)
  }.toTable

proc parseRPN(e: string) =
  echo "postfix: ", e
  var stack = newSeq[tuple[prec: int, expr: string]]()

  for tok in e.split:
    echo "Token: ", tok
    if ops.hasKey tok:
      let op = ops[tok]
      let rhs = stack.pop
      var lhs = stack.pop

      if lhs.prec < op.prec or (lhs.prec == op.prec and op.rAssoc):
        lhs.expr = "(" & lhs.expr & ")"

      lhs.expr.add " " & tok & " "

      if rhs.prec < op.prec or (rhs.prec == op.prec and not op.rAssoc):
        lhs.expr.add "(" & rhs.expr & ")"
      else:
        lhs.expr.add rhs.expr

      lhs.prec = op.prec
      stack.add lhs
    else:
      stack.add((nPrec, tok))

    for f in stack:
      echo "    ", f.prec, " ", f.expr

  echo "infix: ", stack[0].expr

for test in ["3 4 2 * 1 5 - 2 3 ^ ^ / +", "1 2 + 3 4 + ^ 5 6 + ^"]:
  test.parseRPN
```

Output:

```txt
postfix: 3 4 2 * 1 5 - 2 3 ^ ^ / +
Token: 3
    9 3
Token: 4
    9 3
    9 4
Token: 2
    9 3
    9 4
    9 2
Token: *
    9 3
    3 4 * 2
Token: 1
    9 3
    3 4 * 2
    9 1
Token: 5
    9 3
    3 4 * 2
    9 1
    9 5
Token: -
    9 3
    3 4 * 2
    2 1 - 5
Token: 2
    9 3
    3 4 * 2
    2 1 - 5
    9 2
Token: 3
    9 3
    3 4 * 2
    2 1 - 5
    9 2
    9 3
Token: ^
    9 3
    3 4 * 2
    2 1 - 5
    4 2 ^ 3
Token: ^
    9 3
    3 4 * 2
    4 (1 - 5) ^ 2 ^ 3
Token: /
    9 3
    3 4 * 2 / (1 - 5) ^ 2 ^ 3
Token: +
    2 3 + 4 * 2 / (1 - 5) ^ 2 ^ 3
infix: 3 + 4 * 2 / (1 - 5) ^ 2 ^ 3
postfix: 1 2 + 3 4 + ^ 5 6 + ^
Token: 1
    9 1
Token: 2
    9 1
    9 2
Token: +
    2 1 + 2
Token: 3
    2 1 + 2
    9 3
Token: 4
    2 1 + 2
    9 3
    9 4
Token: +
    2 1 + 2
    2 3 + 4
Token: ^
    4 (1 + 2) ^ (3 + 4)
Token: 5
    4 (1 + 2) ^ (3 + 4)
    9 5
Token: 6
    4 (1 + 2) ^ (3 + 4)
    9 5
    9 6
Token: +
    4 (1 + 2) ^ (3 + 4)
    2 5 + 6
Token: ^
    4 ((1 + 2) ^ (3 + 4)) ^ (5 + 6)
infix: ((1 + 2) ^ (3 + 4)) ^ (5 + 6)
```



## Perl


```perl
use strict;
use warnings;
use feature 'say';

my $number   = '[+-/$]?(?:\.\d+|\d+(?:\.\d*)?)';
my $operator = '[-+*/^]';

my @tests = ('1 2 + 3 4 + ^ 5 6 + ^', '3 4 2 * 1 5 - 2 3 ^ ^ / +');

for (@tests) {
    my(@elems,$n);
    $n = -1;
    while (
        s/
            \s* (?<left>$number)   # 1st operand (will be 'left'  in infix)
            \s+ (?<right>$number)  # 2nd operand (will be 'right' in infix)
            \s+ (?<op>$operator)   # operator
            (?:\s+|$)              # more to parse, or done?
         /
            ' '.('$'.++$n).' '     # placeholders
         /ex) {
            $elems[$n] = "($+{left}$+{op}$+{right})"  # infix expression
         }
    while (
        s/ (\$)(\d+)    # for each placeholder
         / $elems[$2]   # evaluate expression, substitute numeric value
         /ex
        ) { say }       # track progress
    say '=>' . substr($_,2,-2)."\n";
}
```

{{out}}

```txt
 ($2^$3)
 (($0^$1)^$3)
 (((1+2)^$1)^$3)
 (((1+2)^(3+4))^$3)
 (((1+2)^(3+4))^(5+6))
=>((1+2)^(3+4))^(5+6)

 (3+$4)
 (3+($0/$3))
 (3+((4*2)/$3))
 (3+((4*2)/($1^$2)))
 (3+((4*2)/((1-5)^$2)))
 (3+((4*2)/((1-5)^(2^3))))
=>3+((4*2)/((1-5)^(2^3)))
```



## Perl 6


```perl6
my @tests = '3 4 2 * 1 5 - 2 3 ^ ^ / +',
            '1 2 + 3 4 + ^ 5 6 + ^';

say rpm-to-infix($_) for @tests;

sub p ($pair, $prec) {
    $pair.key < $prec ?? "( $pair.value() )" !! $pair.value
}
sub rpm-to-infix($string) {
    say "
### ===========
\n$string";
    my @stack;
    for $string.words {
        when /\d/ { @stack.push: 9 => $_ }
        my $y = @stack.pop;
        my $x = @stack.pop;
        when '^'       { @stack.push: 4 => ~(p($x,5), $_, p($y,4)) }
        when '*' | '/' { @stack.push: 3 => ~(p($x,3), $_, p($y,3)) }
        when '+' | '-' { @stack.push: 2 => ~(p($x,2), $_, p($y,2)) }
        # LEAVE { say @stack } # phaser not yet implemented in this context
    }
    say "-----------------";
    @stack».value;
}
```

{{out}}

```txt

### ===========

3 4 2 * 1 5 - 2 3 ^ ^ / +
-----------------
3 + 4 * 2 / ( 1 - 5 ) ^ 2 ^ 3

### ===========

1 2 + 3 4 + ^ 5 6 + ^
-----------------
( ( 1 + 2 ) ^ ( 3 + 4 ) ) ^ ( 5 + 6 )
```



## Phix


```Phix
integer show_workings = 1

constant operators  = {"^","*","/","+","-"},
         precedence = { 4,  3,  3,  2,  2 },
         rassoc     = {'r', 0 ,'l', 0 ,'l'}

procedure parseRPN(string expr, string expected)
sequence stack = {}
sequence ops = split(expr)
string lhs, rhs
integer lprec,rprec
    printf(1,"Postfix input: %-30s%s", {expr,iff(show_workings?'\n':'\t')})
    if length(ops)=0 then ?"error" return end if
    for i=1 to length(ops) do
        string op = ops[i]
        integer k = find(op,operators)
        if k=0 then
            stack = append(stack,{9,op})
        else
            if length(stack)<2 then ?"error" return end if
            {rprec,rhs} = stack[$]; stack = stack[1..$-1]
            {lprec,lhs} = stack[$]
            integer prec = precedence[k]
            integer assoc = rassoc[k]
            if lprec<prec or (lprec=prec and assoc='r') then
                lhs = "("&lhs&")"
            end if
            if rprec<prec or (rprec=prec and assoc='l') then
                rhs = "("&rhs&")"
            end if
            stack[$] = {prec,lhs&" "&op&" "&rhs}
        end if
        if show_workings then
            ?{op,stack}
        end if
    end for
    string res = stack[1][2]
    printf(1,"Infix result: %s [%s]\n", {res,iff(res=expected?"ok","**ERROR**")})
end procedure

parseRPN("3 4 2 * 1 5 - 2 3 ^ ^ / +","3 + 4 * 2 / (1 - 5) ^ 2 ^ 3")
show_workings = 0
parseRPN("1 2 + 3 4 + ^ 5 6 + ^","((1 + 2) ^ (3 + 4)) ^ (5 + 6)")
parseRPN("1 2 + 3 4 + 5 6 + ^ ^","(1 + 2) ^ (3 + 4) ^ (5 + 6)")
parseRPN("moon stars mud + * fire soup * ^","(moon * (stars + mud)) ^ (fire * soup)")
parseRPN("3 4 ^ 2 9 ^ ^ 2 5 ^ ^","((3 ^ 4) ^ 2 ^ 9) ^ 2 ^ 5")
parseRPN("5 6 * * + +","error")
parseRPN("","error")
parseRPN("1 4 + 5 3 + 2 3 * * *","(1 + 4) * (5 + 3) * 2 * 3")
parseRPN("1 2 * 3 4 * *","1 * 2 * 3 * 4")
parseRPN("1 2 + 3 4 + +","1 + 2 + 3 + 4")
parseRPN("1 2 + 3 4 + ^","(1 + 2) ^ (3 + 4)")
parseRPN("5 6 ^ 7 ^","(5 ^ 6) ^ 7")
parseRPN("5 4 3 2 ^ ^ ^","5 ^ 4 ^ 3 ^ 2")
parseRPN("1 2 3 + +","1 + 2 + 3")
parseRPN("1 2 + 3 +","1 + 2 + 3")
parseRPN("1 2 3 ^ ^","1 ^ 2 ^ 3")
parseRPN("1 2 ^ 3 ^","(1 ^ 2) ^ 3")
parseRPN("1 1 - 3 +","1 - 1 + 3")
parseRPN("3 1 1 - +","3 + 1 - 1")           -- [txr says 3 + (1 - 1)]
parseRPN("1 2 3 + -","1 - (2 + 3)")
parseRPN("4 3 2 + +","4 + 3 + 2")
parseRPN("5 4 3 2 + + +","5 + 4 + 3 + 2")
parseRPN("5 4 3 2 * * *","5 * 4 * 3 * 2")
parseRPN("5 4 3 2 + - +","5 + 4 - (3 + 2)") -- [python says 5 + (4 - (3 + 2))]
parseRPN("3 4 5 * -","3 - 4 * 5")
parseRPN("3 4 5 - *","3 * (4 - 5)")         -- [python says (3 - 4) * 5] [!!flagged!!]
parseRPN("3 4 - 5 *","(3 - 4) * 5")
parseRPN("4 2 * 1 5 - +","4 * 2 + 1 - 5")   -- [python says 4 * 2 + (1 - 5)]
parseRPN("4 2 * 1 5 - 2 ^ /","4 * 2 / (1 - 5) ^ 2")
parseRPN("3 4 2 * 1 5 - 2 3 ^ ^ / +","3 + 4 * 2 / (1 - 5) ^ 2 ^ 3")
```

{{out}}

```txt

Postfix input: 3 4 2 * 1 5 - 2 3 ^ ^ / +
{"3",{{9,"3"}}}
{"4",{{9,"3"},{9,"4"}}}
{"2",{{9,"3"},{9,"4"},{9,"2"}}}
{"*",{{9,"3"},{3,"4 * 2"}}}
{"1",{{9,"3"},{3,"4 * 2"},{9,"1"}}}
{"5",{{9,"3"},{3,"4 * 2"},{9,"1"},{9,"5"}}}
{"-",{{9,"3"},{3,"4 * 2"},{2,"1 - 5"}}}
{"2",{{9,"3"},{3,"4 * 2"},{2,"1 - 5"},{9,"2"}}}
{"3",{{9,"3"},{3,"4 * 2"},{2,"1 - 5"},{9,"2"},{9,"3"}}}
{"^",{{9,"3"},{3,"4 * 2"},{2,"1 - 5"},{4,"2 ^ 3"}}}
{"^",{{9,"3"},{3,"4 * 2"},{4,"(1 - 5) ^ 2 ^ 3"}}}
{"/",{{9,"3"},{3,"4 * 2 / (1 - 5) ^ 2 ^ 3"}}}
{"+",{{2,"3 + 4 * 2 / (1 - 5) ^ 2 ^ 3"}}}
Infix result: 3 + 4 * 2 / (1 - 5) ^ 2 ^ 3 [ok]
Postfix input: 1 2 + 3 4 + ^ 5 6 + ^            Infix result: ((1 + 2) ^ (3 + 4)) ^ (5 + 6) [ok]
Postfix input: 1 2 + 3 4 + 5 6 + ^ ^            Infix result: (1 + 2) ^ (3 + 4) ^ (5 + 6) [ok]
Postfix input: moon stars mud + * fire soup * ^ Infix result: (moon * (stars + mud)) ^ (fire * soup) [ok]
Postfix input: 3 4 ^ 2 9 ^ ^ 2 5 ^ ^            Infix result: ((3 ^ 4) ^ 2 ^ 9) ^ 2 ^ 5 [ok]
Postfix input: 5 6 * * + +                      "error"
Postfix input:                                  "error"
Postfix input: 1 4 + 5 3 + 2 3 * * *            Infix result: (1 + 4) * (5 + 3) * 2 * 3 [ok]
Postfix input: 1 2 * 3 4 * *                    Infix result: 1 * 2 * 3 * 4 [ok]
Postfix input: 1 2 + 3 4 + +                    Infix result: 1 + 2 + 3 + 4 [ok]
Postfix input: 1 2 + 3 4 + ^                    Infix result: (1 + 2) ^ (3 + 4) [ok]
Postfix input: 5 6 ^ 7 ^                        Infix result: (5 ^ 6) ^ 7 [ok]
Postfix input: 5 4 3 2 ^ ^ ^                    Infix result: 5 ^ 4 ^ 3 ^ 2 [ok]
Postfix input: 1 2 3 + +                        Infix result: 1 + 2 + 3 [ok]
Postfix input: 1 2 + 3 +                        Infix result: 1 + 2 + 3 [ok]
Postfix input: 1 2 3 ^ ^                        Infix result: 1 ^ 2 ^ 3 [ok]
Postfix input: 1 2 ^ 3 ^                        Infix result: (1 ^ 2) ^ 3 [ok]
Postfix input: 1 1 - 3 +                        Infix result: 1 - 1 + 3 [ok]
Postfix input: 3 1 1 - +                        Infix result: 3 + 1 - 1 [ok]
Postfix input: 1 2 3 + -                        Infix result: 1 - (2 + 3) [ok]
Postfix input: 4 3 2 + +                        Infix result: 4 + 3 + 2 [ok]
Postfix input: 5 4 3 2 + + +                    Infix result: 5 + 4 + 3 + 2 [ok]
Postfix input: 5 4 3 2 * * *                    Infix result: 5 * 4 * 3 * 2 [ok]
Postfix input: 5 4 3 2 + - +                    Infix result: 5 + 4 - (3 + 2) [ok]
Postfix input: 3 4 5 * -                        Infix result: 3 - 4 * 5 [ok]
Postfix input: 3 4 5 - *                        Infix result: 3 * (4 - 5) [ok]
Postfix input: 3 4 - 5 *                        Infix result: (3 - 4) * 5 [ok]
Postfix input: 4 2 * 1 5 - +                    Infix result: 4 * 2 + 1 - 5 [ok]
Postfix input: 4 2 * 1 5 - 2 ^ /                Infix result: 4 * 2 / (1 - 5) ^ 2 [ok]
Postfix input: 3 4 2 * 1 5 - 2 3 ^ ^ / +        Infix result: 3 + 4 * 2 / (1 - 5) ^ 2 ^ 3 [ok]

```



## PicoLisp

We maintain a stack of cons pairs, consisting of precedences and partial expressions. Numbers get a "highest" precedence of '9'.

```PicoLisp
(de leftAssoc (Op)
   (member Op '("*" "/" "+" "-")) )

(de precedence (Op)
   (case Op
      ("\^" 4)
      (("*" "/") 3)
      (("+" "-") 2) ) )

(de rpnToInfix (Str)
   (let Stack NIL
      (prinl "Token  Stack")
      (for Token (str Str "_")
         (cond
            ((num? Token) (push 'Stack (cons 9 @)))  # Highest precedence
            ((not (cdr Stack)) (quit "Stack empty"))
            (T
               (let (X (pop 'Stack)  P (precedence Token))
                  (set Stack
                     (cons P
                        (pack
                           (if ((if (leftAssoc Token) < <=) (caar Stack) P)
                              (pack "(" (cdar Stack) ")")
                              (cdar Stack) )
                           " " Token " "
                           (if ((if (leftAssoc Token) <= <) (car X) P)
                              (pack "(" (cdr X) ")")
                              (cdr X) ) ) ) ) ) ) )
         (prin Token)
         (space 6)
         (println Stack) )
      (prog1 (cdr (pop 'Stack))
         (and Stack (quit "Garbage remained on stack")) ) ) )
```

Test (note that the top-of-stack is in the left-most position):

```PicoLisp
: (rpnToInfix "3 4 2 * 1 5 - 2 3 \^ \^ / +")
Token  Stack
3      ((9 . 3))
4      ((9 . 4) (9 . 3))
2      ((9 . 2) (9 . 4) (9 . 3))
*      ((3 . "4 * 2") (9 . 3))
1      ((9 . 1) (3 . "4 * 2") (9 . 3))
5      ((9 . 5) (9 . 1) (3 . "4 * 2") (9 . 3))
-      ((2 . "1 - 5") (3 . "4 * 2") (9 . 3))
2      ((9 . 2) (2 . "1 - 5") (3 . "4 * 2") (9 . 3))
3      ((9 . 3) (9 . 2) (2 . "1 - 5") (3 . "4 * 2") (9 . 3))
^      ((4 . "2 \^ 3") (2 . "1 - 5") (3 . "4 * 2") (9 . 3))
^      ((4 . "(1 - 5) \^ 2 \^ 3") (3 . "4 * 2") (9 . 3))
/      ((3 . "4 * 2 / (1 - 5) \^ 2 \^ 3") (9 . 3))
+      ((2 . "3 + 4 * 2 / (1 - 5) \^ 2 \^ 3"))
-> "3 + 4 * 2 / (1 - 5) \^ 2 \^ 3"

: (rpnToInfix "1 2 + 3 4 + \^ 5 6 + \^")
Token  Stack
1      ((9 . 1))
2      ((9 . 2) (9 . 1))
+      ((2 . "1 + 2"))
3      ((9 . 3) (2 . "1 + 2"))
4      ((9 . 4) (9 . 3) (2 . "1 + 2"))
+      ((2 . "3 + 4") (2 . "1 + 2"))
^      ((4 . "(1 + 2) \^ (3 + 4)"))
5      ((9 . 5) (4 . "(1 + 2) \^ (3 + 4)"))
6      ((9 . 6) (9 . 5) (4 . "(1 + 2) \^ (3 + 4)"))
+      ((2 . "5 + 6") (4 . "(1 + 2) \^ (3 + 4)"))
^      ((4 . "((1 + 2) \^ (3 + 4)) \^ (5 + 6)"))
-> "((1 + 2) \^ (3 + 4)) \^ (5 + 6)"
```



## PL/I


```PL/I

/* Uses a push-down pop-up stack for the stack (instead of array) */
cvt: procedure options (main);                /* 10 Sept. 2012 */
   declare (true initial ('1'b), false initial ('0'b) ) bit (1);
   declare list character (1) controlled, written bit (1) controlled;
   declare (RPN, out) character (100) varying;
   declare s character (1);
   declare input_priority (5) fixed (1) static initial (1, 1, 2, 2, 3);
   declare stack_priority (5) fixed (1) static initial (1, 1, 2, 2, 4);
   declare (i, ki, kl) fixed binary;

   put ('Convert a Reverse Polish expression to orthodox.');
   put skip list ('Enclose the expression in apostrophes:');
   get list (RPN);
   put skip list ('The original Reverse Polish expression = ' || RPN);
   out = '';

   allocate list, written;
   list = substr(RPN, length(RPN), 1); written = false;

translation:
   do i = length (RPN)-1 to 1 by -1;
      s = substr(RPN, i, 1);
      if s = ' ' then iterate;
      ki = index('+-*/^', s);
      kl = index('+-*/^', list);
      if ki > 0 then /* we have an operator */
         do;
            if input_priority (ki) < stack_priority (kl) then
               do; /* transfer ')' to list, then operator. */
                  allocate list, written;
                  list = '('; written = false;
                  out = ')' || out;
               end;
            allocate list, written;
            list = s; written = false;
         end;
      else /* It's a variable name */
         do;
            out = s || out;
next_list:
            if allocation(list) > 0 then if written then free written, list;
            if allocation(list) > 0 then if list = '(' then
               do; out = list || out; free written, list; end;
            if allocation (list) = 0 then leave translation;
            if written then go to next_list;
            written = true;
            out = list || out; /* Output an operator. */
         end;
      put skip edit ('INPUT=' || s) (a); call show_stack;
      put edit (' OUTPUT=', out) (col(30), 2 a);
   end;
   put skip list ('ALGEBRAIC EXPRESSION=', out);

end cvt;

```

Outputs:

```txt

The original Reverse Polish expression = 3 4 2 * 1 5 - 2 3 ^ ^ / +
INPUT=/ STACK=/+              OUTPUT=
INPUT=^ STACK=^/+             OUTPUT=
INPUT=^ STACK=^(^/+           OUTPUT=)
INPUT=3 STACK=^(^/+           OUTPUT=^3)
INPUT=2 STACK=^/+             OUTPUT=^(2^3)
INPUT=- STACK=-(^/+           OUTPUT=)^(2^3)
INPUT=5 STACK=-(^/+           OUTPUT=-5)^(2^3)
INPUT=1 STACK=/+              OUTPUT=/(1-5)^(2^3)
INPUT=* STACK=*/+             OUTPUT=/(1-5)^(2^3)
INPUT=2 STACK=*/+             OUTPUT=*2/(1-5)^(2^3)
INPUT=4 STACK=+               OUTPUT=+4*2/(1-5)^(2^3)
ALGEBRAIC EXPRESSION=   3+4*2/(1-5)^(2^3)


The original Reverse Polish expression = 1 2+ 3 4 + ^ 5 6 + ^
INPUT=+ STACK=+(^             OUTPUT=)
INPUT=6 STACK=+(^             OUTPUT=+6)
INPUT=5 STACK=^               OUTPUT=^(5+6)
INPUT=^ STACK=^(^             OUTPUT=)^(5+6)
INPUT=+ STACK=+(^(^           OUTPUT=))^(5+6)
INPUT=4 STACK=+(^(^           OUTPUT=+4))^(5+6)
INPUT=3 STACK=^(^             OUTPUT=^(3+4))^(5+6)
INPUT=+ STACK=+(^(^           OUTPUT=)^(3+4))^(5+6)
INPUT=2 STACK=+(^(^           OUTPUT=+2)^(3+4))^(5+6)
ALGEBRAIC EXPRESSION=   ((1+2)^(3+4))^(5+6)

```

Procedure to display stack:

```txt

show_stack: procedure;
   declare stack character (1) controlled;
   put edit (' STACK=') (a);
   do while (allocation (list) > 0);
      allocate stack; stack = list; free list; put edit (stack) (a);
   end;
   do while (allocation (stack) > 0);
      allocate list; list = stack; free stack;
   end;
end show_stack;

```



## Python


```python


"""
>>> # EXAMPLE USAGE
>>> result = rpn_to_infix('3 4 2 * 1 5 - 2 3 ^ ^ / +', VERBOSE=True)
TOKEN  STACK
3      ['3']
4      ['3', '4']
2      ['3', '4', '2']
*      ['3', Node('2','*','4')]
1      ['3', Node('2','*','4'), '1']
5      ['3', Node('2','*','4'), '1', '5']
-      ['3', Node('2','*','4'), Node('5','-','1')]
2      ['3', Node('2','*','4'), Node('5','-','1'), '2']
3      ['3', Node('2','*','4'), Node('5','-','1'), '2', '3']
^      ['3', Node('2','*','4'), Node('5','-','1'), Node('3','^','2')]
^      ['3', Node('2','*','4'), Node(Node('3','^','2'),'^',Node('5','-','1'))]
/      ['3', Node(Node(Node('3','^','2'),'^',Node('5','-','1')),'/',Node('2','*','4'))]
+      [Node(Node(Node(Node('3','^','2'),'^',Node('5','-','1')),'/',Node('2','*','4')),'+','3')]
"""

prec_dict =  {'^':4, '*':3, '/':3, '+':2, '-':2}
assoc_dict = {'^':1, '*':0, '/':0, '+':0, '-':0}

class Node:
    def __init__(self,x,op,y=None):
        self.precedence = prec_dict[op]
        self.assocright = assoc_dict[op]
        self.op = op
        self.x,self.y = x,y

    def __str__(self):
        """
        Building an infix string that evaluates correctly is easy.
        Building an infix string that looks pretty and evaluates
        correctly requires more effort.
        """
        # easy case, Node is unary
        if self.y == None:
            return '%s(%s)'%(self.op,str(self.x))

        # determine left side string
        str_y = str(self.y)
        if  self.y < self or \
            (self.y == self and self.assocright) or \
            (str_y[0] is '-' and self.assocright):

            str_y = '(%s)'%str_y
        # determine right side string and operator
        str_x = str(self.x)
        str_op = self.op
        if self.op is '+' and not isinstance(self.x, Node) and str_x[0] is '-':
            str_x = str_x[1:]
            str_op = '-'
        elif self.op is '-' and not isinstance(self.x, Node) and str_x[0] is '-':
            str_x = str_x[1:]
            str_op = '+'
        elif self.x < self or \
             (self.x == self and not self.assocright and \
              getattr(self.x, 'op', 1) != getattr(self, 'op', 2)):

            str_x = '(%s)'%str_x
        return ' '.join([str_y, str_op, str_x])

    def __repr__(self):
        """
        >>> repr(Node('3','+','4')) == repr(eval(repr(Node('3','+','4'))))
        True
        """
        return 'Node(%s,%s,%s)'%(repr(self.x), repr(self.op), repr(self.y))

    def __lt__(self, other):
        if isinstance(other, Node):
            return self.precedence < other.precedence
        return self.precedence < prec_dict.get(other,9)

    def __gt__(self, other):
        if isinstance(other, Node):
            return self.precedence > other.precedence
        return self.precedence > prec_dict.get(other,9)

    def __eq__(self, other):
        if isinstance(other, Node):
            return self.precedence == other.precedence
        return self.precedence > prec_dict.get(other,9)



def rpn_to_infix(s, VERBOSE=False):
    """

    converts rpn notation to infix notation for string s

    """
    if VERBOSE : print('TOKEN  STACK')

    stack=[]
    for token in s.replace('^','^').split():
        if token in prec_dict:
            stack.append(Node(stack.pop(),token,stack.pop()))
        else:
            stack.append(token)

        # can't use \t in order to make global docstring pass doctest
        if VERBOSE : print(token+' '*(7-len(token))+repr(stack))

    return str(stack[0])

strTest = "3 4 2 * 1 5 - 2 3 ^ ^ / +"
strResult = rpn_to_infix(strTest, VERBOSE=False)
print ("Input: ",strTest)
print ("Output:",strResult)

print()

strTest = "1 2 + 3 4 + ^ 5 6 + ^"
strResult = rpn_to_infix(strTest, VERBOSE=False)
print ("Input: ",strTest)
print ("Output:",strResult)

```

Output:

```txt

Input:  3 4 2 * 1 5 - 2 3 ^ ^ / +
Output: 3 + 4 * 2 / (1 - 5) ^ 2 ^ 3

Input:  1 2 + 3 4 + ^ 5 6 + ^
Output: ((1 + 2) ^ (3 + 4)) ^ (5 + 6)

```



## Racket

{{trans|AWK}}

```racket

#lang racket
(require racket/dict)

(define (RPN->infix expr)
  (define-values (res _)
    (for/fold ([stack '()] [prec '()]) ([t expr])
      (show t stack prec)
      (cond
        [(dict-has-key? operators t)
         (match-define (list pt at) (dict-ref operators t))
         (match-define (list y x ss ...) stack)
         (match-define (list py px ps ...) prec)
         (define fexpr
           (cond
             [(> pt (max px py)) "(~a) ~a (~a)"]
             [(or (< px pt) (and (= pt px) (eq? at 'r))) "(~a) ~a ~a"]
             [(or (< py pt) (and (= pt py) (eq? at 'l))) "~a ~a (~a)"]
             [else "~a ~a ~a"]))
         (define term (format fexpr x t y))
         (values (cons term ss) (cons pt ps))]
        [else (values (cons t stack) (cons +inf.0 prec))])))
  (car res))

;; the list of operators and their properties
(define operators '((+ 2 l) (- 2 l) (* 3 l) (/ 3 l) (^ 4 r)))

;; printing out the intermediate stages
(define (show t stack prec)
  (printf "~a\t" t)
  (for ([s stack] [p prec])
    (if (eq? +inf.0 p) (printf "[~a] " s) (printf "[~a {~a}] " s p)))
  (newline))

```


Output:

```txt

-> (RPN->infix '(3 4 2 * 1 5 - 2 3 ^ ^ / +))
3
4	[3]
2	[4] [3]
*	[2] [4] [3]
1	[4 * 2 {3}] [3]
5	[1] [4 * 2 {3}] [3]
-	[5] [1] [4 * 2 {3}] [3]
2	[1 - 5 {2}] [4 * 2 {3}] [3]
3	[2] [1 - 5 {2}] [4 * 2 {3}] [3]
^	[3] [2] [1 - 5 {2}] [4 * 2 {3}] [3]
^	[2 ^ 3 {4}] [1 - 5 {2}] [4 * 2 {3}] [3]
/	[(1 - 5) ^ 2 ^ 3 {4}] [4 * 2 {3}] [3]
+	[4 * 2 / (1 - 5) ^ 2 ^ 3 {3}] [3]
"3 + 4 * 2 / (1 - 5) ^ 2 ^ 3"

-> (RPN->infix '(1 2 + 3 4 + ^))
1
2	[1]
+	[2] [1]
3	[1 + 2 {2}]
4	[3] [1 + 2 {2}]
+	[4] [3] [1 + 2 {2}]
^	[3 + 4 {2}] [1 + 2 {2}]
"(1 + 2) ^ (3 + 4)"

-> (RPN->infix '(moon stars mud + * fire soup * ^))
moon
stars	[moon]
mud	[stars] [moon]
+	[mud] [stars] [moon]
*	[stars + mud {2}] [moon]
fire	[moon * (stars + mud) {3}]
soup	[fire] [moon * (stars + mud) {3}]
*	[soup] [fire] [moon * (stars + mud) {3}]
^	[fire * soup {3}] [moon * (stars + mud) {3}]
"(moon * (stars + mud)) ^ (fire * soup)"

```



## REXX

{{trans|AWK}}
{{trans|Tcl}}
A Yen symbol   '''¥'''   was used instead of a   '''9'''   to make it apparenet that it's just a placeholder.

The same reasoning was used for the ''operator associations''   (the left   ◄   and right   ►   arrow symbols).

```rexx
/*REXX program converts Reverse Polish Notation (RPN) ──► infix notation*/
showAction = 1                         /*  0  if no showActions wanted. */
         # = 0                         /*initialize stack pointer to 0. */
        oS = '+ - / * ^'               /*operator symbols.              */
        oP = '2 2 3 3 4'               /*operator priorities.           */
        oA = '◄ ◄ ◄ ◄ ►'               /*operator associations.         */
say  "infix: "   toInfix( "3 4 2 * 1 5 - 2 3 ^ ^ / +" )
say  "infix: "   toInfix( "1 2 + 3 4 + ^ 5 6 + ^" )     /* [↓]  Deutsch.*/
say  "infix: "   toInfix( "Mond Sterne Schlamm + * Feur Suppe * ^" )
exit                                   /*stick a fork in it, we're done.*/
/*──────────────────────────────────────────────────────────────────────*/
pop:    pop=#;    #=#-1;                 return @.pop
push:   #=#+1;    @.#=arg(1);            return
/*──────────────────────────────────────────────────────────────────────*/
stack2str: $=;    do j=1  for #;         _ = @.j;      y=left(_,1)
                  if pos(' ', _)==0  then _ = '{'strip(substr(_, 2))"}"
                                     else _ =          substr(_, 2)
                  $=$  '{'strip(y _)"}"
                  end   /*j*/
return space($)
/*──────────────────────────────────────────────────────────────────────*/
toInfix: parse arg rpn;   say copies('─',80-1);    say 'RPN: '  space(rpn)

  do N=1  for words(RPN); ?=word(RPN,N) /*process each of the RPN tokens.*/
  if pos(?,oS)==0  then call push '¥' ? /*when in doubt, add a Yen to it.*/
                   else do;   g=pop();    gp=left(g, 1);    g=substr(g, 2)
                              h=pop();    hp=left(h, 1);    h=substr(h, 2)
                        tp=substr(oP,pos(?, oS),  1)
                        ta=substr(oA,pos(?, oS),  1)
                        if hp<tp  |  (hp==tp & ta=='►')  then h="("h")"
                        if gp<tp  |  (gp==tp & ta=='◄')  then g="("g")"
                        call  push   tp  ||  h  ?  g
                        end
   if showAction  then say   right(?,25)    "──►"    stack2str()
   end   /*N*/

return space(substr(pop(), 2))
```

'''output''' when using the default input:

[Output is very similar to AWK's output.]
<pre style="height:55ex">
───────────────────────────────────────────────────────────────────────────────
RPN:  3 4 2 * 1 5 - 2 3 ^ ^ / +
                        3 ──► {¥ 3}
                        4 ──► {¥ 3} {¥ 4}
                        2 ──► {¥ 3} {¥ 4} {¥ 2}
                        * ──► {¥ 3} {3 4 * 2}
                        1 ──► {¥ 3} {3 4 * 2} {¥ 1}
                        5 ──► {¥ 3} {3 4 * 2} {¥ 1} {¥ 5}
                        - ──► {¥ 3} {3 4 * 2} {2 1 - 5}
                        2 ──► {¥ 3} {3 4 * 2} {2 1 - 5} {¥ 2}
                        3 ──► {¥ 3} {3 4 * 2} {2 1 - 5} {¥ 2} {¥ 3}
                        ^ ──► {¥ 3} {3 4 * 2} {2 1 - 5} {4 2 ^ 3}
                        ^ ──► {¥ 3} {3 4 * 2} {4 ( 1 - 5) ^ 2 ^ 3}
                        / ──► {¥ 3} {3 4 * 2 / ( 1 - 5) ^ 2 ^ 3}
                        + ──► {2 3 + 4 * 2 / ( 1 - 5) ^ 2 ^ 3}
infix:  3 + 4 * 2 / ( 1 - 5) ^ 2 ^ 3
───────────────────────────────────────────────────────────────────────────────
RPN:  1 2 + 3 4 + ^ 5 6 + ^
                        1 ──► {¥ 1}
                        2 ──► {¥ 1} {¥ 2}
                        + ──► {2 1 + 2}
                        3 ──► {2 1 + 2} {¥ 3}
                        4 ──► {2 1 + 2} {¥ 3} {¥ 4}
                        + ──► {2 1 + 2} {2 3 + 4}
                        ^ ──► {4 ( 1 + 2) ^ ( 3 + 4)}
                        5 ──► {4 ( 1 + 2) ^ ( 3 + 4)} {¥ 5}
                        6 ──► {4 ( 1 + 2) ^ ( 3 + 4)} {¥ 5} {¥ 6}
                        + ──► {4 ( 1 + 2) ^ ( 3 + 4)} {2 5 + 6}
                        ^ ──► {4 (( 1 + 2) ^ ( 3 + 4)) ^ ( 5 + 6)}
infix:  (( 1 + 2) ^ ( 3 + 4)) ^ ( 5 + 6)
───────────────────────────────────────────────────────────────────────────────
RPN:  Mond Sterne Schlamm + * Feur Suppe * ^
                     Mond ──► {¥ Mond}
                   Sterne ──► {¥ Mond} {¥ Sterne}
                  Schlamm ──► {¥ Mond} {¥ Sterne} {¥ Schlamm}
                        + ──► {¥ Mond} {2 Sterne + Schlamm}
                        * ──► {3 Mond * ( Sterne + Schlamm)}
                     Feur ──► {3 Mond * ( Sterne + Schlamm)} {¥ Feur}
                    Suppe ──► {3 Mond * ( Sterne + Schlamm)} {¥ Feur} {¥ Suppe}
                        * ──► {3 Mond * ( Sterne + Schlamm)} {3 Feur * Suppe}
                        ^ ──► {4 ( Mond * ( Sterne + Schlamm)) ^ ( Feur * Suppe)
}
infix:  ( Mond * ( Sterne + Schlamm)) ^ ( Feur * Suppe)

```



## Ruby

{{incorrect|Ruby|Fails with right-associativity of exponentiation. <code>RPNExpression.new("5 6 ^ 7 ^").to_infix</code> wrongly returns "5 ^ 6 ^ 7". Correct answer is "(5 ^ 6) ^ 7".}}
See [[Parsing/RPN/Ruby]]


```ruby
rpn = RPNExpression.new("3 4 2 * 1 5 - 2 3 ^ ^ / +")
infix = rpn.to_infix
ruby = rpn.to_ruby
```

outputs

```txt
for RPN expression: 3 4 2 * 1 5 - 2 3 ^ ^ / +
Term	Action	Stack
3	PUSH	[node[3]]
4	PUSH	[node[3], node[4]]
2	PUSH	[node[3], node[4], node[2]]
*	MUL	[node[3], node[*]<left=node[4], right=node[2]>]
1	PUSH	[node[3], node[*]<left=node[4], right=node[2]>, node[1]]
5	PUSH	[node[3], node[*]<left=node[4], right=node[2]>, node[1], node[5]]
-	SUB	[node[3], node[*]<left=node[4], right=node[2]>, node[-]<left=node[1], right=node[5]>]
2	PUSH	[node[3], node[*]<left=node[4], right=node[2]>, node[-]<left=node[1], right=node[5]>, node[2]]
3	PUSH	[node[3], node[*]<left=node[4], right=node[2]>, node[-]<left=node[1], right=node[5]>, node[2], node[3]]
^	EXP	[node[3], node[*]<left=node[4], right=node[2]>, node[-]<left=node[1], right=node[5]>, node[^]<left=node[2], right=node[3]>]
^	EXP	[node[3], node[*]<left=node[4], right=node[2]>, node[^]<left=node[-]<left=node[1], right=node[5]>, right=node[^]<left=node[2], right=node[3]>>]
/	DIV	[node[3], node[/]<left=node[*]<left=node[4], right=node[2]>, right=node[^]<left=node[-]<left=node[1], right=node[5]>, right=node[^]<left=node[2], right=node[3]>>>]
+	ADD	[node[+]<left=node[3], right=node[/]<left=node[*]<left=node[4], right=node[2]>, right=node[^]<left=node[-]<left=node[1], right=node[5]>, right=node[^]<left=node[2], right=node[3]>>>>]
Infix = 3 + 4 * 2 / ( 1 - 5 ) ^ 2 ^ 3
Ruby = 3 + 4 * 2.to_f / ( 1 - 5 ) ** 2 ** 3
```



## Sidef

{{trans|Perl 6}}

```ruby
func p(pair, prec) {
    pair[0] < prec ? "( #{pair[1]} )" : pair[1]
}
 
func rpm_to_infix(string) {
    say "#{'='*17}\n#{string}"
    var stack = []
    string.each_word { |w|
        if (w ~~ /\d/) {
            stack << [9, Num(w)]
        }
        else {
            var y = stack.pop
            var x = stack.pop
            given(w) {
              when ('^')   { stack << [4, [p(x,5), w, p(y,4)].join(' ')] }
              when (<* />) { stack << [3, [p(x,3), w, p(y,3)].join(' ')] }
              when (<+ ->) { stack << [2, [p(x,2), w, p(y,2)].join(' ')] }
            }
            say stack
        }
    }
    say '-'*17
    stack.map{_[1]}
}
 
var tests = [
    '3 4 2 * 1 5 - 2 3 ^ ^ / +',
    '1 2 + 3 4 + ^ 5 6 + ^',
]
 
tests.each { say rpm_to_infix(_).join(' ') }
```

{{out}}

```txt


### ===========

3 4 2 * 1 5 - 2 3 ^ ^ / +
[[9, 3], [3, "4 * 2"]]
[[9, 3], [3, "4 * 2"], [2, "1 - 5"]]
[[9, 3], [3, "4 * 2"], [2, "1 - 5"], [4, "2 ^ 3"]]
[[9, 3], [3, "4 * 2"], [4, "( 1 - 5 ) ^ 2 ^ 3"]]
[[9, 3], [3, "4 * 2 / ( 1 - 5 ) ^ 2 ^ 3"]]
[[2, "3 + 4 * 2 / ( 1 - 5 ) ^ 2 ^ 3"]]
-----------------
3 + 4 * 2 / ( 1 - 5 ) ^ 2 ^ 3

### ===========

1 2 + 3 4 + ^ 5 6 + ^
[[2, "1 + 2"]]
[[2, "1 + 2"], [2, "3 + 4"]]
[[4, "( 1 + 2 ) ^ ( 3 + 4 )"]]
[[4, "( 1 + 2 ) ^ ( 3 + 4 )"], [2, "5 + 6"]]
[[4, "( ( 1 + 2 ) ^ ( 3 + 4 ) ) ^ ( 5 + 6 )"]]
-----------------
( ( 1 + 2 ) ^ ( 3 + 4 ) ) ^ ( 5 + 6 )

```



## Tcl


```tcl
package require Tcl 8.5

# Helpers
proc precassoc op {
    dict get {^ {4 right} * {3 left} / {3 left} + {2 left} - {2 left}} $op
}
proc pop stk {
    upvar 1 $stk s
    set val [lindex $s end]
    set s [lreplace $s end end]
    return $val
}

proc rpn2infix rpn {
    foreach token $rpn {
	switch $token {
	    "^" - "/" - "*" - "+" - "-" {
		lassign [pop stack] bprec b
		lassign [pop stack] aprec a
		lassign [precassoc $token] p assoc
		if {$aprec < $p || ($aprec == $p && $assoc eq "right")} {
		    set a "($a)"
		}
		if {$bprec < $p || ($bprec == $p && $assoc eq "left")} {
		    set b "($b)"
		}
		lappend stack [list $p "$a $token $b"]
	    }
	    default {
		lappend stack [list 9 $token]
	    }
	}
	puts "$token -> $stack"
    }
    return [lindex $stack end 1]
}

puts [rpn2infix {3 4 2 * 1 5 - 2 3 ^ ^ / +}]
puts [rpn2infix {1 2 + 3 4 + ^ 5 6 + ^}]
```

Output:

```txt

3 -> {9 3}
4 -> {9 3} {9 4}
2 -> {9 3} {9 4} {9 2}
* -> {9 3} {3 {4 * 2}}
1 -> {9 3} {3 {4 * 2}} {9 1}
5 -> {9 3} {3 {4 * 2}} {9 1} {9 5}
- -> {9 3} {3 {4 * 2}} {2 {1 - 5}}
2 -> {9 3} {3 {4 * 2}} {2 {1 - 5}} {9 2}
3 -> {9 3} {3 {4 * 2}} {2 {1 - 5}} {9 2} {9 3}
^ -> {9 3} {3 {4 * 2}} {2 {1 - 5}} {4 {2 ^ 3}}
^ -> {9 3} {3 {4 * 2}} {4 {(1 - 5) ^ 2 ^ 3}}
/ -> {9 3} {3 {4 * 2 / (1 - 5) ^ 2 ^ 3}}
+ -> {2 {3 + 4 * 2 / (1 - 5) ^ 2 ^ 3}}
3 + 4 * 2 / (1 - 5) ^ 2 ^ 3
1 -> {9 1}
2 -> {9 1} {9 2}
+ -> {2 {1 + 2}}
3 -> {2 {1 + 2}} {9 3}
4 -> {2 {1 + 2}} {9 3} {9 4}
+ -> {2 {1 + 2}} {2 {3 + 4}}
^ -> {4 {(1 + 2) ^ (3 + 4)}}
5 -> {4 {(1 + 2) ^ (3 + 4)}} {9 5}
6 -> {4 {(1 + 2) ^ (3 + 4)}} {9 5} {9 6}
+ -> {4 {(1 + 2) ^ (3 + 4)}} {2 {5 + 6}}
^ -> {4 {((1 + 2) ^ (3 + 4)) ^ (5 + 6)}}
((1 + 2) ^ (3 + 4)) ^ (5 + 6)

```



## TXR


This solution is a little long because it works by translating RPN to fully parenthesized prefix (Lisp notation).

Also, it improves upon the problem slightly. Note that for the operators <code>*</code> and <code>+</code>, the associativity is configured as<code>nil</code> ("no associativity") rather than left-to-right. This is because these operators obey the associative property: <code>(a + b) + c</code> is <code>a + (b + c)</code>, and so we usually write <code>a + b + c or a * b * c</code> without any parentheses, leaving it ambiguous which addition is done first. Associativity is not important for these operators.

The <code>lisp-to-infix</code> filter then takes advantage of this non-associativity in minimizing the parentheses.


```txrlisp
;; alias for circumflex, which is reserved syntax
(defvar exp (intern "^"))

(defvar *prec* ^((,exp . 4) (* . 3) (/ . 3) (+ . 2) (- . 2)))

(defvar *asso* ^((,exp . :right) (* . nil)
                 (/ . :left) (+ . nil) (- . :left)))

(defun debug-print (label val)
  (format t "~a: ~a\n" label val)
  val)

(defun rpn-to-lisp (rpn)
  (let (stack)
    (each ((term rpn))
      (if (symbolp (debug-print "rpn term" term))
        (let ((right (pop stack))
              (left (pop stack)))
          (push ^(,term ,left ,right) stack))
        (push term stack))
      (debug-print "stack" stack))
    (if (rest stack)
      (return-from error "*excess stack elements*"))
      (debug-print "lisp" (pop stack))))

(defun prec (term)
  (or (cdr (assoc term *prec*)) 99))

(defun asso (term dfl)
  (or (cdr (assoc term *asso*)) dfl))

(defun inf-term (op term left-or-right)
  (if (atom term)
    `@term`
    (let ((pt (prec (car term)))
          (po (prec op))
          (at (asso (car term) left-or-right))
          (ao (asso op left-or-right)))
      (cond
        ((< pt po) `(@(lisp-to-infix term))`)
        ((> pt po) `@(lisp-to-infix term)`)
        ((and (eq at ao) (eq left-or-right ao)) `@(lisp-to-infix term)`)
        (t `(@(lisp-to-infix term))`)))))

(defun lisp-to-infix (lisp)
  (tree-case lisp
    ((op left right) (let ((left-inf (inf-term op left :left))
                           (right-inf (inf-term op right :right)))
                       `@{left-inf} @op @{right-inf}`))
    (()              (return-from error "*stack underflow*"))
    (else            `@lisp`)))

(defun string-to-rpn (str)
  (debug-print "rpn"
    (mapcar (do if (int-str @1) (int-str @1) (intern @1))
            (tok-str str #/[^ \t]+/))))

(debug-print "infix"
  (block error
     (tree-case *args*
       ((a b . c) "*excess args*")
       ((a) (lisp-to-infix (rpn-to-lisp (string-to-rpn a))))
       (else "*arg needed*"))))
```


{{out}}


```txt
$ txr rpn.tl '3 4 2 * 1 5 - 2 3 ^ ^ / +'
rpn: (3 4 2 * 1 5 - 2 3 ^ ^ / +)
rpn term: 3
stack: (3)
rpn term: 4
stack: (4 3)
rpn term: 2
stack: (2 4 3)
rpn term: *
stack: ((* 4 2) 3)
rpn term: 1
stack: (1 (* 4 2) 3)
rpn term: 5
stack: (5 1 (* 4 2) 3)
rpn term: -
stack: ((- 1 5) (* 4 2) 3)
rpn term: 2
stack: (2 (- 1 5) (* 4 2) 3)
rpn term: 3
stack: (3 2 (- 1 5) (* 4 2) 3)
rpn term: ^
stack: ((^ 2 3) (- 1 5) (* 4 2) 3)
rpn term: ^
stack: ((^ (- 1 5) (^ 2 3)) (* 4 2) 3)
rpn term: /
stack: ((/ (* 4 2) (^ (- 1 5) (^ 2 3))) 3)
rpn term: +
stack: ((+ 3 (/ (* 4 2) (^ (- 1 5) (^ 2 3)))))
lisp: (+ 3 (/ (* 4 2) (^ (- 1 5) (^ 2 3))))
infix: 3 + 4 * 2 / (1 - 5) ^ 2 ^ 3

$ txr rpn.tl '1 2 + 3 4 + ^ 5 6 + ^'
rpn: (1 2 + 3 4 + ^ 5 6 + ^)
rpn term: 1
stack: (1)
rpn term: 2
stack: (2 1)
rpn term: +
stack: ((+ 1 2))
rpn term: 3
stack: (3 (+ 1 2))
rpn term: 4
stack: (4 3 (+ 1 2))
rpn term: +
stack: ((+ 3 4) (+ 1 2))
rpn term: ^
stack: ((^ (+ 1 2) (+ 3 4)))
rpn term: 5
stack: (5 (^ (+ 1 2) (+ 3 4)))
rpn term: 6
stack: (6 5 (^ (+ 1 2) (+ 3 4)))
rpn term: +
stack: ((+ 5 6) (^ (+ 1 2) (+ 3 4)))
rpn term: ^
stack: ((^ (^ (+ 1 2) (+ 3 4)) (+ 5 6)))
lisp: (^ (^ (+ 1 2) (+ 3 4)) (+ 5 6))
infix: ((1 + 2) ^ (3 + 4)) ^ (5 + 6)

```


Associativity tests (abbreviated output):


```txt
$ txr rpn.tl '1 2 3 + +'
[ ... ]
infix: 1 + 2 + 3

$ txr rpn.tl '1 2 + 3 +'
[ ... ]
infix: 1 + 2 + 3

$ txr rpn.tl '1 2 3 ^ ^'
rpn tokens: [1 2 3 ^ ^]
[ ... ]
infix: 1 ^ 2 ^ 3

$ txr rpn.tl '1 2 ^ 3 ^'
[ ... ]
infix: (1 ^ 2) ^ 3

$ txr rpn.tl '1 1 - 3 +'
[ .. ]
infix: 1 - 1 + 3

$ txr rpn.tl '3 1 1 - +'
[ .. ]
infix: 3 + (1 - 1)
```



## Visual Basic .NET

{{trans|C#}}

```vbnet
Option Strict On

Imports System.Text.RegularExpressions

Module Module1

    Class Operator_
        Sub New(t As Char, p As Integer, Optional i As Boolean = False)
            Token = t
            Precedence = p
            IsRightAssociative = i
        End Sub

        Property Token As Char
            Get
                Return m_token
            End Get
            Private Set(value As Char)
                m_token = value
            End Set
        End Property

        Property Precedence As Integer
            Get
                Return m_precedence
            End Get
            Private Set(value As Integer)
                m_precedence = value
            End Set
        End Property

        Property IsRightAssociative As Boolean
            Get
                Return m_right
            End Get
            Private Set(value As Boolean)
                m_right = value
            End Set
        End Property

        Private m_token As Char
        Private m_precedence As Integer
        Private m_right As Boolean
    End Class

    ReadOnly operators As New Dictionary(Of Char, Operator_) From {
        {"+"c, New Operator_("+"c, 2)},
        {"-"c, New Operator_("-"c, 2)},
        {"/"c, New Operator_("/"c, 3)},
        {"*"c, New Operator_("*"c, 3)},
        {"^"c, New Operator_("^"c, 4, True)}
    }

    Class Expression
        Public Sub New(e As String)
            Ex = e
        End Sub

        Sub New(e1 As String, e2 As String, o As Operator_)
            Ex = String.Format("{0} {1} {2}", e1, o.Token, e2)
            Op = o
        End Sub

        ReadOnly Property Ex As String
        ReadOnly Property Op As Operator_
    End Class

    Function PostfixToInfix(postfix As String) As String
        Dim stack As New Stack(Of Expression)

        For Each token As String In Regex.Split(postfix, "\s+")
            Dim c = token(0)
            Dim op = operators.FirstOrDefault(Function(kv) kv.Key = c).Value
            If Not IsNothing(op) AndAlso token.Length = 1 Then
                Dim rhs = stack.Pop()
                Dim lhs = stack.Pop()

                Dim opPrec = op.Precedence

                Dim lhsPrec = If(IsNothing(lhs.Op), Integer.MaxValue, lhs.Op.Precedence)
                Dim rhsPrec = If(IsNothing(rhs.Op), Integer.MaxValue, rhs.Op.Precedence)

                Dim newLhs As String
                If lhsPrec < opPrec OrElse (lhsPrec = opPrec AndAlso c = "^") Then
                    'lhs.Ex = "(" + lhs.Ex + ")"
                    newLhs = "(" + lhs.Ex + ")"
                Else
                    newLhs = lhs.Ex
                End If

                Dim newRhs As String
                If rhsPrec < opPrec OrElse (rhsPrec = opPrec AndAlso c <> "^") Then
                    'rhs.Ex = "(" + rhs.Ex + ")"
                    newRhs = "(" + rhs.Ex + ")"
                Else
                    newRhs = rhs.Ex
                End If

                stack.Push(New Expression(newLhs, newRhs, op))
            Else
                stack.Push(New Expression(token))
            End If

            'Print intermediate result
            Console.WriteLine("{0} -> [{1}]", token, String.Join(", ", stack.Reverse().Select(Function(e) e.Ex)))
        Next

        Return stack.Peek().Ex
    End Function

    Sub Main()
        Dim inputs = {"3 4 2 * 1 5 - 2 3 ^ ^ / +", "1 2 + 3 4 + ^ 5 6 + ^"}
        For Each e In inputs
            Console.WriteLine("Postfix : {0}", e)
            Console.WriteLine("Infix : {0}", PostfixToInfix(e))
            Console.WriteLine()
        Next
        Console.ReadLine() 'remove before submit
    End Sub

End Module
```

{{out}}

```txt
Postfix : 3 4 2 * 1 5 - 2 3 ^ ^ / +
3 -> [3]
4 -> [3, 4]
2 -> [3, 4, 2]
* -> [3, 4 * 2]
1 -> [3, 4 * 2, 1]
5 -> [3, 4 * 2, 1, 5]
- -> [3, 4 * 2, 1 - 5]
2 -> [3, 4 * 2, 1 - 5, 2]
3 -> [3, 4 * 2, 1 - 5, 2, 3]
^ -> [3, 4 * 2, 1 - 5, 2 ^ 3]
^ -> [3, 4 * 2, (1 - 5) ^ 2 ^ 3]
/ -> [3, 4 * 2 / (1 - 5) ^ 2 ^ 3]
+ -> [3 + 4 * 2 / (1 - 5) ^ 2 ^ 3]
Infix : 3 + 4 * 2 / (1 - 5) ^ 2 ^ 3

Postfix : 1 2 + 3 4 + ^ 5 6 + ^
1 -> [1]
2 -> [1, 2]
+ -> [1 + 2]
3 -> [1 + 2, 3]
4 -> [1 + 2, 3, 4]
+ -> [1 + 2, 3 + 4]
^ -> [(1 + 2) ^ (3 + 4)]
5 -> [(1 + 2) ^ (3 + 4), 5]
6 -> [(1 + 2) ^ (3 + 4), 5, 6]
+ -> [(1 + 2) ^ (3 + 4), 5 + 6]
^ -> [((1 + 2) ^ (3 + 4)) ^ (5 + 6)]
Infix : ((1 + 2) ^ (3 + 4)) ^ (5 + 6)
```



## zkl

{{trans|Go}}

```zkl
tests:=T("3 4 2 * 1 5 - 2 3 ^ ^ / +","1 2 + 3 4 + ^ 5 6 + ^");

var opa=D(
   "^",T(4, True),
   "*",T(3, False), "/",T(3, False),
   "+",T(2, False), "-",T(2, False) );

const nPrec = 9;

foreach t in (tests) { parseRPN(t) }

fcn parseRPN(e){
   println("\npostfix:", e);
   stack:=L();
   foreach tok in (e.split()){
      println("token: ", tok);
      opPrec,rAssoc:=opa.find(tok,T(Void,Void));
      if(opPrec){
	 rhsPrec,rhsExpr := stack.pop();
	 lhsPrec,lhsExpr := stack.pop();
	 if(lhsPrec < opPrec or (lhsPrec == opPrec and rAssoc))
	    lhsExpr = "(" + lhsExpr + ")";
	 lhsExpr += " " + tok + " ";
	 if(rhsPrec < opPrec or (rhsPrec == opPrec and not rAssoc)){
	    lhsExpr += "(" + rhsExpr + ")"
	 } else
	    lhsExpr += rhsExpr;
	 lhsPrec = opPrec;
	 stack.append(T(lhsPrec,lhsExpr));
      } else
	 stack.append(T(nPrec, tok));
      foreach f in (stack){
         println(0'|    %d "%s"|.fmt(f.xplode()))
      }
   }
   println("infix:", stack[0][1])
}
```

{{out}}

```txt

postfix: 3 4 2 * 1 5 - 2 3 ^ ^ / +
token: 3
    9 "3"
token: 4
    9 "3"
    9 "4"
...<see Go output>
token: ^
    9 "3"
    3 "4 * 2"
    4 "(1 - 5) ^ 2 ^ 3"
token: /
    9 "3"
    3 "4 * 2 / (1 - 5) ^ 2 ^ 3"
token: +
    2 "3 + 4 * 2 / (1 - 5) ^ 2 ^ 3"
infix: 3 + 4 * 2 / (1 - 5) ^ 2 ^ 3

postfix: 1 2 + 3 4 + ^ 5 6 + ^
...<see Go output>
infix: ((1 + 2) ^ (3 + 4)) ^ (5 + 6)

```

