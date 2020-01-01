+++
title = "S-Expressions"
description = ""
date = 2019-07-27T18:19:29Z
aliases = []
[extra]
id = 10679
[taxonomies]
categories = []
tags = []
+++

{{task|Data Structures}}
[[wp:S-Expression|S-Expressions]]   are one convenient way to parse and store data.


;Task:
Write a simple reader and writer for S-Expressions that handles quoted and unquoted strings, integers and floats.

The reader should read a single but nested S-Expression from a string and store it in a suitable datastructure (list, array, etc).

Newlines and other whitespace may be ignored unless contained within a quoted string.

“<tt>()</tt>”   inside quoted strings are not interpreted, but treated as part of the string.

Handling escaped quotes inside a string is optional;   thus “<tt>(foo"bar)</tt>” maybe treated as a string “<tt>foo"bar</tt>”, or as an error.

For this, the reader need not recognize “<tt>\</tt>” for escaping, but should, in addition, recognize numbers if the language has appropriate datatypes.

Languages that support it may treat unquoted strings as symbols.

Note that with the exception of “<tt>()"</tt>” (“<tt>\</tt>” if escaping is supported) and whitespace there are no special characters. Anything else is allowed without quotes.

The reader should be able to read the following input

```lisp
((data "quoted data" 123 4.5)
 (data (!@# (4.5) "(more" "data)")))
```

and turn it into a native datastructure. (see the [[#Pike|Pike]], [[#Python|Python]] and [[#Ruby|Ruby]] implementations for examples of native data structures.)

The writer should be able to take the produced list and turn it into a new S-Expression.
Strings that don't contain whitespace or parentheses () don't need to be quoted in the resulting S-Expression, but as a simplification, any string may be quoted.


;Extra Credit:
Let the writer produce pretty printed output with indenting and line-breaks.





## Ada


Uses Ada 2005 (Ada.Containers).

Specification of package S_Expr:


```Ada
with Ada.Strings.Unbounded;
private with Ada.Containers.Indefinite_Vectors;

generic
   with procedure Print_Line(Indention: Natural; Line: String);
package S_Expr is

   function "-"(S: String) return Ada.Strings.Unbounded.Unbounded_String
     renames Ada.Strings.Unbounded.To_Unbounded_String;

   function "+"(U: Ada.Strings.Unbounded.Unbounded_String) return String
     renames Ada.Strings.Unbounded.To_String;

   type Empty_Data is tagged null record;
   subtype Data is Empty_Data'Class;
   procedure Print(This: Empty_Data; Indention: Natural);
   -- any object form class Data knows how to print itself
   -- objects of class data are either List of Data or Atomic
   -- atomic objects hold either an integer or a float or a string

   type List_Of_Data is new Empty_Data with private;
   overriding procedure Print(This: List_Of_Data; Indention: Natural);
   function First(This: List_Of_Data) return Data;
   function Rest(This: List_Of_Data) return List_Of_Data;
   function Empty(This: List_Of_Data) return Boolean;

   type Atomic is new Empty_Data with null record;

   type Str_Data is new Atomic with record
      Value: Ada.Strings.Unbounded.Unbounded_String;
      Quoted: Boolean := False;
   end record;
   overriding procedure Print(This: Str_Data; Indention: Natural);

   type Int_Data is new Atomic with record
      Value: Integer;
   end record;
   overriding procedure Print(This: Int_Data; Indention: Natural);

   type Flt_Data is new Atomic with record
      Value: Float;
   end record;
   overriding procedure Print(This: Flt_Data; Indention: Natural);

private

   package Vectors is new Ada.Containers.Indefinite_Vectors
     (Index_Type   => Positive,
      Element_Type => Data);

   type List_Of_Data is new Empty_Data with record
      Values: Vectors.Vector;
   end record;

end S_Expr;
```


The implementation of S_Expr:


```Ada
with Ada.Integer_Text_IO, Ada.Float_Text_IO;

package body S_Expr is

   function First(This: List_Of_Data) return Data is
   begin
      return This.Values.First_Element;
   end First;

   function Rest(This: List_Of_Data) return List_Of_Data is
      List: List_Of_Data := This;
   begin
      List.Values.Delete_First;
      return List;
   end Rest;

   function Empty(This: List_Of_Data) return Boolean is
   begin
      return This.Values.Is_Empty;
   end Empty;

   procedure Print(This: Empty_Data; Indention: Natural) is
   begin
      Print_Line(Indention, "");
   end Print;

   procedure Print(This: Int_Data; Indention: Natural) is
   begin
      Print_Line(Indention, Integer'Image(This.Value));
   end Print;

   procedure Print(This: Flt_Data; Indention: Natural) is
   begin
      Print_Line(Indention, Float'Image(This.Value));
   end Print;

   procedure Print(This: Str_Data; Indention: Natural) is
   begin
      if This.Quoted then
         Print_Line(Indention, """" & (+This.Value) & """");
      else
         Print_Line(Indention, +This.Value);
      end if;
   end Print;

   procedure Print(This: List_Of_Data; Indention: Natural) is
   begin
      Print_Line(Indention, " ( ");
      for I in This.Values.First_Index .. This.Values.Last_Index loop
         This.Values.Element(I).Print(Indention + 1);
      end loop;
      Print_Line(Indention, " ) ");
   end Print;

end S_Expr;
```


Specification and Implementation of S_Expr.Parser (a child package of S_Expr):


```Ada
generic -- child of a generic package must be a generic unit
package S_Expr.Parser is

   function Parse(Input: String) return List_Of_Data;
   -- the result of a parse process is always a list of expressions

end S_Expr.Parser;
```



```Ada
with Ada.Integer_Text_IO, Ada.Float_Text_IO;

package body S_Expr.Parser is

   function Parse(Input: String) return List_Of_Data is

      procedure First_Token(S: String;
                            Start_Of_Token, End_Of_Token: out Positive) is
      begin
         Start_Of_Token := S'First;
         while Start_Of_Token <= S'Last and then S(Start_Of_Token) = ' ' loop
            Start_Of_Token := Start_Of_Token + 1; -- skip spaces
         end loop;
         if Start_Of_Token > S'Last then
            End_Of_Token := Start_Of_Token - 1;
            -- S(Start_Of_Token .. End_Of_Token) is the empty string
         elsif (S(Start_Of_Token) = '(') or (S(Start_Of_Token) = ')') then
            End_OF_Token := Start_Of_Token; -- the bracket is the token
         elsif S(Start_Of_Token) = '"' then -- " -- begin quoted string
            End_Of_Token := Start_Of_Token + 1;
            while S(End_Of_Token) /= '"' loop -- " -- search for closing bracket
               End_Of_Token := End_Of_Token + 1;
            end loop; -- raises Constraint_Error if closing bracket not found
         else -- Token is some kind of string
            End_Of_Token := Start_Of_Token;
            while End_Of_Token < S'Last and then
           ((S(End_Of_Token+1) /= ' ') and (S(End_Of_Token+1) /= '(') and
              (S(End_Of_Token+1) /= ')') and (S(End_Of_Token+1) /= '"')) loop  -- "
               End_Of_Token := End_Of_Token + 1;
            end loop;
         end if;
      end First_Token;

      procedure To_Int(Token: String; I: out Integer; Found: out Boolean) is
         Last: Positive;
      begin
         Ada.Integer_Text_IO.Get(Token, I, Last);
         Found := Last = Token'Last;
      exception
         when others => Found := False;
      end To_Int;

      procedure To_Flt(Token: String; F: out Float; Found: out Boolean) is
         Last: Positive;
      begin
         Ada.Float_Text_IO.Get(Token, F, Last);
         Found := Last = Token'Last;
      exception
         when others => Found := False;
      end To_Flt;

      function Quoted_String(Token: String) return Boolean is
      begin
         return
           Token'Length >= 2 and then Token(Token'First)='"' -- "
                             and then Token(Token'Last) ='"'; -- "
      end Quoted_String;

      Start, Stop: Positive;

      procedure Recursive_Parse(This: in out List_Of_Data) is

      Found: Boolean;

      Flt: Flt_Data;
      Int: Int_Data;
      Str: Str_Data;
      Lst: List_Of_Data;

      begin
         while Input(Start .. Stop) /= "" loop
            if Input(Start .. Stop) = ")" then
               return;
            elsif Input(Start .. Stop) = "(" then
               First_Token(Input(Stop+1 .. Input'Last), Start, Stop);
               Recursive_Parse(Lst);
               This.Values.Append(Lst);
            else
               To_Int(Input(Start .. Stop), Int.Value, Found);
               if Found then
                  This.Values.Append(Int);
               else
                  To_Flt(Input(Start .. Stop), Flt.Value, Found);
                  if Found then
                     This.Values.Append(Flt);
                  else
                     if Quoted_String(Input(Start .. Stop)) then
                        Str.Value  := -Input(Start+1 .. Stop-1);
                        Str.Quoted := True;
                     else
                        Str.Value  := -Input(Start .. Stop);
                        Str.Quoted := False;
                     end if;
                     This.Values.Append(Str);
                  end if;
               end if;
            end if;
            First_Token(Input(Stop+1 .. Input'Last), Start, Stop);
         end loop;
      end Recursive_Parse;

      L: List_Of_Data;

   begin
      First_Token(Input, Start, Stop);
      Recursive_Parse(L);
      return L;
   end Parse;

end S_Expr.Parser;
```


The main program Test_S_Expr:


```Ada
with S_Expr.Parser, Ada.Text_IO;

procedure Test_S_Expr is

   procedure Put_Line(Indention: Natural; Line: String) is
   begin
      for I in 1 .. 3*Indention loop
         Ada.Text_IO.Put(" ");
      end loop;
      Ada.Text_IO.Put_Line(Line);
   end Put_Line;

   package S_Exp is new S_Expr(Put_Line);
   package S_Par is new S_Exp.Parser;

   Input: String := "((data ""quoted data"" 123 4.5)" &
                    "(data (!@# (4.5) ""(more"" ""data)"")))";
   Expression_List: S_Exp.List_Of_Data := S_Par.Parse(Input);

begin
   Expression_List.First.Print(Indention => 0);
   -- Parse will output a list of S-Expressions. We need the first Expression.
end Test_S_Expr;
```


{{out}}

```txt
 (
    (
      data
      "quoted data"
       123
       4.50000E+00
    )
    (
      data
      "quoted data"
       123
       4.50000E+00
      data
       (
         !@#
          (
             4.50000E+00
          )
         "(more"
         "data)"
       )
    )
 )
```



## ALGOL 68


```algol68
# S-Expressions #
CHAR nl = REPR 10;
# mode representing an S-expression #
MODE SEXPR = STRUCT( UNION( VOID, STRING, REF SEXPR ) element, REF SEXPR next );
# creates an initialises an SEXPR #
PROC new s expr = REF SEXPR: HEAP SEXPR := ( EMPTY, NIL );
# reports an error #
PROC error = ( STRING msg )VOID: print( ( "**** ", msg, newline ) );
# S-expression reader - reads and returns an S-expression from the string s #
PROC s reader = ( STRING s )REF SEXPR:
     BEGIN
        PROC at end      = BOOL: s pos > UPB s;
        PROC curr        = CHAR: IF at end THEN REPR 0 ELSE s[ s pos ] FI;
        PROC skip spaces = VOID: WHILE NOT at end AND ( curr = " " OR curr = nl ) DO s pos +:= 1 OD;
        PROC end of list = BOOL: at end OR curr = ")";
        INT s pos := LWB s;
        INT t pos;
        [ ( UPB s - LWB s ) + 1 ]CHAR token; # token text - large enough to hold the whole string if necessary #
        # adds the current character to the token #
        PROC add curr    = VOID: token[ t pos +:= 1 ] := curr;
        # get an s expression element from s #
        PROC get element = REF SEXPR:
             BEGIN
                REF SEXPR result = new s expr;
                skip spaces;
                # get token text #
                IF   at end THEN
                    # no element #
                    element OF result := EMPTY
                ELIF curr = "("  THEN
                    s pos +:= 1;
                    skip spaces;
                    IF NOT end of list
                    THEN
                        REF SEXPR nested expression = get element;
                        REF SEXPR element pos      := nested expression;
                        element OF result          := nested expression;
                        skip spaces;
                        WHILE NOT end of list
                        DO
                            element pos := next OF element pos := get element;
                            skip spaces
                        OD
                    FI;
                    IF curr = ")" THEN
                        s pos +:= 1
                    ELSE
                        error( "Missing "")""" )
                    FI
                ELIF curr = ")" THEN
                    s pos +:= 1;
                    error( "Unexpected "")""" );
                    element OF result := EMPTY
                ELSE
                    # quoted or unquoted string #
                    t pos := LWB token - 1;
                    IF curr /= """" THEN
                        # unquoted string #
                        WHILE add curr;
                              s pos +:= 1;
                              NOT at end AND curr /= " " AND curr /= "("
                                         AND curr /= ")" AND curr /= """"
                                         AND curr /= nl
                        DO SKIP OD
                    ELSE
                        # quoted string #
                        WHILE add curr;
                              s pos +:= 1;
                              NOT at end AND curr /= """"
                        DO SKIP OD;
                        IF curr /= """" THEN
                            # missing string quote #
                            error( "Unterminated string: <<" + token[ : t pos ] + ">>" )
                        ELSE
                            # have the closing quote #
                            add curr;
                            s pos +:= 1
                        FI
                    FI;
                    element OF result := token[ : t pos ]
                FI;
                result
             END # get element # ;

        REF SEXPR s expr = get element;
        skip spaces;
        IF NOT at end THEN
            # extraneuos text after the expression #
            error( "Unexpected text at end of expression: " + s[ s pos : ] )
        FI;

        s expr
     END # s reader # ;
# prints an S expression #
PROC s writer = ( REF SEXPR s expr )VOID:
     BEGIN
        # prints an S expression with a suitable indent #
        PROC print indented s expression = ( REF SEXPR s expr, INT indent )VOID:
             BEGIN
                REF SEXPR s pos := s expr;
                WHILE REF SEXPR( s pos ) ISNT REF SEXPR( NIL ) DO
                    FOR i TO indent DO print( ( " " ) ) OD;
                    CASE element OF s pos
                    IN  (VOID       ): print( ( "()", newline ) )
                     ,  (STRING    s): print( ( s,    newline ) )
                     ,  (REF SEXPR e): BEGIN
                                          print( ( "(", newline ) );
                                          print indented s expression( e, indent + 4 );
                                          FOR i TO indent DO print( ( " " ) ) OD;
                                          print( ( ")", newline ) )
                                      END
                    OUT
                        error( "Unexpected S expression element" )
                    ESAC;
                    s pos := next OF s pos
                OD
             END # print indented s expression # ;

        print indented s expression( s expr, 0 )
     END # s writer # ;
# test the eader and writer with the example from the task #
s writer( s reader( "((data ""quoted data"" 123 4.5)"
                  + nl
                  + " (data (!@# (4.5) ""(more"" ""data)"")))"
                  + nl
                  )
        )
```

{{out}}

```txt

(
    (
        data
        "quoted data"
        123
        4.5
    )
    (
        data
        (
            !@#
            (
                4.5
            )
            "(more"
            "data)"
        )
    )
)

```



## AutoHotkey


```AutoHotkey
S_Expressions(Str){
	Str := RegExReplace(Str, "s)(?<![\\])"".*?[^\\]""(*SKIP)(*F)|((?<![\\])[)(]|\s)", "`n$0`n")
	Str := RegExReplace(Str, "`am)^\s*\v+")	,	Cnt := 0
	loop, parse, Str, `n, `r
	{
		Cnt := A_LoopField=")" ? Cnt-1 : Cnt
		Res .= tabs(Cnt) A_LoopField "`r`n"
		Cnt := A_LoopField="(" ? Cnt+1 : Cnt
	}
	return Res
}
tabs(n){
	loop, % n
		Res .= "`t"
	return Res
}
```

Examples:
```AutoHotkey
Str =
(
((data da\(\)ta "quot\\ed data" 123 4.5)
 ("data" (!@# (4.5) "(mo\"re" "data)")))
)
MsgBox, 262144, , % S_Expressions(Str)
```

{{out}}

```txt
(
	(
		data
		da\(\)ta
		"quot\\ed data"
		123
		4.5
	)
	(
		"data"
		(
			!@#
			(
				4.5
			)
			"(mo\"re"
			"data)"
		)
	)
)


```



## C


```c
#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>

enum { S_NONE, S_LIST, S_STRING, S_SYMBOL };

typedef struct {
	int type;
	size_t len;
	void *buf;
} s_expr, *expr;

void whine(const char *s)
{
	fprintf(stderr, "parse error before ==>%.10s\n", s);
}

expr parse_string(const char *s, char **e)
{
	expr ex = calloc(sizeof(s_expr), 1);
	char buf[256] = {0};
	int i = 0;

	while (*s) {
		if (i >= 256) {
			fprintf(stderr, "string too long:\n");
			whine(s);
			goto fail;
		}
		switch (*s) {
		case '\\':
			switch (*++s) {
			case '\\':
			case '"':	buf[i++] = *s++;
					continue;

			default:	whine(s);
					goto fail;
			}
		case '"':	goto success;
		default:	buf[i++] = *s++;
		}
	}
fail:
	free(ex);
	return 0;

success:
	*(const char **)e = s + 1;
	ex->type = S_STRING;
	ex->buf = strdup(buf);
	ex->len = strlen(buf);
	return ex;
}

expr parse_symbol(const char *s, char **e)
{
	expr ex = calloc(sizeof(s_expr), 1);
	char buf[256] = {0};
	int i = 0;

	while (*s) {
		if (i >= 256) {
			fprintf(stderr, "symbol too long:\n");
			whine(s);
			goto fail;
		}
		if (isspace(*s)) goto success;
		if (*s == ')' || *s == '(') {
			s--;
			goto success;
		}

		switch (*s) {
		case '\\':
			switch (*++s) {
			case '\\': case '"': case '(': case ')':
					buf[i++] = *s++;
					continue;
			default:	whine(s);
					goto fail;
			}
		case '"':	whine(s);
				goto success;
		default:	buf[i++] = *s++;
		}
	}
fail:
	free(ex);
	return 0;

success:
	*(const char **)e = s + 1;
	ex->type = S_SYMBOL;
	ex->buf = strdup(buf);
	ex->len = strlen(buf);
	return ex;
}

void append(expr list, expr ele)
{
	list->buf = realloc(list->buf, sizeof(expr) * ++list->len);
	((expr*)(list->buf))[list->len - 1] = ele;
}

expr parse_list(const char *s, char **e)
{
	expr ex = calloc(sizeof(s_expr), 1), chld;
	char *next;

	ex->len = 0;

	while (*s) {
		if (isspace(*s)) {
			s++;
			continue;
		}

		switch (*s) {
		case '"':
			chld = parse_string(s+1, &next);
			if (!chld) goto fail;
			append(ex, chld);
			s = next;
			continue;
		case '(':
			chld = parse_list(s+1, &next);
			if (!chld) goto fail;
			append(ex, chld);
			s = next;
			continue;
		case ')':
			goto success;

		default:
			chld = parse_symbol(s, &next);
			if (!chld) goto fail;
			append(ex, chld);
			s = next;
			continue;
		}
	}

fail:
	whine(s);
	free(ex);
	return 0;

success:
	*(const char **)e = s+1;
	ex->type = S_LIST;
	return ex;
}

expr parse_term(const char *s, char **e)
{
	while (*s) {
		if (isspace(*s)) {
			s++;
			continue;
		}
		switch(*s) {
		case '(':
			return parse_list(s+1, e);
		case '"':
			return parse_string(s+1, e);
		default:
			return parse_symbol(s+1, e);
		}
	}
	return 0;
}

void print_expr(expr e, int depth)
{
#define sep() for(i = 0; i < depth; i++) printf("    ")
	int i;
	if (!e) return;


	switch(e->type) {
	case S_LIST:
		sep();
		puts("(");
		for (i = 0; i < e->len; i++)
			print_expr(((expr*)e->buf)[i], depth + 1);
		sep();
		puts(")");
		return;
	case S_SYMBOL:
	case S_STRING:
		sep();
		if (e->type == S_STRING) putchar('"');
		for (i = 0; i < e->len; i++) {
			switch(((char*)e->buf)[i]) {
			case '"':
			case '\\':
				putchar('\\');
				break;
			case ')': case '(':
				if (e->type == S_SYMBOL)
					putchar('\\');
			}

			putchar(((char*)e->buf)[i]);
		}
		if (e->type == S_STRING) putchar('"');
		putchar('\n');
		return;
	}
}

int main()
{
	char *next;
	const char *in = "((data da\\(\\)ta \"quot\\\\ed data\" 123 4.5)\n"
			" (\"data\" (!@# (4.5) \"(mo\\\"re\" \"data)\")))";

	expr x = parse_term(in, &next);

	printf("input is:\n%s\n", in);
	printf("parsed as:\n");
	print_expr(x, 0);
	return 0;
}
```

{{out}}<lang>input is:
((data da\(\)ta "quot\\ed data" 123 4.5)
 ("data" (!@# (4.5) "(mo\"re" "data)")))
parsed as:
(
    (
        data
        da\(\)ta
        "quot\\ed data"
        123
        4.5
    )
    (
        "data"
        (
            !@#
            (
                4.5
            )
            "(mo\"re"
            "data)"
        )
    )
)
```



## Ceylon


```ceylon
class Symbol(symbol) {
    shared String symbol;
    string => symbol;
}

abstract class Token()
        of  DataToken | leftParen | rightParen {}

abstract class DataToken(data)
        of StringToken | IntegerToken | FloatToken | SymbolToken
        extends Token() {

    shared String|Integer|Float|Symbol data;
    string => data.string;
}

class StringToken(String data) extends DataToken(data) {}
class IntegerToken(Integer data) extends DataToken(data) {}
class FloatToken(Float data) extends DataToken(data) {}
class SymbolToken(Symbol data) extends DataToken(data) {}

object leftParen extends Token() {
    string => "(";
}
object rightParen extends Token() {
    string => ")";
}

class Tokens(String input) satisfies {Token*} {
    shared actual Iterator<Token> iterator() => object satisfies Iterator<Token> {

        variable value index = 0;

        shared actual Token|Finished next() {

            while(exists nextChar = input[index], nextChar.whitespace) {
                index++;
            }

            if(index >= input.size) {
                return finished;
            }

            assert(exists char = input[index]);

            if(char == '(') {
                index++;
                return leftParen;
            }
            if(char == ')') {
                index++;
                return rightParen;
            }

            if(char == '"') {
                value builder = StringBuilder();
                while(exists nextChar = input[++index]) {
                    if(nextChar == '"') {
                        index++;
                        break;
                    }
                    if(nextChar == '\\') {
                        if(exists nextNextChar = input[++index]) {
                            switch(nextNextChar)
                            case('\\') {
                                builder.append("\\");
                            }
                            case('t') {
                                builder.append("\t");
                            }
                            case('n') {
                                builder.append("\n");
                            }
                            case('"') {
                                builder.append("\"");
                            }
                            else {
                                throw Exception("unknown escaped character");
                            }
                        } else {
                            throw Exception("unclosed string");
                        }
                    } else {
                        builder.appendCharacter(nextChar);
                    }
                }
                return StringToken(builder.string);
            }

            value builder = StringBuilder();
            while(exists nextChar = input[index], !nextChar.whitespace && nextChar != '(' && nextChar != ')') {
                builder.appendCharacter(nextChar);
                index++;
            }
            value string = builder.string;
            if(is Integer int = Integer.parse(string)) {
                return IntegerToken(int);
            } else if(is Float float = Float.parse(string)) {
                return FloatToken(float);
            } else {
                return SymbolToken(Symbol(string));
            }
        }
    };
}

abstract class Node() of Atom | Group {}

class Atom(data) extends Node() {
    shared String|Integer|Float|Symbol data;
    string => data.string;
}
class Group() extends Node() satisfies {Node*} {
    shared variable Node[] nodes = [];
    string => nodes.string;
    shared actual Iterator<Node> iterator() => nodes.iterator();

}

Node buildTree(Tokens tokens) {

    [Group, Integer] recursivelyBuild(Token[] tokens, variable Integer index = 0) {
        value result = Group();
        while(exists token = tokens[index]) {
            switch (token)
            case (leftParen) {
                value [newNode, newIndex] = recursivelyBuild(tokens, index + 1);
                index = newIndex;
                result.nodes = result.nodes.append([newNode]);
            }
            case (rightParen) {
                return [result, index];
            }
            else {
                result.nodes = result.nodes.append([Atom(token.data)]);
            }
            index++;
        }
        return [result, index];
    }

    value root = recursivelyBuild(tokens.sequence())[0];
    return root.first else Group();
}

void prettyPrint(Node node, Integer indentation = 0) {

    void paddedPrint(String s) => print(" ".repeat(indentation) + s);

    if(is Atom node) {
        paddedPrint(node.string);
    } else {
        paddedPrint("(");
        for(n in node.nodes) {
            prettyPrint(n, indentation + 2);
        }
        paddedPrint(")");
    }
}

shared void run() {
    value tokens = Tokens("""((data "quoted data" 123 4.5)
                               (data (!@# (4.5) "(more" "data)")))""");
    print(tokens);

    value tree = buildTree(tokens);
    prettyPrint(tree);
}

```

{{out}}

```txt

(
  (
    data
    quoted data
    123
    4.5
  )
  (
    data
    (
      !@#
      (
        4.5
      )
      (more
      data)
    )
  )
)

```



## CoffeeScript

{{improve|CoffeeScript|This solution does not reproduce unquoted strings as per task description}}

```coffeescript

# This code works with Lisp-like s-expressions.
#
# We lex tokens then do recursive descent on the tokens
# to build our data structures.

sexp = (data) ->
  # Convert a JS data structure to a string s-expression.  A sexier version
  # would remove quotes around strings that don't need them.
  s = ''
  if Array.isArray data
    children = (sexp elem for elem in data).join ' '
    '(' + children + ')'
  else
    return JSON.stringify data

parse_sexp = (sexp) ->
  tokens = lex_sexp sexp
  i = 0

  _parse_list = ->
    i += 1
    arr = []
    while i < tokens.length and tokens[i].type != ')'
      arr.push _parse()
    if i < tokens.length
      i += 1
    else
     throw Error "missing end paren"
    arr

  _guess_type = (word) ->
    # This is crude, doesn't handle all forms of floats.
    if word.match /^\d+\.\d+$/
      parseFloat(word)
    else if word.match /^\d+/
      parseInt(word)
    else
      word

  _parse_word = ->
    token = tokens[i]
    i += 1
    if token.type == 'string'
      token.word
    else
      _guess_type token.word

  _parse = ->
    return undefined unless i < tokens.length
    token = tokens[i]
    if token.type == '('
      _parse_list()
    else
      _parse_word()

  exp = _parse()
  throw Error "premature termination" if i < tokens.length
  exp

lex_sexp = (sexp) ->
  is_whitespace = (c) -> c in [' ', '\t', '\n']
  i = 0
  tokens = []

  test = (f) ->
    return false unless i < sexp.length
    f(sexp[i])

  eat_char = (c) ->
    tokens.push
      type: c
    i += 1

  eat_whitespace = ->
    i += 1
    while test is_whitespace
      i += 1

  eat_word = ->
    token = c
    i += 1
    word_char = (c) ->
      c != ')' and !is_whitespace c
    while test word_char
      token += sexp[i]
      i += 1
    tokens.push
      type: "word"
      word: token

  eat_quoted_word = ->
    start = i
    i += 1
    token = ''
    while test ((c) -> c != '"')
      if sexp[i] == '\\'
        i += 1
        throw Error("escaping error") unless i < sexp.length
      token += sexp[i]
      i += 1
    if test ((c) -> c == '"')
      tokens.push
        type: "string"
        word: token
      i += 1
    else
      throw Error("end quote missing #{sexp.substring(start, i)}")

  while i < sexp.length
    c = sexp[i]
    if c == '(' or c == ')'
      eat_char c
    else if is_whitespace c
      eat_whitespace()
    else if c == '"'
      eat_quoted_word()
    else
      eat_word()
  tokens

do ->
  input = """
    ((data "quoted data with escaped \\"" 123 4.5 "14")
     (data (!@# (4.5) "(more" "data)")))
  """
  console.log "input:\n#{input}\n"
  output = parse_sexp(input)
  pp = (data) -> JSON.stringify data, null, '  '
  console.log "output:\n#{pp output}\n"
  console.log "round trip:\n#{sexp output}\n"

```

{{out}}
<lang>
> coffee sexp.coffee
input:
((data "quoted data with escaped \"" 123 4.5 "14")
 (data (!@# (4.5) "(more" "data)")))

output:
[
  [
    "data",
    "quoted data with escaped \"",
    123,
    4.5,
    "14"
  ],
  [
    "data",
    [
      "!@#",
      [
        4.5
      ],
      "(more",
      "data)"
    ]
  ]
]

round trip:
(("data" "quoted data with escaped \"" 123 4.5 "14") ("data" ("!@#" (4.5) "(more" "data)")))

```



## Common Lisp

===Parsing S-Expressions===
Like most Lisp dialects, Common Lisp uses s-expressions to represent
both source code and data.
Therefore, a properly formatted s-expression is easily "read"
then "evaluated" within the top-level REPL (read-eval-print-loop).

```txt
CL-USER> (read-from-string "((data \"quoted data\" 123 4.5) (data (!@# (4.5) \"(more\" \"data)\")))")
((DATA "quoted data" 123 4.5) (DATA (!@# (4.5) "[more" "data]")))
65
CL-USER> (caar *)  ;;The first element from the first sublist is DATA
DATA
CL-USER> (eval (read-from-string "(concatenate 'string \"foo\" \"bar\")"))  ;;An example with evaluation
"foobar"
CL-USER>
```

Unfortunately, our pointy-haired boss has asked you to write a parser for an unusual s-expression syntax that uses square brackets instead of parenthesis.  In most programming languages, this would necessitate writing an entire parser.  Fortunately, the Common Lisp reader can be modified through the use of macro-characters to accomplish this task.  When the reader parses a macro-character token, a function associated with the macro-character is called.  As evidenced below, modifying the behavior of the Lisp reader by setting macro-character functions to handle additional sytax requires far less work than writing a complete parser from scratch.


```lisp
(defun lsquare-reader (stream char)
  (declare (ignore char))
  (read-delimited-list #\] stream t))
(set-macro-character #\[ #'lsquare-reader)               ;;Call the lsquare-reader function when a '[' token is parsed
(set-macro-character #\] (get-macro-character #\) nil))  ;;Do the same thing as ')' when a ']' token is parsed
```

Unit test code:

```lisp
;;A list of unit tests.  Each test is a cons in which the car (left side) contains the
;;test string and the cdr (right side) the expected result of reading the S-Exp.
(setf unit-tests
      (list
       (cons "[]" NIL)
       (cons "[a b c]" '(a b c))
       (cons "[\"abc\" \"def\"]" '("abc" "def"))
       (cons "[1 2 [3 4 [5]]]" '(1 2 (3 4 (5))))
       (cons "[\"(\" 1 2 \")\"]" '("(" 1 2 ")"))
       (cons "[4/8 3/6 2/4]" '(1/2 1/2 1/2))
       (cons "[reduce #'+ '[1 2 3]]" '(reduce #'+ '(1 2 3)))))

(defun run-tests ()
  (dolist (test unit-tests)
    (format t "String: ~23s  Expected: ~23s  Actual: ~s~%"
	    (car test) (cdr test) (read-from-string (car test)))))
```

{{out| Unit test output}}

```txt
CL-USER> (run-tests)
String: "[]"                     Expected: NIL                      Actual: NIL
String: "[a b c]"                Expected: (A B C)                  Actual: (A B C)
String: "[\"abc\" \"def\"]"      Expected: ("abc" "def")            Actual: ("abc" "def")
String: "[1 2 [3 4 [5]]]"        Expected: (1 2 (3 4 (5)))          Actual: (1 2 (3 4 (5)))
String: "[\"(\" 1 2 \")\"]"      Expected: ("(" 1 2 ")")            Actual: ("(" 1 2 ")")
String: "[4/8 3/6 2/4]"          Expected: (1/2 1/2 1/2)            Actual: (1/2 1/2 1/2)
String: "[reduce #'+ '[1 2 3]]"  Expected: (REDUCE #'+ '(1 2 3))    Actual: (REDUCE #'+ '(1 2 3))
```

Error testing:

```txt
(read-from-string "[\"ab\"\"]")  ;;Error:  Object ends with a string.
(read-from-string "[)")          ;;Error:  An object cannot start with ')'
(read-from-string "(]")          ;;Error:  An object cannot start with ']'
```

{{out|Task output}}

```txt
CL-USER>  (setf task "[[data \"quoted data\" 123 4.5] [data [!@# [4.5] \"[more\" \"data]\"]]]")
"[[data \"quoted data\" 123 4.5] [data [!@# [4.5] \"[more\" \"data]\"]]]"
CL-USER> (read-from-string task)
((DATA "quoted data" 123 4.5) (DATA (!@# (4.5) "[more" "data]")))
65
CL-USER> (setf testing *)  ;; * is the previous result in SLIME
((DATA "quoted data" 123 4.5) (DATA (!@# (4.5) "[more" "data]")))
CL-USER> (car testing)
(DATA "quoted data" 123 4.5)
CL-USER> (caar testing)
DATA
CL-USER> (cdar testing)
("quoted data" 123 4.5)
CL-USER> (cadar testing)
"quoted data"
CL-USER>
```


===Writing S-Expressions===
The next step in this task is to write a standard Lisp s-expression in the square bracket notation.

```lisp
(defun write-sexp (sexp)
  "Writes a Lisp s-expression in square bracket notation."
  (labels ((parse (sexp)
	     (cond ((null sexp) "")
		   ((atom sexp) (format nil "~s " sexp))
		   ((listp sexp)
		    (concatenate
		     'string
		     (if (listp (car sexp))
			 (concatenate 'string "["
				      (fix-spacing (parse (car sexp)))
				      "] ")
			 (parse (car sexp)))
		     (parse (cdr sexp))))))
	   (fix-spacing (str)
	     (let ((empty-string ""))
	       (unless (null str)
		 (if (equal str empty-string)
		     empty-string
		     (let ((last-char (1- (length str))))
		       (if (eq #\Space (char str last-char))
			   (subseq str 0 last-char)
			   str)))))))
    (concatenate 'string "[" (fix-spacing (parse sexp)) "]")))
```

Unit test code:

```lisp
(setf unit-tests '(((1 2) (3 4)) (1 2 3 4) ("ab(cd" "mn)op")
		   (1 (2 (3 (4)))) ((1) (2) (3)) ()))

(defun run-tests ()
  (dolist (test unit-tests)
    (format t "Before: ~18s  After: ~s~%"
	    test (write-sexp test))))
```

{{out|Unit test output}}

```txt
CL-USER> (run-tests)
Before: ((1 2) (3 4))       After: "[[1 2] [3 4]]"
Before: (1 2 3 4)           After: "[1 2 3 4]"
Before: ("ab(cd" "mn)op")   After: "[\"ab(cd\" \"mn)op\"]"
Before: (1 (2 (3 (4))))     After: "[1 [2 [3 [4]]]]"
Before: ((1) (2) (3))       After: "[[1] [2] [3]]"
Before: NIL                 After: "[]"
```

{{out|Finally, round trip output for the original task example}}

```txt
CL-USER> task
"[[data \"quoted data\" 123 4.5] [data [!@# [4.5] \"[more\" \"data]\"]]]"
CL-USER> (read-from-string task)
((DATA "quoted data" 123 4.5) (DATA (!@# (4.5) "[more" "data]")))
65
CL-USER> (write-sexp *)  ; * is the previous return value
"[[DATA \"quoted data\" 123 4.5] [DATA [!@# [4.5] \"[more\" \"data]\"]]]"
CL-USER>
```



## C#

Implementation of S-expression serializer & deserializer in C# 6.0 language.

Git repository with code and tests can be found here: https://github.com/ichensky/SExpression/tree/rosettacode


```csharp

using System;
using System.Collections.Generic;
using System.Text;

  public class SNode
    {
        private List<SNode> _items;
        public string Name { get; set; }
        public IReadOnlyCollection<SNode> Items { get { return _items.AsReadOnly(); } }
        public SNode()
        {
            this._items = new List<SNode>();
        }
        public SNode(string name):this()
        {
            this.Name=name;
        }
        public void AddNode(SNode node)
        {
            this._items.Add(node);
        }
    }

    public class SNodeFull : SNode
    {
        private bool _isLeaf;
        public bool IsLeaf { get => _isLeaf; }
        public SNodeFull(bool isLeaf) : base()
        {
            this._isLeaf = isLeaf;
        }

        public SNodeFull(string name, bool isLeaf) : base(name)
        {
            this._isLeaf = isLeaf;
        }

        public SNodeFull RootNode { get; set; }

        public void AddNode(SNodeFull node)
        {
            base.AddNode(node);
            node.RootNode = this;
        }
    }


```


```csharp

using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace SExpression
{
    public partial class SExpression
    {
        public const string ErrorStrNotValidFormat = "Not valid format.";
    }
    public partial class SExpression : ISExpression
    {
        public String Serialize(SNode root)
        {
            if (root == null)
            {
                throw new ArgumentNullException();
            }
            var sb = new StringBuilder();
            Serialize(root, sb);
            return sb.ToString();
        }
        private void Serialize(SNode node, StringBuilder sb)
        {
            sb.Append('(');

            if (node.Items.Count > 0)
            {
                int x = 0;
                foreach (var item in node.Items)
                {
                    if (x>0)
                    {
                    sb.Append(' ');
                    }
                    else
                    {
                        x++;
                    }
                    if (item.Items.Count > 0)
                    {
                        Serialize(item, sb);
                    }
                    else
                    {
                        SerializeItem(item, sb);
                    }
                }
            }

            sb.Append(')');
        }
        private void SerializeItem(SNode node, StringBuilder sb)
        {
            if (node.Name == null)
            {
                sb.Append("()");
                return;
            }
            node.Name = node.Name.Replace("\"", "\\\"");
            if (node.Name.IndexOfAny(new char[] { ' ', '"', '(', ')' }) != -1 || node.Name == string.Empty)
            {
                sb.Append('"').Append(node.Name).Append('"');
                return;
            }
            sb.Append(node.Name);
        }
    }
    public partial class SExpression
    {
        public SNode Deserialize(string st)
        {
            if (st==null)
            {
                return null;
            }
            st = st.Trim();
            if (string.IsNullOrEmpty(st))
            {
                return null;
            }

            var begin = st.IndexOf('(');
            if (begin != 0)
            {
                throw new Exception();
            }
            var end = st.LastIndexOf(')');
            if (end != st.Length - 1)
            {
                throw new Exception(ErrorStrNotValidFormat);
            }
            st = st.Remove(st.Length-1).Remove(0, 1).ToString();
            var node = new SNodeFull(false);
            Deserialize(ref st, node);
            return node;
        }

        private void Deserialize(ref string st, SNodeFull root)
        {
            st = st.Trim();
            if (string.IsNullOrEmpty(st))
            {
                return;
            }

            SNodeFull node = null;
            SNodeFull r = root;
            do
            {
                while (st[0] == ')')
                {
                    st = st.Remove(0, 1).Trim();
                    if (st.Length==0)
                    {
                        return;
                    }
                    r = root.RootNode;
                    if (r==null)
                    {
                        throw new Exception(ErrorStrNotValidFormat);
                    }
                }
                node = DeserializeItem(ref st);
                st = st.Trim();

                r.AddNode(node);

                if (!node.IsLeaf)
                {
                    Deserialize(ref st,node);
                }
            }
            while (st.Length > 0);

        }

        private SNodeFull DeserializeItem(ref string st)
        {
            if (st[0] == '(')
            {
                st = st.Remove(0, 1);
                return new SNodeFull(false);
            }

            var x = 0;
            var esc = 0;
            for (int i = 0; i < st.Length; i++)
            {
                if (st[i] == '"')
                {
                    if (esc == 0)
                    {
                        esc = 1;
                    }
                    else if(esc == 1 && (i> 0 && st[i - 1] == '\\'))
                    {
                        throw new Exception(ErrorStrNotValidFormat);
                    }
                    else
                    {
                        esc = 2;
                        break;
                    }
                }
                else if (esc==0 && " ()".Contains(st[i]))
                {
                    break;
                }

                x++;
            }
            if (esc == 1)
            {
                throw new Exception(ErrorStrNotValidFormat);
            }

            var head = esc==0? st.Substring(0, x): st.Substring(1,x-1);
            st = st.Remove(0, esc ==0 ? x: x + 2);
            return new SNodeFull(head, true);
        }
    }
}

```


```csharp

using System;
using System.Collections.Generic;

namespace SExpression.Test
{
    class Program
    {
        static void Main(string[] args)
        {
            var str =
@"((data ""quoted data"" 123 4.5)
(data(!@# (4.5) ""(more"" ""data)"")))";

            var se = new SExpression();
            var node = se.Deserialize(str);
            var result = se.Serialize(node);
            Console.WriteLine(result);
        }
    }
}

```

{{out}}

```txt

((data "quoted data" 123 4.5) (data (!@# (4.5) "(more" "data)")))

```



## D


```d
import std.stdio, std.conv, std.algorithm, std.variant, std.uni,
       std.functional, std.string;

alias Sexp = Variant;

struct Symbol {
    private string name;
    string toString() @safe const pure nothrow { return name; }
}

Sexp parseSexp(string txt) @safe pure /*nothrow*/ {
    static bool isIdentChar(in char c) @safe pure nothrow {
        return c.isAlpha || "0123456789!@#-".representation.canFind(c);
    }

    size_t pos = 0;

    Sexp _parse() /*nothrow*/ {
        auto i = pos + 1;
        scope (exit)
            pos = i;
        if (txt[pos] == '"') {
            while (txt[i] != '"' && i < txt.length)
                i++;
            i++;
            return Sexp(txt[pos + 1 .. i - 1]);
        } else if (txt[pos].isNumber) {
            while (txt[i].isNumber && i < txt.length)
                i++;
            if (txt[i] == '.') {
                i++;
                while (txt[i].isNumber && i < txt.length)
                    i++;
                auto aux = txt[pos .. i]; //
                return aux.parse!double.Sexp;
            }
            auto aux = txt[pos .. i]; //
            return aux.parse!ulong.Sexp;
        } else if (isIdentChar(txt[pos])) {
            while (isIdentChar(txt[i]) && i < txt.length)
                i++;
            return Sexp(Symbol(txt[pos .. i]));
        } else if (txt[pos] == '(') {
            Sexp[] lst;
            while (txt[i] != ')') {
                while (txt[i].isWhite)
                    i++;
                pos = i;
                lst ~= _parse;
                i = pos;
                while (txt[i].isWhite)
                    i++;
            }
            i = pos + 1;
            return Sexp(lst);
        }
        return Sexp(null);
    }

    txt = txt.find!(not!isWhite);
    return _parse;
}

void writeSexp(Sexp expr) {
    if (expr.type == typeid(string)) {
        write('"', expr, '"');
    } else if (expr.type == typeid(Sexp[])) {
        '('.write;
        auto arr = expr.get!(Sexp[]);
        foreach (immutable i, e; arr) {
            e.writeSexp;
            if (i + 1 < arr.length)
                ' '.write;
        }
        ')'.write;
    } else {
        expr.write;
    }
}

void main() {
    auto pTest = `((data "quoted data" 123 4.5)
                   (data (!@# (4.5) "(more" "data)")))`.parseSexp;
    writeln("Parsed: ", pTest);
    "Printed: ".write;
    pTest.writeSexp;
}
```

{{out}}

```txt
Parsed: [[data, quoted data, 123, 4.5], [data, [!@#, [4.5], (more, data)]]]
Printed: ((data "quoted data" 123 4.5) (data (!@# (4.5) "(more" "data)")))
```



## EchoLisp

The '''(read-from-string  input-string)''' function parses a string into an s-expression, which is the native representation of program/data in EchoLisp and the majority of Lisps .

```lisp

(define input-string #'((data "quoted data" 123 4.5)\n(data (!@# (4.5) "(more" "data)")))'#)

input-string
    → "((data "quoted data" 123 4.5)
    (data (!@# (4.5) "(more" "data)")))"

(define s-expr (read-from-string input-string))
s-expr
    → ((data "quoted data" 123 4.5) (data (!@# (4.5) "(more" "data)")))

(first s-expr)
    → (data "quoted data" 123 4.5)
(first(first s-expr))
    → data
(first(rest s-expr))
    → (data (!@# (4.5) "(more" "data)"))

```



## Factor

Factor's nested sequences are close enough to s-expressions that in most cases we can simply <code>eval</code> s-expression strings after some minor whitespace/bracket/parenthesis transformations. However, if we wish to support symbols, this approach becomes complicated because symbols need to be declared before use. This means we need to go into the string and identify them, so we may as well parse the s-expression properly while we're there.

We have a nice tool at our disposal for doing this. In its standard library, Factor contains a domain-specific language for defining [[wp:extended Backus-Naur form|extended Backus-Naur form]] (EBNF) grammars. EBNF is a convenient, declarative way to describe different parts of a grammar where later rules build on earlier ones until the final rule defines the entire grammar. Upon calling the word defined by <code>EBNF:</code>, an input string will be tokenized according to the declared rules and stored in an abstract syntax tree.

To get an idea of how this works, look at the final rule. It declares that an s-expression is any number of objects (comprised of numbers, floats, strings, and symbols) and s-expressions (the rule is recursive, allowing for nested s-expressions) surrounded by parenthesis which are in turn surrounded by any amount of whitespace. This implementation of EBNF allows us to define actions: the quotation after the <code>=></code> is called on the rule token just before being added to the abstract syntax tree. This is convenient for our use case where we need to parse different types of objects into our sequence structure.

Factor has a comprehensive prettyprinter which can print any Factor object in a readable way. Not only can we leverage it to easily print our native data structure, but we can also call <code>unparse</code> to convert it to a string. This leaves us with a string reminiscent of the original input, and we are able to take it the rest of the way with two simple regular expressions.


```factor
USING: formatting kernel math.parser multiline peg peg.ebnf
regexp sequences prettyprint words ;
IN: rosetta-code.s-expressions

STRING: input
((data "quoted data" 123 4.5)
 (data (!@# (4.5) "(more" "data)")))
;

EBNF: sexp>seq [=[
  ws     = [\n\t\r ]* => [[ drop ignore ]]
  digits = [0-9]+
  number = digits => [[ string>number ]]
  float  = digits:a "." digits:b => [[ a b "." glue string>number ]]
  string = '"'~ [^"]* '"'~ => [[ "" like ]]
  symbol = [!#-'*-~]+ => [[ "" like <uninterned-word> ]]
  object = ws ( float | number | string | symbol ) ws
  sexp   = ws "("~ ( object | sexp )* ")"~ ws => [[ { } like ]]
]=]

: seq>sexp ( seq -- str )
    unparse R/ {\s+/ "(" R/ \s+}/ ")" [ re-replace ] 2bi@ ;

input [ "Input:\n%s\n\n" printf ] [
    sexp>seq dup seq>sexp
    "Native:\n%u\n\nRound trip:\n%s\n" printf
] bi
```

{{out}}

```txt

Input:
((data "quoted data" 123 4.5)
 (data (!@# (4.5) "(more" "data)")))

Native:
{
    { data "quoted data" 123 4.5 }
    { data { !@# { 4.5 } "(more" "data)" } }
}

Round trip:
((data "quoted data" 123 4.5)
    (data (!@# (4.5) "(more" "data)")))

```



## Go


```go
package main

import (
    "errors"
    "fmt"
    "reflect"
    "strconv"
    "strings"
    "unicode"
)

var input = `((data "quoted data" 123 4.5)
 (data (!@# (4.5) "(more" "data)")))`

func main() {
    fmt.Println("input:")
    fmt.Println(input)

    s, err := parseSexp(input)
    if err != nil {
        fmt.Println("error:", err)
        return
    }

    fmt.Println("\nparsed:")
    fmt.Println(s)

    fmt.Println("\nrepresentation:")
    s.dump(0)
}

// dynamic types for i are string, qString, int, float64, list, and error.
type sexp struct {
    i interface{}
}
type qString string
type list []sexp

func (s sexp) String() string {
    return fmt.Sprintf("%v", s.i)
}

func (q qString) String() string {
    return strconv.Quote(string(q))
}

func (l list) String() string {
    if len(l) == 0 {
        return "()"
    }
    b := fmt.Sprintf("(%v", l[0])
    for _, s := range l[1:] {
        b = fmt.Sprintf("%s %v", b, s)
    }
    return b + ")"
}

// parseSexp parses a string into a Go representation of an s-expression.
//
// Quoted strings go from one " to the next.  There is no escape character,
// all characters except " are valid.
//
// Otherwise atoms are any string of characters between any of '(', ')',
// '"', or white space characters.  If the atom parses as a Go int type
// using strconv.Atoi, it is taken as int; if it parses as a Go float64
// type using strconv.ParseFloat, it is taken as float64; otherwise it is
// taken as an unquoted string.
//
// Unmatched (, ), or " are errors.
// An empty or all whitespace input string is an error.
// Left over text after the sexp is an error.
//
// An empty list is a valid sexp, but there is no nil, no cons, no dot.
func parseSexp(s string) (sexp, error) {
    s1, rem := ps2(s, -1)
    if err, isErr := s1.i.(error); isErr {
        return sexp{}, err
    }
    if rem > "" {
        return s1, errors.New("Left over text: " + rem)
    }
    return s1, nil
}

// recursive.  n = -1 means not parsing a list.  n >= 0 means the number
// of list elements parsed so far.  string result is unparsed remainder
// of the input string s0.
func ps2(s0 string, n int) (x sexp, rem string) {
    tok, s1 := gettok(s0)
    switch t := tok.(type) {
    case error:
        return sexp{tok}, s1
    case nil: // this is also an error
        if n < 0 {
            return sexp{errors.New("blank input string")}, s0
        } else {
            return sexp{errors.New("unmatched (")}, ""
        }
    case byte:
        switch {
        case t == '(':
            x, s1 = ps2(s1, 0) // x is a list
            if _, isErr := x.i.(error); isErr {
                return x, s0
            }
        case n < 0:
            return sexp{errors.New("unmatched )")}, ""
        default:
            // found end of list.  allocate space for it.
            return sexp{make(list, n)}, s1
        }
    default:
        x = sexp{tok} // x is an atom
    }
    if n < 0 {
        // not in a list, just return the s-expression x
        return x, s1
    }
    // in a list.  hold on to x while we parse the rest of the list.
    l, s1 := ps2(s1, n+1)
    // result l is either an error or the allocated list, not completely
    // filled in yet.
    if _, isErr := l.i.(error); !isErr {
        // as long as no errors, drop x into its place in the list
        l.i.(list)[n] = x
    }
    return l, s1
}

// gettok gets one token from string s.
// return values are the token and the remainder of the string.
// dynamic type of tok indicates result:
// nil:  no token.  string was empty or all white space.
// byte:  one of '(' or ')'
// otherwise string, qString, int, float64, or error.
func gettok(s string) (tok interface{}, rem string) {
    s = strings.TrimSpace(s)
    if s == "" {
        return nil, ""
    }
    switch s[0] {
    case '(', ')':
        return s[0], s[1:]
    case '"':
        if i := strings.Index(s[1:], `"`); i >= 0 {
            return qString(s[1 : i+1]), s[i+2:]
        }
        return errors.New(`unmatched "`), s
    }
    i := 1
    for i < len(s) && s[i] != '(' && s[i] != ')' && s[i] != '"' &&
        !unicode.IsSpace(rune(s[i])) {
        i++
    }
    if j, err := strconv.Atoi(s[:i]); err == nil {
        return j, s[i:]
    }
    if f, err := strconv.ParseFloat(s[:i], 64); err == nil {
        return f, s[i:]
    }
    return s[:i], s[i:]
}

func (s sexp) dump(i int) {
    fmt.Printf("%*s%v: ", i*3, "", reflect.TypeOf(s.i))
    if l, isList := s.i.(list); isList {
        fmt.Println(len(l), "elements")
        for _, e := range l {
            e.dump(i + 1)
        }
    } else {
        fmt.Println(s.i)
    }
}
```

{{out}}

```txt

input:
((data "quoted data" 123 4.5)
 (data (!@# (4.5) "(more" "data)")))

parsed:
((data "quoted data" 123 4.5) (data (!@# (4.5) "(more" "data)")))

representation:
main.list: 2 elements
   main.list: 4 elements
      string: data
      main.qString: "quoted data"
      int: 123
      float64: 4.5
   main.list: 2 elements
      string: data
      main.list: 4 elements
         string: !@#
         main.list: 1 elements
            float64: 4.5
         main.qString: "(more"
         main.qString: "data)"

```



## Haskell


```haskell
import Data.Functor
import Text.Parsec ((<|>), (<?>), many, many1, char, try, parse, sepBy, choice, between)
import Text.Parsec.Char (noneOf)
import Text.Parsec.Token (integer, float, whiteSpace, stringLiteral, makeTokenParser)
import Text.Parsec.Language (haskell)

data Val = Int Integer
         | Float Double
         | String String
         | Symbol String
         | List [Val] deriving (Eq, Show)

tProg = many tExpr <?> "program"
  where tExpr = between ws ws (tList <|> tAtom) <?> "expression"
        ws = whiteSpace haskell
        tAtom  =  (try (Float <$> float haskell) <?> "floating point number")
              <|> (try (Int <$> integer haskell) <?> "integer")
              <|> (String <$> stringLiteral haskell <?> "string")
              <|> (Symbol <$> many1 (noneOf "()\"\t\n\r ") <?> "symbol")
              <?> "atomic expression"
        tList = List <$> between (char '(') (char ')') (many tExpr) <?> "list"

p = either print (putStrLn . unwords . map show) . parse tProg ""

main = do
    let expr = "((data \"quoted data\" 123 4.5)\n  (data (!@# (4.5) \"(more\" \"data)\")))"
    putStrLn ("The input:\n" ++ expr ++ "\nParsed as:")
    p expr
```


=={{header|Icon}} and {{header|Unicon}}==
The following should suffice as a demonstration.

String escaping and quoting could be handled more robustly.

The example takes single and double quotes.

Single quotes were used instead of doubles in the input.

```Icon
link ximage

procedure main()
  in := "((data 'quoted data' 123 4.5) (data (!@# (4.5) '(more' 'data)')))"
  # in := map(in,"'","\"") # uncomment to put back double quotes if desired
  write("Input:  ",image(in))
  write("Structure: \n",ximage(S := string2sexp(in)))
  write("Output:  ",image(sexp2string(S)))
end

procedure sexp2string(S)   #: return a string representing the s-expr
   s := ""
   every t := !S do {
      if type(t) == "list" then
         s ||:= "(" || trim(sexp2string(t)) || ")"
      else
         if upto('() \t\r\n',t) then
            s ||:= "'" || t || "'"
         else
            s ||:= t
      s ||:= " "
      }
   return trim(s)
end

procedure string2sexp(s)   #: return a s-expression nested list
   if s ? ( sexptokenize(T := []), pos(0) ) then
      return sexpnest(T)
   else
      write("Malformed: ",s)
end

procedure sexpnest(T,L)   #: transform s-expr token list to nested list
   /L := []
   while t := get(T) do
      case t of {
         "("      :  {
                     put(L,[])
                     sexpnest(T,L[*L])
                     }
         ")"      :  return L
         default  :  put(L, numeric(t) | t)
      }
      return L
end

procedure sexptokenize(T) #: return list of tokens parsed from an s-expr string
static sym
initial sym := &letters++&digits++'~`!@#$%^&*_-+|;:.,<>[]{}'
   until pos(0) do
      case &subject[&pos] of {
         " "   :  tab(many(' \t\r\n'))                     # consume whitespace
         "'"|"\""  :
            (q := move(1)) & put(T,tab(find(q))) & move(1) # quotes
         "("   :  put(T,move(1)) & sexptokenize(T)         # open
         ")"   :  put(T,move(1)) &return T                 # close
         default  : put(T, tab(many(sym)))                 # other symbols
         }
   return T
end
```


{{libheader|Icon Programming Library}}
[http://www.cs.arizona.edu/icon/library/src/procs/ximage.icn ximage.icn formats arbitrary structures into printable strings]

{{Out}}

```txt
Input:  "((data 'quoted data' 123 4.5) (data (!@# (4.5) '(more' 'data)')))"
Structure:
L2 := list(1)
   L2[1] := L3 := list(2)
      L3[1] := L4 := list(4)
         L4[1] := "data"
         L4[2] := "quoted data"
         L4[3] := 123
         L4[4] := 4.5
      L3[2] := L5 := list(2)
         L5[1] := "data"
         L5[2] := L6 := list(4)
            L6[1] := "!@#"
            L6[2] := L7 := list(1)
               L7[1] := 4.5
            L6[3] := "(more"
            L6[4] := "data)"
Output:  "((data 'quoted data' 123 4.5) (data (!@# (4.5) '(more' 'data)')))"
```



## J


Since J already has a way of expressing nested lists, this implementation is for illustration purposes only.  No attempt is made to handle arrays which are not representable using sexpr syntax.

This implementation does not support escape characters.  If escape characters were added, we would need additional support in the tokenizer (an extra character class, and in the state table an extra column and two extra rows, or almost double the number of state transitions: 35 instead of 20), and additional support in the data language (unfmt would need to strip out escape characters and fmt would need to insert escape characters -- so each of these routines would also perhaps double in size.)  And that's a lot of bulk for serialize/deserialize mechanism which, by design, cannot represent frequently used data elements (such as matrices and gerunds).


```j
NB. character classes: 0: paren, 1: quote, 2: whitespace, 3: wordforming (default)
chrMap=: '()';'"';' ',LF,TAB,CR

NB. state columns correspond to the above character classes
NB. first digit chooses next state.
NB. second digit is action 0: do nothing, 1: start token, 2: end token
states=: 10 10#: ".;._2]0 :0
  11  21  00  31  NB. state 0: initial state
  12  22  02  32  NB. state 1: after () or after closing "
  40  10  40  40  NB. state 2: after opening "
  12  22  02  30  NB. state 3: after word forming character
  40  10  40  40  NB. state 4: between opening " and closing "
)

tokenize=: (0;states;<chrMap)&;:

rdSexpr=:3 :0 :.wrSexpr
  s=. r=. '' [ 'L R'=. ;:'()'
  for_token. tokenize y do.
    select. token
      case. L do. r=. ''  [ s=. s,<r
      case. R do. s=. }:s [ r=. (_1{::s),<r
      case.   do. r=. r,token
    end.
  end.
  >{.r
)

wrSexpr=: ('(' , ;:^:_1 , ')'"_)^:L.L:1^:L. :.rdSexpr

fmt=: 3 :0 :.unfmt
  if. '"' e. {.y     do. }.,}: y  NB. quoted string
  elseif. 0=#$n=.".y do. n        NB. number or character
  elseif.            do. s:<y     NB. symbol
  end.
)

unfmt=: 3 :0 :.fmt
  select. 3!:0 y
    case. 1;4;8;16;128 do. ":!.20 y
    case. 2;131072     do.
      select. #$y
        case. 0 do. '''',y,''''
        case. 1 do. '"',y,'"'
      end.
    case. 64           do. (":y),'x'
    case. 65536        do. >s:inv y
  end.
)

readSexpr=: fmt L:0 @rdSexpr :.writeSexpr
writeSexpr=: wrSexpr @(unfmt L:0) :.readSexpr
```



Example use:


```j
   readSexpr '((data "quoted data" 123 4.5) (data (!@# (4.5) "(more" "data)")))'
┌───────────────────────────┬────────────────────────────────┐
│┌─────┬───────────┬───┬───┐│┌─────┬────────────────────────┐│
││`data│quoted data│123│4.5│││`data│┌────┬─────┬─────┬─────┐││
│└─────┴───────────┴───┴───┘││     ││`!@#│┌───┐│(more│data)│││
│                           ││     ││    ││4.5││     │     │││
│                           ││     ││    │└───┘│     │     │││
│                           ││     │└────┴─────┴─────┴─────┘││
│                           │└─────┴────────────────────────┘│
└───────────────────────────┴────────────────────────────────┘
   writeSexpr readSexpr '((data "quoted data" 123 4.5) (data (!@# (4.5) "(more" "data)")))'
((data "quoted data" 123 4.5) (data (!@# (4.5) "(more" "data)")))
```



## Java


This code is based on [http://jfkbits.blogspot.com/2008/05/thoughts-on-s-expression-parser.html] and [http://jfkbits.blogspot.com/2008/05/thoughts-on-s-expression-lexer.html]
It is available under the GPL. The author is Joel F. Klein. He has graciously given permission to share the code under the FDL for the purpose of publishing it on RosettaCode.

The code as presented here is taken unmodified from an application being developed right now where it is used.


### =LispTokenizer.java=


```java
package jfkbits;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.Reader;
import java.io.StreamTokenizer;
import java.io.StringReader;
import java.util.Iterator;

public class LispTokenizer implements Iterator<Token>
{
    // Instance variables have default access to allow unit tests access.
    StreamTokenizer m_tokenizer;
    IOException m_ioexn;

    /** Constructs a tokenizer that scans input from the given string.
     * @param src A string containing S-expressions.
     */
    public LispTokenizer(String src)
    {
        this(new StringReader(src));
    }

    /** Constructs a tokenizer that scans input from the given Reader.
     * @param r Reader for the character input source
     */
    public LispTokenizer(Reader r)
    {
        if(r == null)
            r = new StringReader("");
        BufferedReader buffrdr = new BufferedReader(r);
        m_tokenizer = new StreamTokenizer(buffrdr);
        m_tokenizer.resetSyntax(); // We don't like the default settings

        m_tokenizer.whitespaceChars(0, ' ');
        m_tokenizer.wordChars(' '+1,255);
        m_tokenizer.ordinaryChar('(');
        m_tokenizer.ordinaryChar(')');
        m_tokenizer.ordinaryChar('\'');
        m_tokenizer.commentChar(';');
        m_tokenizer.quoteChar('"');
    }

    public Token peekToken()
    {
        if(m_ioexn != null)
            return null;
        try
        {
            m_tokenizer.nextToken();
        }
        catch(IOException e)
        {
            m_ioexn = e;
            return null;
        }
        if(m_tokenizer.ttype == StreamTokenizer.TT_EOF)
            return null;
        Token token = new Token(m_tokenizer);
        m_tokenizer.pushBack();
        return token;
    }

    public boolean hasNext()
    {
        if(m_ioexn != null)
            return false;
        try
        {
            m_tokenizer.nextToken();
        }
        catch(IOException e)
        {
            m_ioexn = e;
            return false;
        }
        if(m_tokenizer.ttype == StreamTokenizer.TT_EOF)
            return false;
        m_tokenizer.pushBack();
        return true;
    }

    /** Return the most recently caught IOException, if any,
     *
     * @return
     */
    public IOException getIOException()
    {
        return m_ioexn;
    }

    public Token next()
    {
        try
        {
            m_tokenizer.nextToken();
        }
        catch(IOException e)
        {
            m_ioexn = e;
            return null;
        }

        Token token = new Token(m_tokenizer);
        return token;
    }

    public void remove()
    {
    }
}
```



### =Token.java=


```java
package jfkbits;
import java.io.StreamTokenizer;

public class Token
{
    public static final int SYMBOL = StreamTokenizer.TT_WORD;
    public int type;
    public String text;
    public int line;

    public Token(StreamTokenizer tzr)
    {
        this.type = tzr.ttype;
        this.text = tzr.sval;
        this.line = tzr.lineno();
    }

    public String toString()
    {
        switch(this.type)
        {
            case SYMBOL:
            case '"':
                return this.text;
            default:
                return String.valueOf((char)this.type);
        }
    }
}
```



### =Atom.java=


```java
package jfkbits;

import jfkbits.LispParser.Expr;

public class Atom implements Expr
{
    String name;
    public String toString()
    {
        return name;
    }
    public Atom(String text)
    {
        name = text;
    }

}
```



### =StringAtom.java=


```java
package jfkbits;

public class StringAtom extends Atom
{
    public String toString()
    {
        // StreamTokenizer hardcodes escaping with \, and doesn't allow \n inside words
        String escaped = name.replace("\\", "\\\\").replace("\n", "\\n").replace("\r", "\\r").replace("\"", "\\\"");
        return "\""+escaped+"\"";
    }

    public StringAtom(String text)
    {
        super(text);
    }
    public String getValue()
    {
        return name;
    }
}

```



### =ExprList.java=


```java
package jfkbits;

import java.util.AbstractCollection;
import java.util.Arrays;
import java.util.Iterator;
import java.util.ArrayList;

import jfkbits.LispParser.Expr;

public class ExprList extends ArrayList<Expr> implements Expr
{
    ExprList parent = null;
    int indent =1;

    public int getIndent()
    {
        if (parent != null)
        {
            return parent.getIndent()+indent;
        }
        else return 0;
    }

    public void setIndent(int indent)
    {
        this.indent = indent;
    }



    public void setParent(ExprList parent)
    {
        this.parent = parent;
    }

    public String toString()
    {
        String indent = "";
        if (parent != null && parent.get(0) != this)
        {
            indent = "\n";
            char[] chars = new char[getIndent()];
            Arrays.fill(chars, ' ');
            indent += new String(chars);
        }

        String output = indent+"(";
        for(Iterator<Expr> it=this.iterator(); it.hasNext(); )
        {
            Expr expr = it.next();
            output += expr.toString();
            if (it.hasNext())
                output += " ";
        }
        output += ")";
        return output;
    }

    @Override
    public synchronized boolean add(Expr e)
    {
        if (e instanceof ExprList)
        {
            ((ExprList) e).setParent(this);
            if (size() != 0 && get(0) instanceof Atom)
                ((ExprList) e).setIndent(2);
        }
        return super.add(e);
    }

}
```



### =LispParser.java=


```java
package jfkbits;


public class LispParser
{
    LispTokenizer tokenizer;

    public LispParser(LispTokenizer input)
    {
        tokenizer=input;
    }

    public class ParseException extends Exception
    {

    }

    public interface Expr
    {
        // abstract parent for Atom and ExprList
    }

    public Expr parseExpr() throws ParseException
    {
        Token token = tokenizer.next();
        switch(token.type)
        {
            case '(': return parseExprList(token);
            case '"': return new StringAtom(token.text);
            default: return new Atom(token.text);
        }
    }


    protected ExprList parseExprList(Token openParen) throws ParseException
    {
        ExprList acc = new ExprList();
        while(tokenizer.peekToken().type != ')')
        {
            Expr element = parseExpr();
            acc.add(element);
        }
        Token closeParen = tokenizer.next();
        return acc;
    }

}

```



### =LispParserDemo.java=


```java
import jfkbits.ExprList;
import jfkbits.LispParser;
import jfkbits.LispParser.ParseException;
import jfkbits.LispTokenizer;

public class LispParserDemo
{
    public static void main(String args[])
    {

        LispTokenizer tzr = new LispTokenizer(
            "((data \"quoted data\" 123 4.5)\n (data (!@# (4.5) \"(more\" \"data)\")))");
        LispParser parser = new LispParser(tzr);

        try
        {
            Expr result = parser.parseExpr();
            System.out.println(result);
        }
        catch (ParseException e1)
        {
            // TODO Auto-generated catch block
            e1.printStackTrace();
        }
    }
}
```



## JavaScript


```JavaScript
String.prototype.parseSexpr = function() {
	var t = this.match(/\s*("[^"]*"|\(|\)|"|[^\s()"]+)/g)
	for (var o, c=0, i=t.length-1; i>=0; i--) {
		var n, ti = t[i].trim()
		if (ti == '"') return
		else if (ti == '(') t[i]='[', c+=1
		else if (ti == ')') t[i]=']', c-=1
		else if ((n=+ti) == ti) t[i]=n
		else t[i] = '\'' + ti.replace('\'', '\\\'') + '\''
		if (i>0 && ti!=']' && t[i-1].trim()!='(' ) t.splice(i,0, ',')
		if (!c) if (!o) o=true; else return
	}
	return c ? undefined : eval(t.join(''))
}

Array.prototype.toString = function() {
	var s=''; for (var i=0, e=this.length; i<e; i++) s+=(s?' ':'')+this[i]
	return '('+s+')'
}

Array.prototype.toPretty = function(s) {
	if (!s) s = ''
	var r = s + '(
'
	var s2 = s + Array(6).join(' ')
	for (var i=0, e=this.length; i<e; i+=1) {
		var ai = this[i]
		r += ai.constructor != Array ? s2+ai+'
' : ai.toPretty(s2)
	}
	return r + s + ')
'
}

var str = '((data "quoted data" 123 4.5)\n (data (!@# (4.5) "(more" "data)")))'
document.write('text:
', str.replace(/\n/g,'
').replace(/ /g,' '), '

')
var sexpr = str.parseSexpr()
if (sexpr === undefined)
	document.write('Invalid s-expr!', '
')
else
	document.write('s-expr:
', sexpr, '

', sexpr.constructor != Array ? '' : 'pretty print:
' + sexpr.toPretty())
```

{{output}}

```txt

text:
((data "quoted data" 123 4.5)
 (data (!@# (4.5) "(more" "data)")))

s-expr:
((data "quoted data" 123 4.5) (data (!@# (4.5) "(more" "data)")))

pretty print:
(
      (
            data
            "quoted data"
            123
            4.5
      )
      (
            data
            (
                  !@#
                  (
                        4.5
                  )
                  "(more"
                  "data)"
            )
      )
)

```



## Julia


```julia

function rewritequotedparen(s)
    segments = split(s, "\"")
    for i in 1:length(segments)
        if i & 1 == 0  # even i
            ret = replace(segments[i], r"\(", s"_O_PAREN")
            segments[i] = replace(ret, r"\)", s"_C_PAREN")
        end
    end
    join(segments, "\"")
end

function reconsdata(n, s)
    if n > 1
        print(" ")
    end
    if s isa String && ismatch(r"[\$\%\!\$\#]", s) == false
        print("\"$s\"")
    else
        print(s)
    end
end

function printAny(anyarr)
    print("(")
    for (i, el) in enumerate(anyarr)
        if el isa Array
            print("(")
            for (j, el2) in enumerate(el)
                if el2 isa Array
                    print("(")
                    for(k, el3) in enumerate(el2)
                        if el3 isa Array
                            print(" (")
                            for(n, el4) in enumerate(el3)
                                reconsdata(n, el4)
                            end
                            print(")")
                        else
                            reconsdata(k, el3)
                        end
                    end
                    print(")")
                else
                    reconsdata(j, el2)
                end
            end
            if i == 1
                print(")\n ")
            else
                print(")")
            end
        end
    end
    println(")")
end

removewhitespace(s) = replace(replace(s, r"\n", " "), r"^\s*(\S.*\S)\s*$", s"\1")
quote3op(s) = replace(s, r"([\$\!\@\#\%]{3})", s"\"\1\"")
paren2bracket(s) = replace(replace(s, r"\(", s"["), r"\)", s"]")
data2symbol(s) = replace(s, "[data", "[:data")
unrewriteparens(s) = replace(replace(s, "_C_PAREN", ")"), "_O_PAREN", "(")
addcommas(s) = replace(replace(s, r"\]\s*\[", "],["), r" (?![a-z])", ",")

inputstring = """
((data "quoted data" 123 4.5)
 (data (!@# (4.5) "(more" "data)")))
 """

println("The input string is:\n", inputstring)
processed = (inputstring |> removewhitespace |> rewritequotedparen |> quote3op
                        |> paren2bracket |> data2symbol |> unrewriteparens |> addcommas)
nat = eval(parse("""$processed"""))
println("The processed native structure is:\n", nat)
println("The reconstructed string is:\n"), printAny(nat)

```

{{output}}
```txt

The input string is:
    ((data "quoted data" 123 4.5)
     (data (!@# (4.5) "(more" "data)")))

The processed native structure is:
Array{Any,1}[Any[:data, "quoted data", 123, 4.5], Any[:data, Any["!@#", [4.5], "(more", "data)"]]]

The reconstructed string is:
    ((data "quoted data" 123 4.5)
     (data(!@# (4.5) "(more" "data)")))

```



## Kotlin

{{trans|JavaScript}}

```groovy
// version 1.2.31

const val INDENT = 2

fun String.parseSExpr(): List<String>? {
    val r = Regex("""\s*("[^"]*"|\(|\)|"|[^\s()"]+)""")
    val t = r.findAll(this).map { it.value }.toMutableList()
    if (t.size == 0) return null
    var o = false
    var c = 0
    for (i in t.size - 1 downTo 0) {
        val ti = t[i].trim()
        val nd = ti.toDoubleOrNull()
        if (ti == "\"") return null
        if (ti == "(") {
            t[i] = "["
            c++
        }
        else if (ti == ")") {
            t[i] = "]"
            c--
        }
        else if (nd != null) {
             val ni = ti.toIntOrNull()
             if (ni != null) t[i] = ni.toString()
             else t[i] = nd.toString()
        }
        else if (ti.startsWith("\"")) { // escape embedded double quotes
             var temp = ti.drop(1).dropLast(1)
             t[i] = "\"" + temp.replace("\"", "\\\"") + "\""
        }
        if (i > 0 && t[i] != "]" && t[i - 1].trim() != "(") t.add(i, ", ")
        if (c == 0) {
            if (!o) o = true else return null
        }
    }
    return if (c != 0) null else t
}

fun MutableList<String>.toSExpr(): String {
    for (i in 0 until this.size) {
        this[i] = when (this[i]) {
            "["  -> "("
            "]"  -> ")"
            ", " -> " "
            else ->  {
                if (this[i].startsWith("\"")) { // unescape embedded quotes
                    var temp = this[i].drop(1).dropLast(1)
                    "\"" + temp.replace("\\\"", "\"") + "\""
                }
                else this[i]
            }
        }
    }
    return this.joinToString("")
}

fun List<String>.prettyPrint() {
    var level = 0
    loop@for (t in this) {
        var n: Int
        when(t) {
            ", ", " " -> continue@loop
            "[", "(" -> {
                n = level * INDENT + 1
                level++
             }
             "]", ")" -> {
                level--
                n = level * INDENT + 1
             }
             else -> {
                n = level * INDENT + t.length
             }
        }
        println("%${n}s".format(t))
    }
}

fun main(args: Array<String>) {
    val str = """((data "quoted data" 123 4.5)""" + "\n" +
              """ (data (!@# (4.5) "(more" "data)")))"""
    val tokens = str.parseSExpr()
    if (tokens == null)
        println("Invalid s-expr!")
    else {
        println("Native data structure:")
        println(tokens.joinToString(""))
        println("\nNative data structure (pretty print):")
        tokens.prettyPrint()

        val tokens2 = tokens.toMutableList()
        println("\nRecovered S-Expression:")
        println(tokens2.toSExpr())
        println("\nRecovered S-Expression (pretty print):")
        tokens2.prettyPrint()
    }
}
```


{{out}}

```txt

Native data structure:
[[data, "quoted data", 123, 4.5], [data, [!@#, [4.5], "(more", "data)"]]]

Native data structure (pretty print):
[
  [
    data
    "quoted data"
    123
    4.5
  ]
  [
    data
    [
      !@#
      [
        4.5
      ]
      "(more"
      "data)"
    ]
  ]
]

Recovered S-Expression:
((data "quoted data" 123 4.5) (data (!@# (4.5) "(more" "data)")))

Recovered S-Expression (pretty print):
(
  (
    data
    "quoted data"
    123
    4.5
  )
  (
    data
    (
      !@#
      (
        4.5
      )
      "(more"
      "data)"
    )
  )
)

```



## Nim



```nim
import strutils

const Input = """
((data "quoted data" 123 4.5)
 (data (!@# (4.5) "(more" "data)")))
"""

type
  TokenKind = enum
    tokInt, tokFloat, tokString, tokIdent
    tokLPar, tokRPar
    tokEnd
  Token = object
    case kind: TokenKind
    of tokString: stringVal: string
    of tokInt: intVal: int
    of tokFloat: floatVal: float
    of tokIdent: ident: string
    else: discard

proc lex(input: string): seq[Token] =
  var pos = 0

  template current: char =
    if pos < input.len: input[pos]
    else: '\x00'

  while pos < input.len:
    case current
    of ';':
      inc(pos)
      while current notin {'\r', '\n'}:
        inc(pos)
      if current == '\r': inc(pos)
      if current == '\n': inc(pos)
    of '(': inc(pos); result.add(Token(kind: tokLPar))
    of ')': inc(pos); result.add(Token(kind: tokRPar))
    of '0'..'9':
      var
        num = ""
        isFloat = false
      while current in Digits:
        num.add(current)
        inc(pos)
      if current == '.':
        num.add(current)
        isFloat = true
        inc(pos)
        while current in Digits:
          num.add(current)
          inc(pos)
      result.add(if isFloat: Token(kind: tokFloat, floatVal: parseFloat(num))
                 else: Token(kind: tokInt, intVal: parseInt(num)))
    of ' ', '\t', '\n', '\r': inc(pos)
    of '"':
      var str = ""
      inc(pos)
      while current != '"':
        str.add(current)
        inc(pos)
      inc(pos)
      result.add(Token(kind: tokString, stringVal: str))
    else:
      const BannedChars = {' ', '\t', '"', '(', ')', ';'}
      var ident = ""
      while current notin BannedChars:
        ident.add(current)
        inc(pos)
      result.add(Token(kind: tokIdent, ident: ident))
  result.add(Token(kind: tokEnd))

type
  SExprKind = enum
    seInt, seFloat, seString, seIdent, seList
  SExpr = ref object
    case kind: SExprKind
    of seInt: intVal: int
    of seFloat: floatVal: float
    of seString: stringVal: string
    of seIdent: ident: string
    of seList: children: seq[SExpr]
  ParseError = object of CatchableError

proc `$`*(se: SExpr): string =
  case se.kind
  of seInt: result = $se.intVal
  of seFloat: result = $se.floatVal
  of seString: result = '"' & se.stringVal & '"'
  of seIdent: result = se.ident
  of seList:
    result = "("
    for i, ex in se.children:
      if ex.kind == seList and ex.children.len > 1:
        result.add("\n")
        result.add(indent($ex, 2))
      else:
        if i > 0:
          result.add(" ")
        result.add($ex)
    result.add(")")

var
  tokens = lex(Input)
  pos = 0

template current: Token =
  if pos < tokens.len: tokens[pos]
  else: Token(kind: tokEnd)

proc parseInt(token: Token): SExpr =
  result = SExpr(kind: seInt, intVal: token.intVal)

proc parseFloat(token: Token): SExpr =
  result = SExpr(kind: seFloat, floatVal: token.floatVal)

proc parseString(token: Token): SExpr =
  result = SExpr(kind: seString, stringVal: token.stringVal)

proc parseIdent(token: Token): SExpr =
  result = SExpr(kind: seIdent, ident: token.ident)

proc parse(): SExpr

proc parseList(): SExpr =
  result = SExpr(kind: seList)
  while current.kind notin {tokRPar, tokEnd}:
    result.children.add(parse())
  if current.kind == tokEnd:
    raise newException(ParseError, "Missing right paren ')'")
  else:
    inc(pos)

proc parse(): SExpr =
  var token = current
  inc(pos)
  result =
    case token.kind
    of tokInt: parseInt(token)
    of tokFloat: parseFloat(token)
    of tokString: parseString(token)
    of tokIdent: parseIdent(token)
    of tokLPar: parseList()
    else: nil

echo parse()
```


{{out}}


```txt

(
  (data "quoted data" 123 4.5)
  (data
    (!@# (4.5) "(more" "data)")))

```



## OCaml


You may be interested by [https://realworldocaml.org/v1/en/html/data-serialization-with-s-expressions.html this chapter of the book Real World Ocaml]

The file <code>SExpr.mli</code> containing the interface:


```ocaml
(** This module is a very simple parsing library for S-expressions. *)
(* Copyright (C) 2009  Florent Monnier, released under MIT license. *)

type sexpr = Atom of string | Expr of sexpr list
(** the type of S-expressions *)

val parse_string : string -> sexpr list
(** parse from a string *)

val parse_ic : in_channel -> sexpr list
(** parse from an input channel *)

val parse_file : string -> sexpr list
(** parse from a file *)

val parse : (unit -> char option) -> sexpr list
(** parse from a custom function, [None] indicates the end of the flux *)

val print_sexpr : sexpr list -> unit
(** a dump function for the type [sexpr] *)

val print_sexpr_indent : sexpr list -> unit
(** same than [print_sexpr] but with indentation *)

val string_of_sexpr : sexpr list -> string
(** convert an expression of type [sexpr] into a string *)

val string_of_sexpr_indent : sexpr list -> string
(** same than [string_of_sexpr] but with indentation *)
```


The file <code>SExpr.ml</code> containing the implementation:


```ocaml
(** This module is a very simple parsing library for S-expressions. *)
(* Copyright (C) 2009  Florent Monnier, released under MIT license. *)
(* modified to match the task description *)

type sexpr = Atom of string | Expr of sexpr list

type state =
  | Parse_root of sexpr list
  | Parse_content of sexpr list
  | Parse_word of Buffer.t * sexpr list
  | Parse_string of bool * Buffer.t * sexpr list

let parse pop_char =
  let rec aux st =
    match pop_char() with
    | None ->
        begin match st with
        | Parse_root sl -> (List.rev sl)
        | Parse_content _
        | Parse_word _
        | Parse_string _ ->
            failwith "Parsing error: content not closed by parenthesis"
        end
    | Some c ->
        match c with
        | '(' ->
            begin match st with
            | Parse_root sl ->
                let this = aux(Parse_content []) in
                aux(Parse_root((Expr this)::sl))
            | Parse_content sl ->
                let this = aux(Parse_content []) in
                aux(Parse_content((Expr this)::sl))
            | Parse_word(w, sl) ->
                let this = aux(Parse_content []) in
                aux(Parse_content((Expr this)::Atom(Buffer.contents w)::sl))
            | Parse_string(_, s, sl) ->
                Buffer.add_char s c;
                aux(Parse_string(false, s, sl))
            end
        | ')' ->
            begin match st with
            | Parse_root sl ->
                failwith "Parsing error: closing parenthesis without openning"
            | Parse_content sl -> (List.rev sl)
            | Parse_word(w, sl) -> List.rev(Atom(Buffer.contents w)::sl)
            | Parse_string(_, s, sl) ->
                Buffer.add_char s c;
                aux(Parse_string(false, s, sl))
            end
        | ' ' | '\n' | '\r' | '\t' ->
            begin match st with
            | Parse_root sl -> aux(Parse_root sl)
            | Parse_content sl -> aux(Parse_content sl)
            | Parse_word(w, sl) -> aux(Parse_content(Atom(Buffer.contents w)::sl))
            | Parse_string(_, s, sl) ->
                Buffer.add_char s c;
                aux(Parse_string(false, s, sl))
            end
        | '"' ->
            (* '"' *)
            begin match st with
            | Parse_root _ -> failwith "Parse error: double quote at root level"
            | Parse_content sl ->
                let s = Buffer.create 74 in
                aux(Parse_string(false, s, sl))
            | Parse_word(w, sl) ->
                let s = Buffer.create 74 in
                aux(Parse_string(false, s, Atom(Buffer.contents w)::sl))
            | Parse_string(true, s, sl) ->
                Buffer.add_char s c;
                aux(Parse_string(false, s, sl))
            | Parse_string(false, s, sl) ->
                aux(Parse_content(Atom(Buffer.contents s)::sl))
            end
        | '\\' ->
            begin match st with
            | Parse_string(true, s, sl) ->
                Buffer.add_char s c;
                aux(Parse_string(false, s, sl))
            | Parse_string(false, s, sl) ->
                aux(Parse_string(true, s, sl))
            | _ ->
                failwith "Parsing error: escape character in wrong place"
            end
        | _ ->
            begin match st with
            | Parse_root _ ->
                failwith(Printf.sprintf "Parsing error: char '%c' at root level" c)
            | Parse_content sl ->
                let w = Buffer.create 16 in
                Buffer.add_char w c;
                aux(Parse_word(w, sl))
            | Parse_word(w, sl) ->
                Buffer.add_char w c;
                aux(Parse_word(w, sl))
            | Parse_string(_, s, sl) ->
                Buffer.add_char s c;
                aux(Parse_string(false, s, sl))
            end
  in
  aux (Parse_root [])


let string_pop_char str =
  let len = String.length str in
  let i = ref(-1) in
  (function () -> incr i; if !i >= len then None else Some(str.[!i]))


let parse_string str =
  parse (string_pop_char str)


let ic_pop_char ic =
  (function () ->
     try Some(input_char ic)
     with End_of_file -> (None))


let parse_ic ic =
  parse (ic_pop_char ic)


let parse_file filename =
  let ic = open_in filename in
  let res = parse_ic ic in
  close_in ic;
  (res)


let quote s =
  "\"" ^ s ^ "\""

let needs_quote s =
  List.exists (String.contains s) [' '; '\n'; '\r'; '\t'; '('; ')']

let protect s =
  let s = String.escaped s in
  if needs_quote s then quote s else s


let string_of_sexpr s =
  let rec aux acc = function
  | (Atom tag)::tl -> aux ((protect tag)::acc) tl
  | (Expr e)::tl ->
      let s =
        "(" ^
        (String.concat " " (aux [] e))
        ^ ")"
      in
      aux (s::acc) tl
  | [] -> (List.rev acc)
  in
  String.concat " " (aux [] s)


let print_sexpr s =
  print_endline (string_of_sexpr s)


let string_of_sexpr_indent s =
  let rec aux i acc = function
  | (Atom tag)::tl -> aux i ((protect tag)::acc) tl
  | (Expr e)::tl ->
      let s =
        "\n" ^ (String.make i ' ') ^ "(" ^
        (String.concat " " (aux (succ i) [] e))
        ^ ")"
      in
      aux i (s::acc) tl
  | [] -> (List.rev acc)
  in
  String.concat "\n" (aux 0 [] s)


let print_sexpr_indent s =
  print_endline (string_of_sexpr_indent s)
```


Then we compile this small module and test it in the interactive loop:


```txt
$ ocamlc -c SExpr.mli
$ ocamlc -c SExpr.ml
$ ocaml SExpr.cmo
        Objective Caml version 3.11.2

# open SExpr ;;

# let s = read_line () ;;
((data "quoted data" 123 4.5) (data (!@# (4.5) "(more" "data)")))
val s : string =
  "((data \"quoted data\" 123 4.5) (data (!@# (4.5) \"(more\" \"data)\")))"

# let se = SExpr.parse_string s ;;
val se : SExpr.sexpr list =
  [Expr
    [Expr [Atom "data"; Atom "quoted data"; Atom "123"; Atom "4.5"];
     Expr
      [Atom "data";
       Expr [Atom "!@#"; Expr [Atom "4.5"]; Atom "(more"; Atom "data)"]]]]

# SExpr.print_sexpr se ;;
((data "quoted data" 123 4.5) (data (!@# (4.5) "(more" "data)")))
- : unit = ()

# SExpr.print_sexpr_indent se ;;

(
 (data "quoted data" 123 4.5)
 (data
  (!@#
   (4.5) "(more" "data)")))
- : unit = ()
```



## Perl


```perl
#!/usr/bin/perl -w
use strict;
use warnings;

sub sexpr
{
	my @stack = ([]);
	local $_ = $_[0];

	while (m{
		\G    # start match right at the end of the previous one
		\s*+  # skip whitespaces
		# now try to match any of possible tokens in THIS order:
		(?<lparen>\() |
		(?<rparen>\)) |
		(?<FLOAT>[0-9]*+\.[0-9]*+) |
		(?<INT>[0-9]++) |
		(?:"(?<STRING>([^\"\\]|\\.)*+)") |
		(?<IDENTIFIER>[^\s()]++)
		# Flags:
		#  g = match the same string repeatedly
		#  m = ^ and $ match at \n
		#  s = dot and \s matches \n
		#  x = allow comments within regex
		}gmsx)
	{
		die "match error" if 0+(keys %+) != 1;

		my $token = (keys %+)[0];
		my $val = $+{$token};

		if ($token eq 'lparen') {
			my $a = [];
			push @{$stack[$#stack]}, $a;
			push @stack, $a;
		} elsif ($token eq 'rparen') {
			pop @stack;
		} else {
			push @{$stack[$#stack]}, bless \$val, $token;
		}
	}
	return $stack[0]->[0];
}

sub quote
{ (local $_ = $_[0]) =~ /[\s\"\(\)]/s ? do{s/\"/\\\"/gs; qq{"$_"}} : $_; }

sub sexpr2txt
{
	qq{(@{[ map {
		ref($_) eq '' ? quote($_) :
		ref($_) eq 'STRING' ? quote($$_) :
		ref($_) eq 'ARRAY' ? sexpr2txt($_) : $$_
	} @{$_[0]} ]})}
}
```

Check:

```perl
my $s = sexpr(q{

((data "quoted data" 123 4.5)
 (data (!@# (4.5) "(more" "data)")))

});

# Dump structure
use Data::Dumper;
print Dumper $s;

# Convert back
print sexpr2txt($s)."\n";
```

Output:

```txt
$VAR1 = [
          [
            bless( do{\(my $o = 'data')}, 'IDENTIFIER' ),
            bless( do{\(my $o = 'quoted data')}, 'STRING' ),
            bless( do{\(my $o = '123')}, 'INT' ),
            bless( do{\(my $o = '4.5')}, 'FLOAT' )
          ],
          [
            bless( do{\(my $o = 'data')}, 'IDENTIFIER' ),
            [
              bless( do{\(my $o = '!@#')}, 'IDENTIFIER' ),
              [
                bless( do{\(my $o = '4.5')}, 'FLOAT' )
              ],
              bless( do{\(my $o = '(more')}, 'STRING' ),
              bless( do{\(my $o = 'data)')}, 'STRING' )
            ]
          ]
        ];
((data "quoted data" 123 4.5) (data (!@# (4.5) "(more" "data)")))
```



## Perl 6

{{works with|Rakudo|version 2014.04-250-gea173d0 built on MoarVM version 2014.04-98-gbed1693
}}

This parses the task, but it isn't really a good lisp parser, because it always wants whitespace between lists, so <code>(()())</code> will fail ( <code>(() ())</code> wont)


```perl6
grammar S-Exp {
  rule TOP    {^ <s-list> $};

  token s-list { '(' ~ ')' [ <in_list>+ % [\s+] | '' ] }
  token in_list { <s-token> | <s-list> }

  proto token s-token {*}
  token s-token:sym<Num>    {\d*\.?\d+}
  token s-token:sym<String> {'"' ['\"' |<-[\\"]>]*? '"'} #'
  token s-token:sym<Atom>   {<-[()\s]>+}

}

# The Actions class, for each syntactic rule there is a method
# that stores some data in the abstract syntax tree with make
class S-Exp::ACTIONS {
  method TOP ($/) {make $<s-list>.ast}
  method s-list ($/) {make [$<in_list>».ast]}
  method in_list ($/) {make $/.values[0].ast}

  method s-token:sym<Num> ($/){make +$/}
  method s-token:sym<String> ($/){make ~$/.substr(1,*-1)}
  method s-token:sym<Atom> ($/){make ~$/}
}

multi s-exp_writer (Positional $ary) {'(' ~ $ary.map(&s-exp_writer).join(' ') ~ ')'}
multi s-exp_writer (Numeric    $num) {~$num}
multi s-exp_writer (Str        $str) {
  return $str unless $str ~~ /<[(")]>|\s/;
  return '()' if $str eq '()';
  '"' ~ $str.subst('"', '\"' ) ~ '"';
}


my $s-exp = '((data "quoted data" 123 4.5)
 (data (!@# (4.5) "(more" "data)")))';

my $actions = S-Exp::ACTIONS.new();
my $perl_array = (S-Exp.parse($s-exp, :$actions)).ast;

say "the expression:\n$s-exp\n";
say "the perl6-expression:\n{$perl_array.perl}\n";
say "and back:\n{s-exp_writer($perl_array)}";
```


{{out}}

```txt
the expression:
((data "quoted data" 123 4.5)
 (data (!@# (4.5) "(more" "data)")))

the perl6-expression:
[["data", "quoted data", "123", 9/2], ["data", ["!\@#", [9/2], "(more", "data)"]]]

and back:
((data "quoted data" 123 4.5) (data (!@# (4.5) "(more" "data)")))
```



## Phix

The distinction between a symbol data and a quoted string "data" is simple: both are represented as strings, with the symbol
being held as "data" and the quoted string being held as "\"data\"", and you can test for the latter by seeing if the first
character is a double quote. Internally, it is easy to differentiate between a symbol (held as a string) and a number, but
that may not be clear on the display: 4e-5 and 4-e5 may appear similar but the latter is probably a parse failure. It may
be more sensible for get_term() to raise an error if the scanf fails, than assume it is a symbol like it does now.
Also, I added pp_StrFmt -3 (a combination of existing -1 and -2 behaviour) specifically for this task.

```Phix
constant s_expr_str = """
((data "quoted data" 123 4.5)
 (data (!@# (4.5) "(more" "data)")))"""

function skip_spaces(string s, integer sidx)
    while sidx<=length(s) and find(s[sidx]," \t\r\n") do sidx += 1 end while
    return sidx
end function

function get_term(string s, integer sidx)
-- get a single quoted string, symbol, or number.
integer ch = s[sidx]
string res = ""
    if ch='\"' then
        res &= ch
        while 1 do
            sidx += 1
            ch = s[sidx]
            res &= ch
            if ch='\\' then
                sidx += 1
                ch = s[sidx]
                res &= ch
            elsif ch='\"' then
                sidx += 1
                exit
            end if
        end while
    else
        integer asnumber = (ch>='0' and ch<='9')
        while not find(ch,") \t\r\n") do
            res &= ch
            sidx += 1
            if sidx>length(s) then exit end if
            ch = s[sidx]
        end while
        if asnumber then
            sequence scanres = scanf(res,"%f")
            if length(scanres)=1 then return {scanres[1][1],sidx} end if
            -- error? (failed to parse number)
        end if
    end if
    return {res,sidx}
end function

function parse_s_expr(string s, integer sidx)
integer ch = s[sidx]
sequence res = {}
object element
    if ch!='(' then ?9/0 end if
    sidx += 1
    while 1 do
        sidx = skip_spaces(s,sidx)
        -- error? (if past end of string/missing ')')
        ch = s[sidx]
        if ch=')' then exit end if
        if ch='(' then
            {element,sidx} = parse_s_expr(s,sidx)
        else
            {element,sidx} = get_term(s,sidx)
        end if
        res = append(res,element)
    end while
    sidx = skip_spaces(s,sidx+1)
    return {res,sidx}
end function

sequence s_expr
integer sidx
{s_expr,sidx} = parse_s_expr(s_expr_str,1)
if sidx<=length(s_expr_str) then
    printf(1,"incomplete parse(\"%s\")\n",{s_expr_str[sidx..$]})
end if

puts(1,"\nThe string:\n")
?s_expr_str

puts(1,"\nDefault pretty printing:\n")
--?s_expr
pp(s_expr)

puts(1,"\nBespoke pretty printing:\n")
--ppEx(s_expr,{pp_Nest,1,pp_StrFmt,-3,pp_Brkt,"()"})
ppEx(s_expr,{pp_Nest,4,pp_StrFmt,-3,pp_Brkt,"()"})
```

{{out}}

```txt

The string:
"((data \"quoted data\" 123 4.5)\n (data (!@# (4.5) \"(more\" \"data)\")))"

Default pretty printing:
{{"data", "\"quoted data\"", 123'{',4.5},
 {"data", {"!@#", {4.5}, "\"(more\"", "\"data)\""}}}

Bespoke pretty printing:
((data,
  "quoted data",
  123,
  4.5),
 (data,
  (!@#,
   (4.5),
   "(more",
   "data)")))

```



## PicoLisp

The '[http://software-lab.de/doc/refA.html#any any]' function parses an s-expression from a string (indentical to the way '[http://software-lab.de/doc/refR.html#read read]' does this from an input stream).

```PicoLisp
: (any "((data \"quoted data\" 123 4.5) (data (!@# (4.5) \"(more\" \"data)\")))")
-> ((data "quoted data" 123 5) (data (!@# (5) "(more" "data)")))

: (view @)
+---+-- data
|   |
|   +-- "quoted data"
|   |
|   +-- 123
|   |
|   +-- 5
|
+---+-- data
    |
    +---+-- !@#
        |
        +---+-- 5
        |
        +-- "(more"
        |
        +-- "data)"
```

Implementing a subset of 'any' explicitly:

```PicoLisp
(de readSexpr ()
   (case (skip)
      ("(" (char) (readList))
      ("\"" (char) (readString))
      (T (readAtom)) ) ) )

(de readList ()
   (make
      (loop
         (NIL (skip))
         (T (= @ ")") (char))
         (link (readSexpr)) ) ) )

(de readString ()
   (pack
      (make
         (until (= "\"" (or (peek) (quit "Unterminated string")))
            (link (char)) )
         (char) ) ) )

(de readAtom ()
   (let X
      (make
         (until (or (sp? (peek)) (member (peek) '("(" ")")))
            (link (char)) ) )
      (or (format X) (intern (pack X))) ) )
```

It can be used in a pipe to read from a string:

```PicoLisp
: (pipe (prin "((data \"quoted data\" 123 4.5) (data (!@# (4.5) \"(more\" \"data)\")))") (readSexpr))
-> ((data "quoted data" 123 5) (data (!@# (5) "(more" "data)")))
```

'[http://software-lab.de/doc/refS.html#sym sym]' does the reverse (i.e. builds a symbol (string) from an expression).

```PicoLisp
: (sym @@)
-> "((data \"quoted data\" 123 5) (data (!@# (5) \"(more\" \"data)\")))"
```

Implementing a subset of the built-in printer:

```PicoLisp
(de printSexpr (Expr Fun)
   (cond
      ((pair Expr)
         (Fun "(")
         (printSexpr (car Expr) Fun)
         (for X (cdr Expr)
            (Fun " ")
            (printSexpr X Fun) )
         (Fun ")") )
      ((str? Expr)
         (Fun "\"")
         (mapc Fun (chop Expr))
         (Fun "\"") )
      (T (mapc Fun (chop Expr))) ) )
```

This can be used for plain printing

```PicoLisp
: (printSexpr
   '((data "quoted data" 123 4.5) (data (!@# (4.5) "(more" "data)")))
   prin )
((data "quoted data" 123 5) (data (!@# (5) "(more" "data)")))
```

or to collect the characters into a string:

```PicoLisp
: (pack
   (make
      (printSexpr
         '((data "quoted data" 123 4.5) (data (!@# (4.5) "(more" "data)")))
         link ) ) )
-> "((data \"quoted data\" 123 5) (data (!@# (5) \"(more\" \"data)\")))"
```



## Pike


```pike
class Symbol(string name)
{
    string _sprintf(int type)
    {
        switch(type)
        {
            case 's': return name;
            case 'O': return sprintf("(Symbol: %s)", name||"");
            case 'q': return name;
            case 't': return "Symbol";
            default:  return sprintf("%"+int2char(type), name);
        }
    }

    mixed cast(string type)
    {
        switch(type)
        {
            case "string": return name;
            default: throw(sprintf("can not cast 'Symbol' to '%s'", type));
        }
    }
}

mixed value(string token)
{
    if ((string)(int)token==token)
        return (int)token;
    array result = array_sscanf(token, "%f%s");
    if (sizeof(result) && floatp(result[0]) && ! sizeof(result[1]))
        return result[0];
    else
        return Symbol(token);
}

array tokenizer(string input)
{
    array output = ({});
    for(int i=0; i<sizeof(input); i++)
    {
        switch(input[i])
        {
            case '(': output+= ({"("}); break;
            case ')': output += ({")"}); break;
            case '"': //"
                      output+=array_sscanf(input[++i..], "%s\"%[ \t\n]")[0..0];
                      i+=sizeof(output[-1]);
                      break;
            case ' ':
            case '\t':
            case '\n': break;
            default: string token = array_sscanf(input[i..], "%s%[) \t\n]")[0];
                     output+=({ value(token) });
                     i+=sizeof(token)-1;
                     break;
        }
    }
    return output;
}

// this function is based on the logic in Parser.C.group() in the pike library;
array group(array tokens)
{
    ADT.Stack stack=ADT.Stack();
    array ret =({});

    foreach(tokens;; string token)
    {
        switch(token)
        {
            case "(": stack->push(ret); ret=({}); break;
            case ")":
                    if (!sizeof(ret) || !stack->ptr)
                    {
                      // Mismatch
                        werror ("unmatched close parenthesis\n");
                        return ret;
                    }
                    ret=stack->pop()+({ ret });
                    break;
            default: ret+=({token}); break;
        }
    }
    return ret;
}

string sexp(array input)
{
    array output = ({});
    foreach(input;; mixed item)
    {
        if (arrayp(item))
            output += ({ sexp(item) });
        else if (intp(item))
            output += ({ sprintf("%d", item) });
        else if (floatp(item))
            output += ({ sprintf("%f", item) });
        else
            output += ({ sprintf("%q", item) });
    }
    return "("+output*" "+")";
}

string input = "((data \"quoted data\" 123 4.5)\n (data (!@# (4.5) \"(more\" \"data)\")))";
array data = group(tokenizer(input))[0];
string output = sexp(data);
```


Output:
 ((data "quoted data" 123 4.5)
  (data (!@# (4.5) "(more" "data)")))

 ({ ({ (Symbol: data), "quoted data", 123, 4.5 }), ({ (Symbol: data), ({ (Symbol: !@#), ({ 4.5 }), "(more", "data)" }) }) })

 ((data "quoted data" 123 4.5) (data (!@# (45) "(more" "data)")))


## Potion

How values are stored: Tuples for list, integers for integers, floats for floats, strings for symbols, quoted strings for strings. This implementation is not the most elegant/succinct or practical (it's trusty and has no real error handling).

```potion
isdigit = (c): 47 < c ord and c ord < 58.
iswhitespace = (c): c ord == 10 or c ord == 13 or c == " ".

# str: a string of the form "...<nondigit>[{<symb>}]..."
# i: index to start at (must be the index of <nondigit>)
# => returns (<the symbol as a string>, <index after the last char>)
parsesymbol = (str, i) :
   datum = ()
   while (str(i) != "(" and str(i) != ")" and not iswhitespace(str(i)) and str(i) != "\"") :
      datum append(str(i++))
   .
   (datum join, i)
.

# str: a string of the form "...[<minus>]{<digit>}[<dot>{<digit>}]..."
# i: index to start at (must be the index of the first token)
# => returns (<float or int>, <index after the last digit>)
parsenumber = (str, i) :
   datum = ()
   dot = false
   while (str(i) != "(" and str(i) != ")" and not iswhitespace(str(i)) and str(i) != "\"") :
      if (str(i) == "."): dot = true.
      datum append(str(i++))
   .
   if (dot): (datum join number, i).
   else: (datum join number integer, i).
.

# str: a string of the form "...\"....\"..."
# i: index to start at (must be the index of the first quote)
# => returns (<the string>, <index after the last quote>)
parsestring = (str, i) :
   datum = ("\"")
   while (str(++i) != "\"") :
      datum append(str(i))
   .
   datum append("\"")
   (datum join, ++i)
.

# str: a string of the form "...(...)..."
# i: index to start at
# => returns (<tuple/list>, <index after the last paren>)
parselist = (str, i) :
   lst = ()
   data = ()
   while (str(i) != "("): i++.
   i++
   while (str(i) != ")") :
      if (not iswhitespace(str(i))) :
         if (isdigit(str(i)) or (str(i) == "-" and isdigit(str(i + 1)))): data = parsenumber(str, i).
         elsif (str(i) == "\""): data = parsestring(str, i).
         elsif (str(i) == "("): data = parselist(str, i).
         else: data = parsesymbol(str, i).
         lst append(data(0))
         i = data(1)
      . else :
         ++i
      .
   .
   (lst, ++i)
.

parsesexpr = (str) :
   parselist(str, 0)(0)
.

parsesexpr("(define (factorial x) \"compute factorial\" (version 2.0) (apply * (range 1 x)))") string print
"\n" print
parsesexpr("((data \"quoted data\" 123 4.5)
 (data (!@# (4.5) \"(more\" \"data)\")))") string print
"\n" print
```



## Python


```python
import re

dbg = False

term_regex = r'''(?mx)
    \s*(?:
        (?P<brackl>\()|
        (?P<brackr>\))|
        (?P<num>\-?\d+\.\d+|\-?\d+)|
        (?P<sq>"[^"]*")|
        (?P<s>[^(^)\s]+)
       )'''

def parse_sexp(sexp):
    stack = []
    out = []
    if dbg: print("%-6s %-14s %-44s %-s" % tuple("term value out stack".split()))
    for termtypes in re.finditer(term_regex, sexp):
        term, value = [(t,v) for t,v in termtypes.groupdict().items() if v][0]
        if dbg: print("%-7s %-14s %-44r %-r" % (term, value, out, stack))
        if   term == 'brackl':
            stack.append(out)
            out = []
        elif term == 'brackr':
            assert stack, "Trouble with nesting of brackets"
            tmpout, out = out, stack.pop(-1)
            out.append(tmpout)
        elif term == 'num':
            v = float(value)
            if v.is_integer(): v = int(v)
            out.append(v)
        elif term == 'sq':
            out.append(value[1:-1])
        elif term == 's':
            out.append(value)
        else:
            raise NotImplementedError("Error: %r" % (term, value))
    assert not stack, "Trouble with nesting of brackets"
    return out[0]

def print_sexp(exp):
    out = ''
    if type(exp) == type([]):
        out += '(' + ' '.join(print_sexp(x) for x in exp) + ')'
    elif type(exp) == type('') and re.search(r'[\s()]', exp):
        out += '"%s"' % repr(exp)[1:-1].replace('"', '\"')
    else:
        out += '%s' % exp
    return out


if __name__ == '__main__':
    sexp = ''' ( ( data "quoted data" 123 4.5)
         (data (123 (4.5) "(more" "data)")))'''

    print('Input S-expression: %r' % (sexp, ))
    parsed = parse_sexp(sexp)
    print("\nParsed to Python:", parsed)

    print("\nThen back to: '%s'" % print_sexp(parsed))
```


;Output:

```txt
Input S-expression: '((data "quoted data" 123 4.5)\n         (data (123 (4.5) "(more" "data)")))'

Parsed to Python: [['data', 'quoted data', 123, 4.5], ['data', [123, [4.5], '(more', 'data)']]]

Then back to: '((data "quoted data" 123 4.5) (data (123 (4.5) "(more" "data)")))'
```


;Simpler parser:
Note that in the example above the parser also recognises and changes the type of some tokens as well as generating a nested list. If that functionality is not needed, or better done elsewhere, then the parse function can be achieved more simply by just applying the regexp:

```python>>>
 from pprint import pprint as pp
>>> x = [[(t,v) for t,v in  termtypes.groupdict().items() if v][0] for termtypes in re.finditer(term_regex, sexp)]
>>> pp(x)
[('brackl', '('),
 ('brackl', '('),
 ('s', 'data'),
 ('sq', '"quoted data"'),
 ('num', '123'),
 ('num', '4.5'),
 ('brackr', ')'),
 ('brackl', '('),
 ('s', 'data'),
 ('brackl', '('),
 ('num', '123'),
 ('brackl', '('),
 ('num', '4.5'),
 ('brackr', ')'),
 ('sq', '"(more"'),
 ('sq', '"data)"'),
 ('brackr', ')'),
 ('brackr', ')'),
 ('brackr', ')')]
>>>
```



## Racket


Racket has builtin support for S-expressions in the form of the read function.

```racket

#lang racket
(define input
#<<---
((data "quoted data" 123 4.5)
 (data (!@# (4.5) "(more" "data)")))
---
  )

(read (open-input-string input))

```

Output:

```txt

'((data "quoted data" 123 4.5) (data (!@# (4.5) "(more" "data)")))

```



## REXX

The checking of errors has been minimized (issuing of error message is very rudimentary, as is the error recovery).

More grouping symbols have been added   (brackets   '''[ ]''',   braces   '''{ }''',   and   guillemets   <big>'''« »''')</big>,   as well as another types of literals.

Also added were two more separators   (a comma and semicolon).

Separators that could be added are more whitespace characters (vertical/horizontal tabs, line feed, form feed, tab char, etc).


It would normally be considered improper, but the literal string delimiters were left intact; making it much easier to understand what is/was being parsed.

```rexx
/*REXX program  parses  an   S-expression   and  displays the results to the terminal.  */
input= '((data "quoted data" 123 4.5) (data (!@# (4.5) "(more" "data)")))'
say center('input', length(input), "═")          /*display the header title to terminal.*/
say         input                                /*   "     "  input data    "    "     */
say copies('═',     length(input) )              /*   "     "  header sep    "    "     */
grpO.=;      grpO.1 = '{'   ;    grpC.1 = "}"    /*pair of grouping symbol: braces      */
             grpO.2 = '['   ;    grpC.2 = "]"    /*  "   "    "       "     brackets    */
             grpO.3 = '('   ;    grpC.3 = ")"    /*  "   "    "       "     parentheses */
             grpO.4 = '«'   ;    grpC.4 = "»"    /*  "   "    "       "     guillemets  */
q.=;            q.1 = "'"   ;       q.2 = '"'    /*1st and 2nd literal string delimiter.*/
#        = 0                                     /*the number of tokens found (so far). */
tabs     = 10                                    /*used for the indenting of the levels.*/
seps     = ',;'                                  /*characters used for separation.      */
atoms    = ' 'seps                               /*     "       "  to  separate atoms.  */
level    = 0                                     /*the current level being processed.   */
quoted   = 0                                     /*quotation level  (for nested quotes).*/
grpU     =                                       /*used to go   up  an expression level.*/
grpD     =                                       /*  "   "  "  down  "     "       "    */
@.=;        do n=1  while grpO.n\==''
            atoms = atoms || grpO.n || grpC.n    /*add Open and Closed groups to  ATOMS.*/
            grpU  = grpU  || grpO.n              /*add Open            groups to  GRPU, */
            grpD  = grpD  || grpC.n              /*add          Closed groups to  GRPD, */
            end   /*n*/                          /* [↑]  handle a bunch of grouping syms*/
literals=
            do k=1  while q.k\=='';  literals=literals || q.k   /*add literal delimiters*/
            end   /*k*/
!=;                                      literalStart=
      do j=1  to length(input);          $=substr(input, j, 1)                             /* ◄■■■■■text parsing*/
                                                                                           /* ◄■■■■■text parsing*/
      if quoted                then do;  !=! || $;    if $==literalStart  then quoted=0    /* ◄■■■■■text parsing*/
                                         iterate                                           /* ◄■■■■■text parsing*/
                                    end            /* [↑]  handle running  quoted str.*/   /* ◄■■■■■text parsing*/
                                                                                           /* ◄■■■■■text parsing*/
      if pos($, literals)\==0  then do;  literalStart=$;      !=! || $;        quoted=1    /* ◄■■■■■text parsing*/
                                         iterate                                           /* ◄■■■■■text parsing*/
                                    end            /* [↑]  handle start of quoted str.*/   /* ◄■■■■■text parsing*/
                                                                                           /* ◄■■■■■text parsing*/
      if pos($, atoms)==0      then do;  !=! || $ ;   iterate;   end  /*is    an atom?*/   /* ◄■■■■■text parsing*/
                               else do;  call add!;   !=$;       end  /*isn't an atam?*/   /* ◄■■■■■text parsing*/
                                                                                           /* ◄■■■■■text parsing*/
      if pos($, literals)==0   then do;  if pos($, grpU)\==0  then level=level + 1         /* ◄■■■■■text parsing*/
                                         call add!                                         /* ◄■■■■■text parsing*/
                                         if pos($, grpD)\==0  then level=level - 1         /* ◄■■■■■text parsing*/
                                         if level<0  then say  'error, mismatched'   $     /* ◄■■■■■text parsing*/
                                    end                                                    /* ◄■■■■■text parsing*/
      end   /*j*/                                                                          /* ◄■■■■■text parsing*/
                                                                                           /* ◄■■■■■text parsing*/
call add!                                        /*process any residual tokens.*/          /* ◄■■■■■text parsing*/
if level\==0  then say  'error, mismatched grouping symbol'                                /* ◄■■■■■text parsing*/
if quoted     then say  'error, no end of quoted literal'      literalStart                /* ◄■■■■■text parsing*/

      do m=1  for #;   say @.m;     end  /*m*/   /*display the tokens to the terminal.  */
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
add!: if !=''  then return;   #=#+1;  @.#=left("", max(0, tabs*(level-1)))!;  !=;   return
```

{{out|output|text=  when using the default input:}}

```txt

══════════════════════════════input══════════════════════════════
((data "quoted data" 123 4.5) (data (!@# (4.5) "(more" "data)")))
═════════════════════════════════════════════════════════════════
(
          (
          data
           "quoted data"
           123
           4.5
          )
          (
          data
                    (
                    !@#
                              (
                              4.5
                              )
                     "(more"
                     "data)"
                    )
          )
)

```



## Ruby

{{works with|Ruby|1.9}}

```ruby
class SExpr
  def initialize(str)
    @original = str
    @data = parse_sexpr(str)
  end
  attr_reader :data, :original

  def to_sexpr
    @data.to_sexpr
  end

  private

  def parse_sexpr(str)
    state = :token_start
    tokens = []
    word = ""
    str.each_char do |char|
      case state

      when :token_start
        case char
        when "("
          tokens << :lbr
        when ")"
          tokens << :rbr
        when /\s/
          # do nothing, just consume the whitespace
        when  '"'
          state = :read_quoted_string
          word = ""
        else
          state = :read_string_or_number
          word = char
        end

      when :read_quoted_string
        case char
        when '"'
          tokens << word
          state = :token_start
        else
          word << char
        end

      when :read_string_or_number
        case char
        when /\s/
          tokens << symbol_or_number(word)
          state = :token_start
        when ')'
          tokens << symbol_or_number(word)
          tokens << :rbr
          state = :token_start
        else
          word << char
        end
      end
    end

    sexpr_tokens_to_array(tokens)
  end

  def symbol_or_number(word)
    Integer(word)
  rescue ArgumentError
    begin
      Float(word)
    rescue ArgumentError
      word.to_sym
    end
  end

  def sexpr_tokens_to_array(tokens, idx = 0)
    result = []
    while idx < tokens.length
      case tokens[idx]
      when :lbr
        tmp, idx = sexpr_tokens_to_array(tokens, idx + 1)
        result << tmp
      when :rbr
        return [result, idx]
      else
        result << tokens[idx]
      end
      idx += 1
    end
    result[0]
  end
end

class Object
  def to_sexpr
    self
  end
end

class String
  def to_sexpr
    self.match(/[\s()]/) ? self.inspect : self
  end
end

class Symbol
  alias :to_sexpr :to_s
end

class Array
  def to_sexpr
    "(%s)" % inject([]) {|a, elem| a << elem.to_sexpr}.join(" ")
  end
end


sexpr = SExpr.new <<END
((data "quoted data" 123 4.5)
 (data (!@# (4.5) "(more" "data)")))
END

puts "original sexpr:\n#{sexpr.original}"
puts "\nruby data structure:\n#{sexpr.data}"
puts "\nand back to S-Expr:\n#{sexpr.to_sexpr}"
```


{{out}}

```txt

original sexpr:
((data "quoted data" 123 4.5)
 (data (!@# (4.5) "(more" "data)")))

ruby data structure:
[[:data, "quoted data", 123, 4.5], [:data, [:"!@#", [4.5], "(more", "data)"]]]

and back to S-Expr:
((data "quoted data" 123 4.5) (data (!@# (4.5) "(more" "data)")))

```



## Scheme

Because Scheme, like all serious lisp implementations, has a native function called read for parsing s-expressions,
this code will never be used. It serves more as an example of how to write simple parsers in Scheme.
It also forgos turning things into their native scheme representation and uses strings for all atoms of data.

Note that this example includes erroneous closing quotes when checking for #\" because syntax highlighting sucks
and no one should have to wade through blocks of red.

Using guile scheme 2.0.11


```scheme
(define (sexpr-read port)
  (define (help port)
    (let ((char (read-char port)))
      (cond
       ((or (eof-object? char) (eq? char #\) )) '())
       ((eq? char #\( ) (cons (help port) (help port)))
       ((char-whitespace? char) (help port))
       ((eq? char #\"") (cons (quote-read port) (help port)))
       (#t (unread-char char port) (cons (string-read port) (help port))))))
  ; This is needed because the function conses all parsed sexprs onto something,
  ; so the top expression is one level too deep.
  (car (help port)))

(define (quote-read port)
  (define (help port)
    (let ((char (read-char port)))
      (if
       (or (eof-object? char) (eq? char #\""))
       '()
       (cons char (help port)))))
  (list->string (help port)))

(define (string-read port)
  (define (help port)
    (let ((char (read-char port)))
      (cond
       ((or (eof-object? char) (char-whitespace? char)) '())
       ((eq? char #\) ) (unread-char char port) '())
       (#t (cons char (help port))))))
  (list->string (help port)))

(define (format-sexpr expr)
  (define (help expr pad)
    (if
     (list? expr)
     (begin
      (format #t "~a(~%" (make-string pad #\tab))
      (for-each (lambda (x) (help x (1+ pad))) expr)
      (format #t "~a)~%" (make-string pad #\tab)))
     (format #t "~a~a~%" (make-string pad #\tab) expr)))
  (help expr 0))

(format-sexpr (sexpr-read
 (open-input-string "((data \"quoted data\" 123 4.5) (data (!@# (4.5) \"(more\" \"data)\")))")))
```


Output:
 (
 	(
 		data
 		quoted data
 		123
 		4.5
 	)
 	(
 		data
 		(
 			!@#
 			(
 				4.5
 			)
 			(more
 			data)
 		)
 	)
 )


## Sidef

{{trans|Perl}}

```ruby
var t = frequire('Text::Balanced');

func sexpr(txt) {
    txt.trim!;

    var m = txt.match(/^\((.*)\)$/s) || die "Invalid: <<#{txt}>>";
    txt = m[0];

    var w;
    var ret = [];
    while (!txt.is_empty) {
        given (txt.first) {
            when('(') {
                (w, txt) = t.extract_bracketed(txt, '()');
                w = sexpr(w);
            }
            when ('"') {
                (w, txt) = t.extract_delimited(txt, '"')
                w.sub!(/^"(.*)"/, {|s1| s1 });
            }
            default {
                txt.sub!(/^(\S+)/, {|s1| w = s1; '' });
            }
        }
        ret << w;
        txt.trim_beg!;
    }
    return ret;
}

func sexpr2txt(String e) {
    e ~~ /[\s"\(\)]/ ? do { e.gsub!('"', '\\"'); %Q("#{e}") } : e;
}

func sexpr2txt(expr) {
    '(' + expr.map {|e| sexpr2txt(e) }.join(' ') + ')';
}

var s = sexpr(%q{

((data "quoted data" 123 4.5)
 (data (!@# (4.5) "(more" "data)")))

});

say s;              # dump structure
say sexpr2txt(s);   # convert back
```

{{out}}

```txt

[["data", "quoted data", "123", "4.5"], ["data", ["!\@#", ["4.5"], "(more", "data)"]]]
((data "quoted data" 123 4.5) (data (!@# (4.5) "(more" "data)")))

```



## Tcl

Note that because Tcl doesn't expose a type system (well, not in a conventional sense) the parts of the parsed out data structure are tagged lists; the first element is one of “<tt>string</tt>”, “<tt>int</tt>”, “<tt>real</tt>” and “<tt>atom</tt>” to indicate a leaf token, or “<tt>list</tt>” to indicate a sublist. A “native” data structure could also be generated, but then that would turn things into lists that are not in the original.

```tcl
package require Tcl 8.5

proc fromSexp {str} {
    set tokenizer {[()]|"(?:[^\\""]|\\.)*"|(?:[^()""\s\\]|\\.)+|[""]}
    set stack {}
    set accum {}
    foreach token [regexp -inline -all $tokenizer $str] {
	if {$token eq "("} {
	    lappend stack $accum
	    set accum {}
	} elseif {$token eq ")"} {
	    if {![llength $stack]} {error "unbalanced"}
	    set accum [list {*}[lindex $stack end] [list list {*}$accum]]
	    set stack [lrange $stack 0 end-1]
	} elseif {$token eq "\""} {
	    error "bad quote"
	} elseif {[string match {"*"} $token]} {
	    set token [string range $token 1 end-1]
	    lappend accum [list string [regsub -all {\\(.)} $token {\1}]]
	} else {
	    if {[string is integer -strict $token]} {
		set type int
	    } elseif {[string is double -strict $token]} {
		set type real
	    } else {
		set type atom
	    }
	    lappend accum [list $type [regsub -all {\\(.)} $token {\1}]]
	}
    }
    if {[llength $stack]} {error "unbalanced"}
    return [lindex $accum 0]
}
proc toSexp {tokList} {
    set content [lassign $tokList type]
    if {$type eq "list"} {
	set s "("
	set sep ""
	foreach bit $content {
	    append s $sep [toSexp $bit]
	    set sep " "
	}
	return [append s ")"]
    } elseif {$type eq "string"} {
	return "\"[regsub -all {[\\""]} [lindex $content 0] {\\\0}]\""
    } else {
	return [lindex $content 0]
    }
}
```

Demonstrating with the sample data:

```tcl
set sample {((data "quoted data" 123 4.5)
 (data (!@# (4.5) "(more" "data)")))}
set parsed [fromSexp $sample]
puts "sample: $sample"
puts "parsed: $parsed"
puts "regen: [toSexp $parsed]"
```

Output:

```txt

sample: ((data "quoted data" 123 4.5)
 (data (!@# (4.5) "(more" "data)")))
parsed: list {list {atom data} {string {quoted data}} {int 123} {real 4.5}} {list {atom data} {list {atom !@#} {list {real 4.5}} {string (more} {string data)}}}
regen: ((data "quoted data" 123 4.5) (data (!@# (4.5) "(more" "data)")))

```

As you can see, whitespace is not preserved in non-terminal locations.


## TXR


TXR is in the Lisp family, and uses S-Expressions. So right from the system prompt we can do:


```txt
$ txr -p '(read)'
((data "quoted data" 123 4.5)
 (data (!@# (4.5) "(more" "data)")))
[Ctrl-D][Enter]
((data "quoted data" 123 4.5) (data (! (sys:var #(4.5)) "(more" "data)")))
```


However, note that the <code>@</code> character has a special meaning: <code>@obj</code> turns into <code>(sys:var obj)</code>. The purpose of this notation is to support Lisp code that requires meta-variables and meta-expressions. This can be used, for instance, in pattern matching to distinguish binding variables and matching operations from literal syntax.

The following solution avoids "cheating" in this way with the built-in parser; it implements a from-scratch S-exp parser which treats <code>!@#</code> as just a symbol.

The grammar is roughly as follows:


```txt

expr := ws? atom
     |  ws? ( ws? expr* ws? )

atom := float | int | sym | str

float := sign? digit+ . digit* exponent?
      |  sign? digit* . digit+ exponent?
      |  sign? digit+ exponent

int := sign? digit+

str := " (\" | anychar )* "

sym := sym-char +

sym-char := /* non-whitespace, but not ( and not ) */
```


Code:


```txr
@(define float (f))@\
  @(local (tok))@\
  @(cases)@\
    @{tok /[+\-]?\d+\.\d*([Ee][+\-]?\d+)?/}@\
  @(or)@\
    @{tok /[+\-]?\d*\.\d+([Ee][+\-]?\d+)?/}@\
  @(or)@\
    @{tok /[+\-]?\d+[Ee][+\-]?\d+/}@\
  @(end)@\
  @(bind f @(flo-str tok))@\
@(end)
@(define int (i))@\
  @(local (tok))@\
  @{tok /[+\-]?\d+/}@\
  @(bind i @(int-str tok))@\
@(end)
@(define sym (s))@\
  @(local (tok))@\
  @{tok /[^\s()]+/}@\
  @(bind s @(intern tok))@\
@(end)
@(define str (s))@\
  @(local (tok))@\
  @{tok /"(\\"|[^"])*"/}@\
  @(bind s @[tok 1..-1])@\
@(end)
@(define atom (a))@\
  @(cases)@\
    @(float a)@(or)@(int a)@(or)@(str a)@(or)@(sym a)@\
  @(end)@\
@(end)
@(define expr (e))@\
  @(cases)@\
    @/\s*/@(atom e)@\
  @(or)@\
    @/\s*\(\s*/@(coll :vars (e))@(expr e)@/\s*/@(last))@(end)@\
  @(end)@\
@(end)
@(freeform)
@(expr e)@junk
@(output)
expr: @(format nil "~s" e)
junk: @junk
@(end)
```


Run:


```txt
$ txr s-expressions.txr -
()
expr: nil
junk:
$ txr s-expressions.txr -
3e3
expr: 3000.0
junk:
$ txr s-expressions.txr -
+3
expr: 3
junk:
$ txr s-expressions.txr -
abc*
expr: abc*
junk:
$ txr s-expressions.txr -
abc*)
expr: abc*
junk: )
$ txr s-expressions.txr -
((data "quoted data" 123 4.5)
 (data (!@# (4.5) "(more" "data)")))
expr: ((data "quoted data" 123 4.5) (data (!@# (4.5) "(more" "data)")))
junk:

```


TODO: Note that the recognizer for string literals does not actually process the interior escape sequences <code>\"</code>; these remain as part of the string data. The only processing is the stripping of the outer quotes from the lexeme.

Explanation of most confusing line:


```txr
    @/\s*\(\s*/@(coll :vars (e))@(expr e)@/\s*/@(last))@(end)
```


First, we match an open parenthesis that can be embedded in whitespace. Then we have a <code>@(coll)</code> construct which terminates with <code>@(end)</code>. This is a repetition construct for collecting zero or more items. The <code>:vars (e)</code> argument makes the collect strict: each repetition must bind the variable <code>e</code>. More importantly, in this case, if nothing is
collected, then <code>e</code> gets bound to <code>nil</code> (the empty list). The collect construct does not look at context beyond itself. To terminate the collect at the closing parenthesis we use <code>@(last))</code>. The second closing parenthesis here is literal text to be matched, not TXR syntax. This special clause establishes the terminating context without which the collect will munge all input. When the last clause matches, whatever it matches is consumed and the collect ends. (There is a related <code>@(until)</code> clause which terminates the collect, but leaves its own match unconsumed.)

{{omit from|Brlcad}}
{{omit from|GUISS}}
