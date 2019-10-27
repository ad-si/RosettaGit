+++
title = "Arithmetic evaluation/Ada"
description = ""
date = 2010-02-06T12:26:11Z
aliases = []
[extra]
id = 4821
[taxonomies]
categories = []
tags = []
+++

{{libheader|Simple components for Ada}}

The following implementation uses table-driven parsers provided by [[:Category: Simple components for Ada|Simple components for Ada]]. A parser is controlled by the tables of prefix, infix and postfix operations. Between the operations it calls ''Get_Operand'' in order to recognize expression terms. The parser communicates with its back end using the primitive operations ''Call'' and ''Enclose''. The former is used for operations, the latter is for brackets. For this example we generate the parsing tree from there. There are many other operations which are used for advanced parsing and optimization, here they are defined as trivially returning True or False.

A parsing tree node has one operation ''Evaluate'' in order to calculate the expression. The nodes are allocated in an arena implemented by a storage pool. The pools is organized as a stack, so that the whole tree is popped when no more needed. This is a standard technique in compiler construction.

The implementation provides an advanced error handling and skipping blanks and Ada comments (these are taken from the library).

```ada

with Ada.Unchecked_Deallocation;
with Parsers.String_Source;            use Parsers.String_Source;
with Parsers.Generic_Lexer.Ada_Blanks;
with Parsers.Generic_Token.Segmented_Lexer;
with Stack_Storage;
with Tables.Names;

package Parsers.Simple is
   type Operations is (Add, Sub, Mul, Div, Left_Bracket, Right_Bracket);
   type Priorities is mod 3; -- The levels of association

   function "and" (Left, Right : Operations) return Boolean;
   function Is_Commutative (Left, Right : Operations) return Boolean;
   function Is_Inverse (Operation : Operations) return Boolean;
   function Group_Inverse (Operation : Operations) return Operations;

   Tree_Pool : Stack_Storage.Pool (2048, 128); -- Arena for the tree
      -- Tree nodes
   type Node is abstract tagged limited null record;
   function Evaluate (Item : Node) return Integer is abstract;
   type Node_Ptr is access Node'Class;
   for Node_Ptr'Storage_Pool use Tree_Pool;
   procedure Free is
      new Standard.Ada.Unchecked_Deallocation (Node'Class, Node_Ptr);
      -- Stub of the arena
   type Mark is new Node with null record;
   overriding function Evaluate (Item : Mark) return Integer;
      -- Terminal nodes
   type Literal is new Node with record
      Location : Parsers.String_Source.Location;
      Value    : Integer;
   end record;
   overriding function Evaluate (Item : Literal) return Integer;
      -- Non-terminal nodes
   type Argument_List is array (Positive range <>) of Node_Ptr;
   type Expression (Count : Positive) is new Node with record
      Operation : Operations;
      Location  : Parsers.String_Source.Location;
      Operands  : Argument_List (1..Count);
   end record;
   overriding function Evaluate (Item : Expression) return Integer;

   package Tokens is -- The lexical tokens
      new Parsers.Generic_Token
          (  Operation_Type => Operations,
             Argument_Type  => Node_Ptr,
             Priority_Type  => Priorities,
             Sources        => Code
          );
   use Tokens;

   procedure Check_Spelling (Name : String);
   function Check_Matched (Source : String; Pointer : Integer)
      return Boolean;
   package Token_Tables is new Tokens.Vocabulary.Names;
      -- The tables of prefix, infix and postfix operations
   Prefixes  : aliased Token_Tables.Dictionary;
   Infixes   : aliased Token_Tables.Dictionary;
   Postfixes : aliased Token_Tables.Dictionary;

   package Lexers is new Tokens.Segmented_Lexer; -- Table driven lexers
   package Blank_Skipping_Lexers is -- Lexers that skip Ada blanks
      new Lexers.Token_Lexer.Implementation.Ada_Blanks (Lexers.Lexer);

   type Simple_Expression is -- The lexer that uses our tables
      new Blank_Skipping_Lexers.Lexer
          (  Prefixes  => Prefixes'Access,
             Infixes   => Infixes'Access,
             Postfixes => Postfixes'Access
          )  with null record;
   overriding -- Evaluates an operator
      function Call
               (  Context   : access Simple_Expression;
                  Operation : Tokens.Operation_Token;
                  List      : Tokens.Arguments.Frame
               )  return Tokens.Argument_Token;
   overriding -- Evaluates an expression in brackets
      function Enclose
               (  Context : access Simple_Expression;
                  Left    : Tokens.Operation_Token;
                  Right   : Tokens.Operation_Token;
                  List    : Tokens.Arguments.Frame
               )  return Tokens.Argument_Token;
   overriding -- Recognizes an operand (float number)
      procedure Get_Operand
                (  Context  : in out Simple_Expression;
                   Code     : in out Source;
                   Argument : out Tokens.Argument_Token;
                   Got_It   : out Boolean
                );
end Parsers.Simple;

```

Here is the implementation of the package.

```ada

with Ada.Characters.Handling;  use Ada.Characters.Handling;
with Ada.Exceptions;           use Ada.Exceptions;
with Ada.IO_Exceptions;        use Ada.IO_Exceptions;
with Strings_Edit.Integers;    use Strings_Edit.Integers;

package body Parsers.Simple is 

   function "and" (Left, Right : Operations) return Boolean is
   begin
      return True;
   end "and";

   function Is_Commutative (Left, Right : Operations) return Boolean is
   begin
      return False;
   end Is_Commutative;

   function Is_Inverse (Operation : Operations) return Boolean is
   begin
      return False;
   end Is_Inverse;

   function Group_Inverse (Operation : Operations) return Operations is
   begin
      return Mul;
   end Group_Inverse;

   procedure Check_Spelling (Name : String) is
   begin
      null;
   end Check_Spelling;

   function Check_Matched (Source : String; Pointer : Integer)
      return Boolean is
   begin
      return
      (  not Is_Alphanumeric (Source (Pointer))
      or else
         not Is_Alphanumeric (Source (Pointer - 1))
      );
   end Check_Matched;

   function Call
            (  Context   : access Simple_Expression;
               Operation : Tokens.Operation_Token;
               List      : Tokens.Arguments.Frame
            )  return Tokens.Argument_Token is
      Result : Node_Ptr := new Expression (List'Length);
   begin
      declare
         This : Expression renames Expression (Result.all);
      begin
         This.Operation := Operation.Operation;
         This.Location  := Operation.Location;
         for Argument in List'Range loop
            This.Operands (Integer (Argument)) :=
               List (Argument).Value;
         end loop;
      end;
      return (Result, Operation.Location & Link (List));
   end Call;

   function Enclose
            (  Context : access Simple_Expression;
               Left    : Tokens.Operation_Token;
               Right   : Tokens.Operation_Token;
               List    : Tokens.Arguments.Frame
            )  return Tokens.Argument_Token is
      Result : Node_Ptr := new Expression (List'Length);
   begin
      declare
         This : Expression renames Expression (Result.all);
      begin
         This.Operation := Left.Operation;
         This.Location  := Left.Location & Right.Location;
         for Argument in List'Range loop
            This.Operands (Integer (Argument)) :=
               List (Argument).Value;
         end loop;
      end;
      return (Result, Left.Location & Right.Location & Link (List));
   end Enclose;

   procedure Get_Operand
             (  Context  : in out Simple_Expression;
                Code     : in out Source;
                Argument : out Tokens.Argument_Token;
                Got_It   : out Boolean
             )  is
      Line    : String renames Get_Line (Code);
      Pointer : Integer := Get_Pointer (Code);
      Value   : Integer;
   begin
      if Is_Decimal_Digit (Line (Pointer)) then
         Get (Line, Pointer, Value);
         Set_Pointer (Code, Pointer);
         Argument.Location := Link (Code);
         Argument.Value := new Literal;
         declare
            Result : Literal renames Literal (Argument.Value.all);
         begin
            Result.Value    := Value;
            Result.Location := Argument.Location;
         end;
         Got_It := True;
      else
         Got_It := False;
      end if;
   exception
      when Constraint_Error =>
         Raise_Exception
         (  Parsers.Syntax_Error'Identity,
            "Too large number at " &  Image (Link (Code))
         );
      when Data_Error =>
         Raise_Exception
         (  Parsers.Syntax_Error'Identity,
            "Malformed number at " &  Image (Link (Code))
         );
      when End_Error =>
         Got_It := False;
   end Get_Operand;

   function Evaluate (Item : Mark) return Integer is
   begin
      return 0;
   end Evaluate;

   function Evaluate (Item : Literal) return Integer is
   begin
      return Item.Value;
   end Evaluate;

   function Evaluate (Item : Expression) return Integer is
      Argument : array (Item.Operands'Range) of Integer;
   begin
      for I in Argument'Range loop
        Argument (I) := Item.Operands (I).Evaluate;
      end loop;
      case Item.Operation is
         when Add => return Argument (1) + Argument (2);
         when Sub => return Argument (1) - Argument (2);
         when Mul => return Argument (1) * Argument (2);
         when Div => return Argument (1) / Argument (2);
         when others => return Argument (1);
      end case;
   exception
      when Constraint_Error =>
         Raise_Exception
         (  Parsers.Syntax_Error'Identity,
            "Numeric error at " & Image (Item.Location)
         );
   end Evaluate;

   use type Tokens.Descriptors.Descriptor_Class;
   use Lexers.Lexical_Descriptors.Operation;
   use Lexers.Lexical_Arguments;

begin
   Add_Operator (Infixes,   "+", Add, 1, 1);
   Add_Operator (Infixes,   "-", Sub, 1, 1);
   Add_Operator (Infixes,   "*", Mul, 2, 2);
   Add_Operator (Infixes,   "/", Div, 2, 2);

   Add_Bracket  (Prefixes,  "(", Left_Bracket);
   Add_Bracket  (Postfixes, ")", Right_Bracket);

end Parsers.Simple;

```

The next is a little test. It reads a line from the keyboard and then evaluates it. The program stops when the input is empty:

```ada

with Ada.Exceptions;                 use Ada.Exceptions;
with Ada.Text_IO;                    use Ada.Text_IO;
with Parsers.Simple;                 use Parsers.Simple;
with Parsers.String_Source;          use Parsers.String_Source;
with Strings_Edit.Integers;          use Strings_Edit.Integers;
with Parsers.Generic_Source.Text_IO;

procedure Test_Simple_Parser is
   use Lexers, Tokens;

   package Text_IO is new Code.Text_IO;
   use Text_IO;

   Parser : Simple_Expression;
   Result : Argument_Token;
   Stub   : Node_Ptr;
begin
   loop
      Put ("Expression:");
      declare
         Line : aliased String := Get_Line;
         Code : Source (Line'Access);
      begin
         exit when Line'Length = 0;
         Stub := new Mark; -- Mark the tree stack
         begin
            Parse (Parser, Code, Result);
            Put_Line
            (  Image (Result.Location)
            &  " = "
            &  Image (Result.Value.Evaluate)
            );
         exception
            when Error : Parsers.Syntax_Error =>
               Put_Line ("Error : " & Exception_Message (Error));
         end;
         Free (Stub);      -- Release the stack
      end;
   end loop;
end Test_Simple_Parser;

```

Sample exchange. When the expression is evaluated its range in the source string is indicated. Upon errors, the location of is shown as well:

```txt

Expression:(3 * 50) - (100 / 10)
1..21 = 140
Expression:1+
Error : Operand expected at 3
Expression:39999999999*9999999999+23
Error : Too large number at 1
Expression:5/0
Error : Numeric error at 2..2
Expression:

```

