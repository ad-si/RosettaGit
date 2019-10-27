+++
title = "Execute Brain****/Ada"
description = ""
date = 2010-08-19T16:46:37Z
aliases = []
[extra]
id = 2776
[taxonomies]
categories = []
tags = []
+++

{{implementation|Brainf***}}{{collection|RCBF}}
This is a simple implementation of [[Brainf***]] (it will be called just BF in what follows). It is written in [[Ada]].
=Standalone interpreter=
This implementation imposes hard limits in the size of both the memory and the program size. The whole program is read first, and then interpreted. The program must be stored in a file. The filename is given as the only argument to the interpreter.

The program reads input data from the standard input and writes output data to the standard output. Error messages go to the standard error output. Of course, these channels can be redirected to files, as usual, when invoking the BF interpreter.

The program is stored internally in a fixed length array of characters. Programs larger than the array size cannot be processed.

The internal data memory is implemented as a fixed length array of integers. Programs that attempt to go beyond the memory limits will stop with an error message at run time.

To allow using the interpreter in an interactive terminal session, control characters are ignored on input. But they can be generated on output, like any other ASCII character.

Memory cells are intended to have non-negative values. A special negative value represents an undefined value. End-of-file conditions are signaled by this special undefined value in read/write operations.

More detailed information about the code can be found in the comments throughout it.<br clear=all>


```ada
-- BF Interpreter

-- Usage:  bf programfile[.bf]

--   reads input data from standard input (control chars are ignored)
--   writes output data to standard output
--   writes error messages to standard error

with Ada.Text_Io;
use Ada.Text_Io;
with Ada.Command_Line;
use Ada.Command_Line;


procedure Bf is

   -- Data structures for memory
   Bottom   : constant Integer := - 1;
   subtype T_Data is Integer range Bottom..Integer'Last;        -- value range
   Mem_Last : constant Integer := 30000;                        -- memory size
   M        : array (0 .. Mem_Last) of T_Data := (others => 0); -- memory cells
   Mp       : Integer;                                          -- memory pointer

   -- Data structures for program text
   Prog_Name   : String := Argument (1);               -- program file name
   Prog_Suffix : constant String := ".bf";             -- standard program filename extension
   Program     : File_Type;                            -- program file
   Prog_Last   : constant Integer := 30000;            -- program storage size
   P           : array (0 .. Prog_Last) of Character;  -- program
   Pp          : Integer;                              -- program pointer
   Prog_Len    : Integer;                              -- program length

   -- Data structures for program execution control
   Level      : Integer;          -- nested loops level
   Data       : Character;        -- input datum
   Ok         : Boolean;          -- program execution switch
   End_Input  : Boolean := False; -- input EOF condition
   End_Output : Boolean := False; -- output EOF condition

   -- Compose and print an error message
   procedure Msgerr (
         Msg : in     String ) is
   begin
      New_Line;
      Put_Line( Standard_Error, "** error: " & Msg );
      Ok := False;
   end;

begin

   --get and open the program file
   declare
   begin
      Open( Program, In_File, Prog_Name);
   exception
      when others =>
         Open( Program, In_File, Prog_Name & Prog_Suffix);
   end;

   -- first, read the whole program file
   Prog_Len := 0;
   while not End_Of_File( Program ) loop
      Get_Immediate(Program, P(Prog_Len));
      Prog_Len := Prog_Len+1;
   end loop;
   Close( Program );

   -- then, interpret the program
   Pp := 0;
   Mp := 0;
   Ok := True;
   End_Input := False;
   End_Output := False;
   while Ok and (Pp < Prog_Len) loop -- while interpreting
      case P(Pp) is
         when '+' =>               -- (+) increment memory location
            M(Mp) := M(Mp)+1;

         when '-' =>               -- (-) decrement memory location
            if M(Mp) <= Bottom then
               Msgerr( "arithmetic underflow" );
            else
               M(Mp) := M(Mp)-1;
            end if;

         when '.' =>               -- (.) output data (print char)
            if End_Output then
               Msgerr("attempt to write past EOF");
            elsif M(Mp) < 0 then
               End_Output := True;
            else
               Put( Character'Val(M(Mp)) );
            end if;

         when ',' =>               -- (,) input data (read char)
            if End_Input then
               Msgerr("attempt to read past EOF");
            else
               loop
                  if End_Of_File(Standard_Input) then
                     M(Mp) := Bottom;
                     End_Input := True;
                     exit;
                  else
                     Get( Data );
                     M(Mp) := Character'Pos(Data);
                     -- ignore control chars
                     exit when Data >= ' ';
                  end if;
               end loop;
            end if;

         when '>' =>               -- (>) increment memory pointer (use next cell)
            Mp := Mp+1;
            if Mp >= Mem_Last then
               Msgerr("memory pointer overflow");
            end if;

         when '<' =>               -- (<) decrement memory pointer (use previous cell)
            Mp := Mp - 1;
            if Mp < 0 then
               Msgerr("memory pointer underflow");
            end if;

         when '[' =>               -- ([) begin of loop structure
            if M(Mp) = 0 then
               -- terminate loop, goto matching ']'
               Pp := Pp+1;
               Level := 0;
               while Pp < Prog_Len and then (Level > 0 or else P(Pp) /= ']') loop
                  if P(Pp) = '[' then
                     Level := Level+1;
                  end if;
                  if P(Pp) = ']' then
                     Level := Level-1;
                  end if;
                  Pp := Pp+1;
               end loop;
               if Pp >= Prog_Len then
                  Msgerr("no matching ']'");
               end if;
            end if;

         when ']' =>               -- (]) end of loop structure
            if M(Mp) /= 0 then
               -- repeat loop, goto matching '['
               Pp := Pp-1;
               Level := 0;
               while Pp >= 0 and then (Level > 0 or else P(Pp) /= '[') loop
                  if P(Pp) = ']' then
                     Level := Level+1;
                  end if;
                  if P(Pp) = '[' then
                     Level := Level-1;
                  end if;
                  Pp := Pp-1;
               end loop;
               if Pp < 0 then
                  Msgerr("no matching '['");
               end if;
            end if;

         when others =>
            null;                  -- (?) ignore unrecognized command
      end case;
      Pp := Pp+1;
   end loop; -- while interpreting
   New_Line;

end Bf;
```

=Callable interpreter=
This implementation provides a procedure that can be called to interpret a [[Brainf***]] stored in a string. The memory is passed as a parameter. Input and output of the memory cells is stream. By default the input and output streams are used. The interpreter uses the native machine memory. Upon errors such as addressing errors and program errors (unclosed brackets) Constraint_Error is propagated.

```Ada

with Ada.Streams;               use Ada.Streams;
with Ada.Text_IO.Text_Streams;  use Ada.Text_IO.Text_Streams;
with Ada.Text_IO;               use Ada.Text_IO;
with System.Storage_Elements;   use System.Storage_Elements;

procedure BF
          (  Source : String;
             Memory : in out Storage_Array;
             Input  : access Root_Stream_Type'Class := Stream (Standard_Input);
             Output : access Root_Stream_Type'Class := Stream (Standard_Output)
          )  is
   subtype Address is Storage_Offset range Memory'Range;
   PC      : Address := Address'First;
   Index   : Integer := Source'First;
   Nesting : Natural := 0;
begin
   while Index <= Source'Last loop
      case Source (Index) is
         when '>' => -- Increment PC
            PC := PC + 1;
         when '<' => -- Decrement PC
            PC := PC - 1;
         when '+' => -- Increment at PC
            Memory (PC) := Memory (PC) + 1;
         when '-' => -- Decrement at PC
            Memory (PC) := Memory (PC) - 1;
         when '.' => -- Output at PC
            Storage_Element'Write (Output, Memory (PC));
         when ',' => -- Input at PC
            Storage_Element'Read (Input, Memory (PC));
         when '[' => --	Forward if zero at PC
            if Memory (PC) = 0 then
               loop
                  Index := Index + 1;
                  case Source (Index) is
                     when '[' =>
                        Nesting := Nesting + 1;
                     when ']' =>
                        exit when Nesting = 0;
                        Nesting := Nesting - 1;
                     when others =>
                        null;
                  end case;
               end loop;
            end if;
         when ']' => --	Backward if non-zero at PC
            if Memory (PC) /= 0 then
               loop
                  Index := Index - 1;
                  case Source (Index) is
                     when '[' =>
                        exit when Nesting = 0;
                        Nesting := Nesting - 1;
                     when ']' =>
                        Nesting := Nesting + 1;
                     when others =>
                        null;
                  end case;
               end loop;
            end if;
         when others => -- Comment
            null;
      end case;
      Index := Index + 1;
   end loop;
end BF;

```

==Test programs==

### Hello world


```Ada

with System.Storage_Elements;  use System.Storage_Elements;
with BF;

procedure Test_BF_Hello is
   Memory : Storage_Array := (0..100_000 => 0);
begin
   BF ("++++++++++[>+++++++>++++++++++>+++>+<<<<-]>++.>+.+++++++..+++.>++.<<+++++++++++++++.>.+++.------.--------.>+.>.", Memory);
end Test_BF_Hello;

```

Sample output:

```txt

Hello World!

```


### Bracket test


```Ada

with System.Storage_Elements;  use System.Storage_Elements;
with BF;

procedure Test_BF is
   Memory : Storage_Array := (0..100_000 => 0);
begin
   BF (">>++++[<++++[<++++>-]>-]<<.[-]++++++++++.", Memory);
end Test_BF;

```

Sample output:

```txt

@

```

