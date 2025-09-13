+++
title = "Execute SNUSP/Ada"
description = ""
date = 2010-12-21T11:30:44Z
aliases = []
[extra]
id = 9009
[taxonomies]
categories = []
tags = []
+++


Interpreter for Modular [SNUSP](https://rosettacode.org/wiki/SNUSP).

First need some kind of storage for memory. Using Ada.Containers.Ordered_Maps for this. Only storing memory cells that have value /= 0, to save memory.

memory.ads:

```Ada
with Ada.Finalization;
generic
   type Byte is mod <>;
   type Key_Type is private;
   with function "<" (Left, Right : in Key_Type) return Boolean is <>;
package Memory is

   type Memory is new Ada.Finalization.Limited_Controlled with private;

   function Get_Value (Storage : in Memory; Key : in Key_Type) return Byte;

   procedure Set_Value
     (Storage : in out Memory;
      Key     : in Key_Type;
      Value   : in Byte);

   procedure Increment (Storage : in out Memory; Key : in Key_Type);

   procedure Decrement (Storage : in out Memory; Key : in Key_Type);

private

   type Memory_Type;

   type Memory_Access is access Memory_Type;

   type Memory is new Ada.Finalization.Limited_Controlled with record
      Cells : Memory_Access;
   end record;

   procedure Initialize (Storage : in out Memory);
   procedure Finalize (Storage : in out Memory);

end Memory;
```


memory.adb:

```Ada
with Ada.Containers.Ordered_Maps;
with Ada.Unchecked_Deallocation;
package body Memory is

   package Memory_Maps is new Ada.Containers.Ordered_Maps (
      Key_Type => Key_Type,
      Element_Type => Byte);
   use type Memory_Maps.Cursor;

   type Memory_Type is record
      Map : Memory_Maps.Map;
   end record;

   function Get_Value (Storage : in Memory; Key : in Key_Type) return Byte is
      Result   : Byte               := 0;
      Position : Memory_Maps.Cursor :=
         Memory_Maps.Find (Storage.Cells.Map, Key);
   begin
      if Position /= Memory_Maps.No_Element then
         Result := Memory_Maps.Element (Position);
      end if;
      return Result;
   end Get_Value;

   procedure Set_Value
     (Storage : in out Memory;
      Key     : in Key_Type;
      Value   : in Byte)
   is
      Position : Memory_Maps.Cursor :=
         Memory_Maps.Find (Storage.Cells.Map, Key);
   begin
      if Position = Memory_Maps.No_Element then
         if Value /= 0 then
            Memory_Maps.Insert (Storage.Cells.Map, Key, Value);
         end if;
      else
         if Value = 0 then
            Memory_Maps.Delete (Storage.Cells.Map, Key);
         else
            Memory_Maps.Replace (Storage.Cells.Map, Key, Value);
         end if;
      end if;
   end Set_Value;

   procedure Increment (Storage : in out Memory; Key : in Key_Type) is
      Value : Byte := Get_Value (Storage, Key) + 1;
   begin
      Set_Value (Storage, Key, Value);
   end Increment;

   procedure Decrement (Storage : in out Memory; Key : in Key_Type) is
      Value : Byte := Get_Value (Storage, Key) - 1;
   begin
      Set_Value (Storage, Key, Value);
   end Decrement;

   procedure Initialize (Storage : in out Memory) is
   begin
      Storage.Cells := new Memory_Type;
   end Initialize;

   procedure Finalize (Storage : in out Memory) is
      procedure Free is new Ada.Unchecked_Deallocation (
         Object => Memory_Type,
         Name => Memory_Access);
   begin
      Memory_Maps.Clear (Storage.Cells.Map);
      Free (Storage.Cells);
   end Finalize;

end Memory;
```


Next is a Machine for interpreting SNUSP code.

snusp.ads:

```Ada
with Ada.Containers.Vectors;
with Ada.Strings.Fixed;
with Ada.Text_IO;
with Memory;
package SNUSP is

   type SNUSP_Machine is limited private;

   type Code is array (Positive range <>, Positive range <>) of Character;

   procedure Run (Machine : in out SNUSP_Machine; Input : in Code);

private

   type Byte is mod 2 ** 8;
   package Byte_Memory is new Memory (Byte => Byte, Key_Type => Positive);

   type Direction is (Up, Down, Left, Right);
   type Code_Pointer is record
      X, Y : Positive  := 1;
      Dir  : Direction := Right;
   end record;
   package Call_Stacks is new Ada.Containers.Vectors (
      Index_Type => Positive,
      Element_Type => Code_Pointer);

   type SNUSP_Machine is limited record
      Main_Memory            : Byte_Memory.Memory;
      Current_Memory_Pointer : Positive := 1;
      Call_Stack             : Call_Stacks.Vector;
      IP                     : Code_Pointer;
   end record;

end SNUSP;
```


snusp.adb:

```Ada
package body SNUSP is

   procedure Move_IP (IP : in out Code_Pointer) is
   begin
      case IP.Dir is
         when Up =>
            IP.Y := IP.Y - 1;
         when Down =>
            IP.Y := IP.Y + 1;
         when Left =>
            IP.X := IP.X - 1;
         when Right =>
            IP.X := IP.X + 1;
      end case;
   end Move_IP;

   procedure Process_Next_Instruction
     (Machine : in out SNUSP_Machine;
      Input   : in Code)
   is
      Instruction : Character := Input (Machine.IP.Y, Machine.IP.X);
   begin
      case Instruction is
      when '>' =>
         Machine.Current_Memory_Pointer := Machine.Current_Memory_Pointer +
                                           1;
      when '<' =>
         Machine.Current_Memory_Pointer := Machine.Current_Memory_Pointer -
                                           1;
      when '+' =>
         Byte_Memory.Increment
           (Machine.Main_Memory,
            Machine.Current_Memory_Pointer);
      when '-' =>
         Byte_Memory.Decrement
           (Machine.Main_Memory,
            Machine.Current_Memory_Pointer);
      when ',' =>
         declare
            User_Input : Character;
            Value      : Byte;
         begin
            Ada.Text_IO.Get_Immediate (User_Input);
            Value := Character'Pos (User_Input);
            Byte_Memory.Set_Value
              (Machine.Main_Memory,
               Machine.Current_Memory_Pointer,
               Value);
         end;
      when '.' =>
         declare
            Value  : Byte      :=
               Byte_Memory.Get_Value
                 (Machine.Main_Memory,
                  Machine.Current_Memory_Pointer);
            Output : Character := Character'Val (Value);
         begin
            Ada.Text_IO.Put (Output);
         end;
      when '/' =>
         case Machine.IP.Dir is
            when Up =>
               Machine.IP.Dir := Right;
            when Down =>
               Machine.IP.Dir := Left;
            when Left =>
               Machine.IP.Dir := Down;
            when Right =>
               Machine.IP.Dir := Up;
         end case;
      when '\' =>
         case Machine.IP.Dir is
            when Up =>
               Machine.IP.Dir := Left;
            when Down =>
               Machine.IP.Dir := Right;
            when Left =>
               Machine.IP.Dir := Up;
            when Right =>
               Machine.IP.Dir := Down;
         end case;
      when '!' =>
         Move_IP (Machine.IP);
      when '?' =>
         if Byte_Memory.Get_Value
               (Machine.Main_Memory,
                Machine.Current_Memory_Pointer) =
            0
         then
            Move_IP (Machine.IP);
         end if;
      when '@' =>
         Call_Stacks.Append (Machine.Call_Stack, Machine.IP);
      when '#' =>
         Machine.IP := Call_Stacks.Last_Element (Machine.Call_Stack);
         Call_Stacks.Delete_Last (Machine.Call_Stack);
         Move_IP (Machine.IP);
      when others =>
         null;
      end case;
      Move_IP (Machine.IP);
   end Process_Next_Instruction;

   procedure Run (Machine : in out SNUSP_Machine; Input : in Code) is
   begin
      -- find begin ($)
      declare
         Start_Found : Boolean := False;
      begin
         for Row in Input'Range (1) loop
            for Col in Input'Range (2) loop
               if Input (Row, Col) = '$' then
                  if Start_Found then
                     raise Program_Error;
                  end if;
                  Start_Found  := True;
                  Machine.IP.Y := Row;
                  Machine.IP.X := Col;
               end if;
            end loop;
         end loop;
      end;

      loop
         Process_Next_Instruction (Machine, Input);
      end loop;
   exception
      when Constraint_Error =>
         null;
   end Run;

end SNUSP;
```


Sample usage:

main.adb:

```Ada
with Ada.Text_IO;
with SNUSP;
procedure Main is

   Test_Code : SNUSP.Code :=
      (1 => "Example taken from RosettaCode.org                                       ",
       2 => "                                                                         ",
       3 => "$@\G.@\o.o.@\d.--b.@\y.@\e.>@\comma.@\.<-@\W.+@\o.+++r.------l.@\d.>+.! #",
       4 => "  |   |     \@------|#  |    \@@+@@++|+++#-    \\               -        ",
       5 => "  |   \@@@@=+++++#  |   \===--------!\===!\-----|-------#-------/        ",
       6 => "  \@@+@@@+++++#     \!#+++++++++++++++++++++++#!/                        ");

   My_SNUSP_Machine : SNUSP.SNUSP_Machine;

begin

   SNUSP.Run (My_SNUSP_Machine, Test_Code);

end Main;
```


Output:

```txt
Goodbye, World!
```


[Category:Ada](https://rosettacode.org/wiki/Category:Ada)
