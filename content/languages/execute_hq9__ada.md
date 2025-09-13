+++
title = "Execute HQ9+/Ada"
description = ""
date = 2017-01-04T12:55:49Z
aliases = []
[extra]
id = 8991
[taxonomies]
categories = []
tags = []
+++




```Ada
with Ada.Text_IO;
procedure HQ9Plus is
   -- took example from bottle-task
   procedure Bottles is
   begin
      for X in reverse 1..99 loop
         Ada.Text_IO.Put_Line(Integer'Image(X) & " bottles of beer on the wall");
         Ada.Text_IO.Put_Line(Integer'Image(X) & " bottles of beer");
         Ada.Text_IO.Put_Line("Take one down, pass it around");
         Ada.Text_IO.Put_Line(Integer'Image(X - 1) & " bottles of beer on the wall");
         Ada.Text_IO.New_Line;
      end loop;
   end Bottles;

   procedure Interpret_HQ9Plus (Input : in String) is
      Accumulator : Natural := 0;
   begin
      for I in Input'Range loop
         case Input (I) is
            when 'H'|'h' =>
               Ada.Text_IO.Put_Line ("Hello, World!");
            when 'Q'|'q' =>
               Ada.Text_IO.Put_Line (Input);
            when '9'     =>
               Bottles;
            when '+'     =>
               Accumulator := Accumulator + 1;
            when others  =>
               null;
         end case;
      end loop;
   end Interpret_HQ9Plus;

   Test_Code : String := "hq9+HqQ+Qq";
begin
   Interpret_HQ9Plus (Test_Code);
end HQ9Plus;
```

