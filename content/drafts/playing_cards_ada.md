+++
title = "Playing cards/Ada"
description = ""
date = 2010-02-06T14:06:01Z
aliases = []
[extra]
id = 5313
[taxonomies]
categories = []
tags = []
+++

{{collection|Playing Cards}}

Here is the package specification for a deck of playing cards.

```ada
package Playing_Cards is
   pragma Elaborate_Body(Playing_Cards);

   type Card is private;
   procedure Print(The_Card : Card);
   type Deck is private;
   procedure Print(the_Deck : Deck);
   procedure Deal(From : in out Deck; The_Card : out Card);
   procedure Shuffle(The_Deck : in out Deck);
   function New_Deck return Deck;
   Deck_Empty : exception;
private
   type Pips is (Two, Three, Four, Five, Six, Seven,
      Eight, Nine, Ten, Jack, Queen, King, Ace);
   type Suits is (Diamonds, Spades, Hearts, Clubs);
   type Card is record
      Pip : Pips;
      Suit : Suits;
   end record;
   type Index is range 1..53;
   subtype Deck_Index is Index range 1..52;
   type Deck_Reference is array(Deck_Index) of Deck_Index;
   type Deck is record
      Next_Card : Index;
      Deck_Offsets : Deck_Reference;
   end record;
end Playing_Cards;
```

Here is the package body for that same playing card package. This implementation stores one array of cards in sorted order. Each deck contains an array of indices into that one array of cards. Shuffling the deck actually results in randomizing the order of those indices into the array of cards. This approach maximizes shuffling efficiency by only exchanging indices. It also maximizes memory efficiency since an array of cards requires more memory than an array of indices.

```ada
with Ada.Numerics.Discrete_Random;
With Ada.Text_IO;

package body Playing_Cards is
   type Internal_Deck is array(Deck_Index) of Card;
   Base_Deck : Internal_Deck;
   Base_Index : Index;
   ----------
   -- Deal --
   ----------

   procedure Deal (From : in out Deck; The_Card : out Card) is
   begin
      if From.Next_Card not in Deck_Index then
         raise Deck_Empty;
      end if;
      The_Card := Base_Deck(From.Deck_Offsets(From.Next_Card));
      From.Next_Card := From.Next_Card + 1;
   end Deal;

   --------------
   -- New_Deck --
   --------------

   function New_Deck return Deck is
      Temp : Deck;
   begin
      for I in Base_Deck'range loop
         Temp.Deck_Offsets(I) := I;
      end loop;
      Temp.Next_Card := 1;
      return Temp;
   end New_Deck;

   -----------
   -- Print --
   -----------
   
   procedure Print(The_Card : Card) is
      package Pip_Io is new Ada.Text_Io.Enumeration_Io(Pips);
      use Pip_Io;
      package Suit_Io is new Ada.Text_Io.Enumeration_Io(Suits);
      use Suit_Io;
   begin
      Put(Item => The_Card.Pip, Width => 1);
      Ada.Text_Io.Put(" of ");
      Put(Item => The_Card.Suit, Width => 1);
      Ada.Text_Io.New_Line;
   end Print;
   
   -----------
   -- Print --
   -----------
   
   procedure Print(The_Deck : Deck) is
   begin
      for I in The_Deck.Next_Card..Deck_Index'Last loop
         Print(Base_Deck(The_Deck.Deck_Offsets(I)));
      end loop;
   end Print;
   
   -------------
   -- Shuffle --
   -------------

   procedure Shuffle (The_Deck : in out Deck) is
      procedure Swap(Left, Right : in out Deck_Index) is
         Temp : Deck_Index := Left;
      begin
         Left := Right;
         Right := Temp;
      end Swap;
      Swap_Index : Deck_Index;
   begin
      for I in Deck_Index'First..Deck_Index'Pred(Deck_Index'Last) loop
         declare
            subtype Remaining_Indices is Deck_Index range I..Deck_Index'Last;
            package Rand_Card is new Ada.Numerics.Discrete_Random(Remaining_Indices);
            use Rand_Card;
            Seed : Generator;
         begin
            Reset(Seed);
            Swap_Index := Random(Seed);
            Swap(The_Deck.deck_Offsets(I), The_Deck.Deck_Offsets(Swap_Index));
         end;
      end loop;
      The_Deck.Next_Card := 1;
   end Shuffle;
   
begin
   Base_Index := 1;
   for Suit in Suits'range loop
      for Pip in Pips'range loop
         Base_Deck(Base_Index) := (Pip, Suit);
         Base_Index := Base_Index + 1;
      end loop;
   end loop;
end Playing_Cards;
```

