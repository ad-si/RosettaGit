+++
title = "Bitmap/Oz"
description = ""
date = 2010-02-06T12:32:21Z
aliases = []
[extra]
id = 5344
[taxonomies]
categories = []
tags = []
+++

{{collection|Basic bitmap storage}}

<code>Array.oz</code>:

```oz
functor
export
   New
   Get
   Set
   Fold
   ForAll
   Transform
   Map
   Clone
define
   fun {New Width Height Init}
      C = {Array.new 1 Height unit}
   in
      for Row in 1..Height do
	 C.Row := {Array.new 1 Width Init}
      end

      array2d(width:Width
	      height:Height
	      contents:C)
   end

   fun {Get array2d(contents:C ...) X Y}
      C.Y.X
   end

   proc {Set array2d(contents:C ...) X Y Val}
      C.Y.X := Val
   end

   proc {Transform array2d(contents:C width:W height:H ...) Fun}
      for Y in 1..H do
	 for X in 1..W do
	    C.Y.X := {Fun C.Y.X}
	 end
      end
   end

   fun {Clone array2d(contents:C width:W height:H ...)}
      NC = {Array.clone C}
   in
      for Y in 1..H do
	 NC.Y := {Array.clone NC.Y}
      end
      array2d(width:W
	      height:H
	      contents:NC)
   end
   
   fun {Map Arr Fun}
      R = {Clone Arr}
   in
      {Transform R Fun}
      R
   end

   fun {Fold array2d(contents:C width:W height:H ...) Fun}
      Acc = {NewCell C.1.1}
   in
      for X in 2..W do
	 Acc := {Fun @Acc C.1.X}
      end
      for Y in 2..H do
	 for X in 1..W do
	    Acc := {Fun @Acc C.Y.X}
	 end
      end
      @Acc
   end

   proc {ForAll array2d(contents:C width:W height:H ...) Proc}
      for Y in 1..H do
	 for X in 1..W do
	    {Proc C.Y.X}
	 end
      end
   end
end
```


<code>Bitmap.oz</code>:

```oz
%% For real task prefer QTk's images:
%% http://www.mozart-oz.org/home/doc/mozart-stdlib/wp/qtk/html/node38.html

functor
import
   Array2D
export
   New
   Fill
   GetPixel
   SetPixel
   MaxValue
   ForAllPixels
   Transform
define
   Black = color(0x00 0x00 0x00)
   
   fun {New Width Height}
      bitmap( {Array2D.new Width Height Black} )
   end

   proc {Fill bitmap(Arr) Color}
      {Array2D.transform Arr fun {$ _} Color end}
   end
   
   fun {GetPixel bitmap(Arr) X Y}
      {Array2D.get Arr X Y}
   end
   
   proc {SetPixel bitmap(Arr) X Y Color}
      {Array2D.set Arr X Y Color}
   end

   fun {MaxValue bitmap(Arr)}
      {MaxColorValue {Array2D.fold Arr MaxColor}}
   end
   
   fun {MaxColorValue color(R G B)}
      {FoldL [R G B] Max 0}
   end

   fun {MaxColor C1 C2}
      if {MaxColorValue C1} > {MaxColorValue C2} then C1 else C2 end
   end
   
   proc {ForAllPixels bitmap(Arr) Proc}
      {Array2D.forAll Arr Proc}
   end

   proc {Transform bitmap(Arr) Fun}
      {Array2D.transform Arr Fun}
   end
end
```


<code>makefile.oz</code>:

```oz
makefile(
   lib:['Array2D.ozf' 'Bitmap.ozf']
   )
```

