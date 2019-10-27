+++
title = "RCSNUSP/Mathematica"
description = ""
date = 2015-11-01T23:16:42Z
aliases = []
[extra]
id = 19721
[taxonomies]
categories = []
tags = []
+++


```Mathematica
$IterationLimit = Infinity;
next[{x_, y_}, dir_] := {x, y} + 
   Switch[dir, Up, {0, -1}, Down, {0, 1}, Left, {-1, 0}, 
    Right, {1, 0}];
lurd[dir_] := 
  Switch[dir, Up, Left, Down, Right, Left, Up, Right, Down];
ruld[dir_] := 
  Switch[dir, Up, Right, Down, Left, Left, Down, Right, Up];
snusp[prog_, {x_, y_}, _, out_, _, _, _] /; 
   x < 1 || y < 1 || y > Length[prog] || x > Length[prog[[1]]] := 
  out;
snusp[prog_, {x_, y_}, in_, out_, dir_, tape_, ptr_] /; 
   prog[[y, x]] == "<" := 
  snusp[prog, next[{x, y}, dir], in, out, dir, 
   If[ptr == 1, Prepend[tape, 0], tape], Max[ptr - 1, 1]];
snusp[prog_, {x_, y_}, in_, out_, dir_, tape_, ptr_] /; 
   prog[[y, x]] == ">" := 
  snusp[prog, next[{x, y}, dir], in, out, dir, 
   If[ptr == Length[tape], Append[tape, 0], tape], ptr + 1];
snusp[prog_, {x_, y_}, in_, out_, dir_, tape_, ptr_] /; 
   prog[[y, x]] == "+" := 
  snusp[prog, next[{x, y}, dir], in, out, dir, 
   ReplacePart[tape, ptr -> Mod[tape[[ptr]] + 1, 256]], ptr];
snusp[prog_, {x_, y_}, in_, out_, dir_, tape_, ptr_] /; 
   prog[[y, x]] == "-" := 
  snusp[prog, next[{x, y}, dir], in, out, dir, 
   ReplacePart[tape, ptr -> Mod[tape[[ptr]] - 1, 256]], ptr];
snusp[prog_, {x_, y_}, in_, out_, dir_, tape_, ptr_] /; 
   prog[[y, x]] == "," := 
  snusp[prog, next[{x, y}, dir], If[in == "", "", StringDrop[in, 1]], 
   out, dir, 
   ReplacePart[tape, 
    ptr -> If[in == "", 255, ToCharacterCode[in][[1]]]], ptr];
snusp[prog_, {x_, y_}, in_, out_, dir_, tape_, ptr_] /; 
   prog[[y, x]] == "." := 
  snusp[prog, next[{x, y}, dir], in, 
   out <> FromCharacterCode[tape[[ptr]]], dir, tape, ptr];
snusp[prog_, {x_, y_}, in_, out_, dir_, tape_, ptr_] /; 
   prog[[y, x]] == "\\" := 
  snusp[prog, next[{x, y}, lurd[dir]], in, out, lurd[dir], tape, 
   ptr];
snusp[prog_, {x_, y_}, in_, out_, dir_, tape_, ptr_] /; 
   prog[[y, x]] == "/" := 
  snusp[prog, next[{x, y}, ruld[dir]], in, out, ruld[dir], tape, 
   ptr];
snusp[prog_, {x_, y_}, in_, out_, dir_, tape_, ptr_] /; 
   prog[[y, x]] == "!" := 
  snusp[prog, next[next[{x, y}, dir], dir], in, out, dir, tape, ptr];
snusp[prog_, {x_, y_}, in_, out_, dir_, tape_, ptr_] /; 
   prog[[y, x]] == "?" := 
  snusp[prog, If[tape[[ptr]] == 0, next, # &][next[{x, y}, dir], dir],
    in, out, dir, tape, ptr];
snusp[prog_, {x_, y_}, in_, out_, dir_, tape_, ptr_] := 
  snusp[prog, next[{x, y}, dir], in, out, dir, tape, ptr];
input[] := 
  Block[{l = InputString[]}, 
   If[l === EndOfFile, "", l <> "\n" <> input[]]];
If[Length[$ScriptCommandLine] < 2, 
  WriteString["stderr", 
   "Usage: WolframScript -script " <> $ScriptCommandLine[[1]] <> 
    " <file...>\n"]; Quit[]];
file = StringRiffle[$ScriptCommandLine[[2 ;;]]];
If[! FileExistsQ[file], 
  WriteString["stderr", 
   "Error: File '" <> file <> "' does not exist.\n"]; Quit[]];
sProg = StringSplit[ReadString[file], "\n"];
gProg = PadRight[Characters[#], Max[StringLength /@ sProg], " "] & /@ 
   sProg;
Print[snusp[gProg, 
   FirstPosition[gProg, 
     "$"] /. {{y_, x_} :> {x, y}, _Missing -> {1, 1}}, 
   StringDrop[input[], -1], "", Right, {0}, 1]];
```

Run as <code>WolframScript -script <file> <program...></code>.
