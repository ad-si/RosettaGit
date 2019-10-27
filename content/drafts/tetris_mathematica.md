+++
title = "Tetris/Mathematica"
description = ""
date = 2018-07-28T08:29:32Z
aliases = []
[extra]
id = 21933
[taxonomies]
categories = []
tags = []
+++

{{collection|Tetris}}
CREDITS: this is a joint work by Boris F. and Nikita S.
The program is started by calling Tetris[] from a blank input line.
The cursor arrow keys are used to move, rotate, drop.

==Code==
<lang>
(* ::Package::*)
BeginPackage["Tetris`"];

Tetris::usage = "Tetris[] starts game";

Begin["`Private`"];

{w, h} = {14, 26};
bg = {0, 0, 0}; (*background color*)
br = {.3, .3, .3}; (*border color*)
speed0 = .6; (*initial speed*)
fallspeed = 0.001;
acc = 0.8; (*speed acceleration factor*)
sounds = False;
lpl = 2; (*lines per level*)
sndbrick = Play[Sin[1000 t] + Cos[1100 t], {t, 0, .1}];
gomsg = "GAME OVER";
bcaption = "  New game  ";

figs =
  { (*figure:{coords, color}*)
   {{{0, -1}, {0, 0}, {0, 1}, {1, -1}}, {0.1, 0.1, 1.0}} (*J*), 
   {{{0, -1}, {0, 0}, {0, 1}, {1, 1}}, {1.0, 0.5, 0.0}} (*L*),
   {{{1, 0}, {0, 0}, {1, -1}, {0, 1}}, {1.0, 0.0, 0.0}} (*Z*),
   {{{1, 0}, {0, 0}, {0, -1}, {1, 1}}, {0.1, 1.0, 0.1}} (*S*),
   {{{0, 1}, {0, 0}, {0, 2}, {0, -1}}, {0.1, 0.9, 1.0}} (*I*),
   {{{0, 0}, {1, 0}, {1, 1}, {0, 1}}, {1.0, 1.0, 0.1}} (*O*),
   {{{0, -1}, {0, 0}, {0, 1}, {1, 0}}, {0.9, 0.1, 1.0}} (*T*)
  };

init[] :=
  (oldspeed = 0
   ; speed = speed0
   ; lines = score = level = 0
   ; glass = Table[If[1 < j < w && i > 1, bg, br], {i, h}, {j, w}]
   ; nextglass = Table[bg, {4}, {6}]
   ; fig7 = RandomSample@figs
   ; nfig = First@fig7
   ; fig7 = Rest@fig7
   ; newmask[]
   ; playing = False
   ; benabled = True
   ; msg = ""
   ; RemoveScheduledTask /@ ScheduledTasks[]
  );

newmask[] := (mask = Map[# == bg &, glass, {2}]);

newfig[] :=
  (put[nextglass, nfig[[1]], {2, 3}, bg]
   ; {fig, fc} = nfig
   ; nfig = First@fig7
   ; fig7 = Rest@fig7
   ; If[fig7 === {}, fig7 = RandomSample@figs]
   ; put[nextglass, nfig[[1]], {2, 3}, nfig[[2]]]
   ; {y, x} = {h - 3, Floor[w/2]}
   ; If[check[fig, {y, x}]
    , put[glass, fig, {y, x}, fc]
    , stop[]
    ; playing = False
    ; benabled = True
    ; msg = gomsg
    ; Return[]
   ];
   If[oldspeed != 0
    , stop[]
    ; speed = oldspeed
    ; oldspeed = 0
    ; start[]
   ];
  );

rotate[f_] :=
  If[f[[1]] == {0, 0}
   , f
   , {{0, -1}, {1, 0}}.# &  /@ f
  ];

SetAttributes[do, HoldAll];
do[act_] := If[playing, act];

stop[] := RemoveScheduledTask[t];
start[] := StartScheduledTask[t = CreateScheduledTask[move[], speed]];

move[] :=
  If[check[fig, {y - 1, x}]
   , put[glass, fig, {y, x}, bg]
   ; put[glass, fig, {--y, x}, fc]
   , es@sndbrick
   ; newmask[]
   ; del[]
   ; newfig[]
  ];

turn[] :=
  Block[{newf},
   newf = rotate@fig
   ; If[check[newf, {y, x}]
    , put[glass, fig, {y, x}, bg]
    ; fig = newf
    ; put[glass, fig, {y, x}, fc]
   ];
  ];

shift[dx_] :=
  If[check[fig, {y, x + dx}]
   , put[glass, fig, {y, x}, bg]
   ; x += dx
   ; put[glass, fig, {y, x}, fc]
  ];

SetAttributes[es, HoldFirst];
es[s_] := If[sounds, EmitSound@s];

price[n_] := Switch[n, 1, 40, 2, 100, 3, 300, 4, 1200];

del[] := Module[{sel, g, ln},
   sel = Not[Or @@ #] & /@ mask
   ; sel[[1]] = False
   ; g = Pick[glass, Not /@ sel]
   ; If[(ln = h - Length@g) > 0
    , glass = g~Join~Table[If[1 < i < w, bg, br], {ln}, {i, w}]
    ; es@Play[UnitStep@Sin[2000 t] Sin[5000 t t], {t, 0, .2 (ln)}]
    ; lines += ln
    ; score += price[ln]*(level + 1)
    ; cl = Quotient[lines, lpl]
    ; If[cl > level
     , level = cl
     ; stop[]
     ; speed *= acc
     ; oldspeed *= acc
     ; start[]]
    ; newmask[]
   ];
  ];

SetAttributes[#, HoldFirst] & /@ {set, put};

set[g_, p_, c_] := (g[[Sequence @@ p]] = c);

put[g_, f_, p_, c_] := set[g, #, c] & /@ (# + p &  /@ f);

get[p_] := mask[[Sequence @@ p]];

check[f_, p_] := And @@ (get /@ (# + p &  /@ f));

drop[] := If[speed != fallspeed
   , stop[]
   ; playing = False
   ; oldspeed = speed
   ; speed = fallspeed
   ; start[]
   ; playing = True
  ];

menu = Button[bcaption
   , init[]
   ; newfig[]
   ; playing = True
   ; start[]
   ; benabled = False
   , Enabled -> Dynamic@benabled
  ];

Tetris[] := DynamicModule[{},
   init[];
   EventHandler[
    Graphics[
     {Raster@Dynamic@glass[[;; -4]], 
      Raster[Dynamic@nextglass, {{w, h - 7}, {w + 6, h - 3}}], 
      Text[Style["Score", 24, White, Bold], {w, 17}, {-1, 0}], 
      Text[Style[Dynamic@score, 24, White, Bold], {w + 6, 17}, {1, 0}],
      Text[Style["Lines", 24, Green, Bold], {w, 14}, {-1, 0}], 
      Text[Style[Dynamic@lines, 24, Green, Bold], {w + 6, 14}, {1, 0}],
      Text[Style["Level", 24, Cyan, Bold], {w, 11}, {-1, 0}], 
      Text[Style[Dynamic@level, 24, Cyan, Bold], {w + 6, 11}, {1, 0}],
      Text[Style[Dynamic@msg, 24, White, Bold], {w + 3, 7}, {0, 0}], 
      Inset[menu, {w + 3, 2}]
     }
     , PlotRange -> {{0, w + 7}, {0, h - 2}}
     , Background -> RGBColor@br
     , ImageSize -> 600
    ],
    {"RightArrowKeyDown" :> do@shift@1
     , "LeftArrowKeyDown" :> do@shift@-1
     , "UpArrowKeyDown" :> do@turn[]
     , "DownArrowKeyDown" :> do@drop[]
    }
   ]
  ];

End[];

EndPackage[];
```

