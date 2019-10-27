+++
title = "Conway's Game of Life/ALGOL 68"
description = ""
date = 2011-06-03T01:26:49Z
aliases = []
[extra]
id = 5303
[taxonomies]
categories = []
tags = []
+++

{{collection|Conway's Game of Life}}

The first Life program was written for the [[wp:PDP-7|PDP-7]] by [[wp:Mike Guy|Mike. J. T. Guy]] and [[wp:Stephen R. Bourne|Stephen Bourne]] in 1970.  It was written in [[ALGOL 68]].  c.f. [http://ddi.cs.uni-potsdam.de/HyFISCH/Produzieren/lis_projekt/proj_gamelife/ConwayScientificAmerican.htm Scientific American 223 (October 1970): 120-123]

```algol68
MODE UNIVERSE = [upb OF class universe, upb OF class universe]BOOL;

STRUCT(
  INT upb,
  BOOL lifeless, alive,
  PROC(REF UNIVERSE)VOID init,
  PROC(REF UNIVERSE)STRING repr,
  PROC(REF UNIVERSE, INT, INT)VOID insert glider,
  PROC(REF UNIVERSE)VOID next
) class universe = (
# upb = # 50,
# lifeless = # FALSE,
# alive = # TRUE,

# PROC init = # (REF UNIVERSE self)VOID:
    FOR row index FROM LWB self TO UPB self DO
      init row(self[row index, ])
    OD,

# PROC repr = # (REF UNIVERSE self)STRING:(
    FORMAT cell = $b("[]", "  ")$,
        horizon = $"+"n(UPB self)("--")"+"l$;

    FILE outf; STRING out; associate(outf, out);
    putf(outf, (horizon, $"|"n(UPB self)(f(cell))"|"l$, self, horizon));
    close(outf);
    out
  ),

# PROC insert glider = # (REF UNIVERSE self, INT row, col)VOID:(
    self[row-2, col+1] :=          TRUE;
    self[row-1, col+2] :=                TRUE;
    self[row, col:col+2] := (TRUE, TRUE, TRUE )
  ),

# PROC next = # (REF UNIVERSE self)VOID:(
    [0:2, LWB self-1:UPB self+1]BOOL window; # Create a 3 row window into the previous generation #

    # Set the universe horizon to be lifeless cells #
    init row(window[LWB window, ]);
    window[LWB   self, 2 LWB window] :=
      window[LWB   self, 2 UPB window] :=
        window[UPB window, 2 LWB window] :=
          window[UPB window, 2 UPB window] := lifeless OF class universe;

    # Initialise the first row #
    window[LWB self, LWB self:UPB self] := self[LWB self, ];

    FOR row FROM LWB self TO UPB self DO
      REF []BOOL next row = window[(row+1) MOD 3, ];
      IF row NE UPB self THEN
        next row[LWB self:UPB self] := self[row+1, ]
      ELSE
        init row(next row)
      FI;
      FOR col FROM LWB self TO UPB self DO
        INT live := 0;
        # Scan for life forms in 3x3 block #
        FOR row FROM row-1 TO row+1 DO
          REF[]BOOL window row = window[row MOD 3, ];
          FOR col FROM col-1 TO col+1 DO
            IF window row[col] THEN live +:= 1 FI
          OD
        OD;
        self[row, col] :=
            IF window[row MOD 3, col] THEN #
1. Any live cell with fewer than two live neighbours dies, as if by loneliness.
2. Any live cell with more than three live neighbours dies, as if by overcrowding.
3. Any live cell with two or three live neighbours lives, unchanged, to the next generation. #
              live -:= 1; # don't count life in current cell #
              live = 3 OR live = 2
            ELSE #
4. Any lifeless cell with exactly three live neighbours comes to life. #
              live = 3
            FI
      OD
    OD
  )
);

# Shared static procedure #
PROC init row = (REF [] BOOL xrow)VOID:
  FOR col FROM LWB xrow TO UPB xrow DO xrow[col] := lifeless OF class universe OD;

PROC insert gosper gun = (REF [, ] BOOL universe)VOID:(
  [, ]CHAR template = (
    ("________________________X___________"),
    ("______________________X X___________"),
    ("____________XX______XX____________XX"),
    ("___________X___X____XX____________XX"),
    ("XX________X_____X___XX______________"),
    ("XX________X___X_XX____X_X___________"),
    ("__________X_____X_______X___________"),
    ("___________X___X____________________"),
    ("____________XX______________________")
  );
  FOR row TO 1 UPB template DO
    FOR col TO 2 UPB template DO
      universe[row, col] := template[row, col]="X"
    OD
  OD
);

UNIVERSE conways universe; (init OF class universe)(conways universe);

# Insert a squadron of gliders #
FOR i FROM UPB conways universe OVER 2 BY 5 TO UPB conways universe DO
  (insert glider OF class universe)(conways universe, i, ENTIER (UPB conways universe*1.2 - i*0.9))
OD;

# Insert a gosper (repeating) gun #
insert gosper gun(conways universe[5:, :]);

STRING home = REPR 27 +"[H";
TO 564 DO
  print((home));
  print((repr OF class universe)(conways universe));
  (next OF class universe)(conways universe)
OD
```

Output after 564 iterations:

```txt

+----------------------------------------------------------------------------------------------------+
|                                                                                                    |
|                                                                                                    |
|                                                                                                    |
|                                                                                                    |
|                                                  [][]                                              |
|                                                []  []                                              |
|                    []                        [][][]        [][]  [][][]                            |
|                    [][][][]                [][][]        []    [][][][]                            |
|[][]                  [][][][]                [][][]        [][]                                    |
|[][]                  []    []                  []  []                                              |
|          []          [][][][]                    [][]                                              |
|          []        [][][][]                []                                                      |
|                    []                        []                                                    |
|                                          [][][]                                                    |
|                                                                                                    |
|                                                                                                    |
|                                                                                                    |
|                                                                                                    |
|                                                                                                    |
|                                                            []                                      |
|                                                        []  []                                      |
|                                                          [][]                                      |
|                                                                                                    |
|                                                                                                    |
|                                                                                                    |
|                              [][]                                                                  |
|                              []  []                                      []                        |
|                              []                                            []                      |
|                                                                        [][][]                      |
|                                                                                                    |
|                                                                                                    |
|                                                                                                    |
|                                                                                                    |
|                                                                                                    |
|                                                                                                    |
|                                                                                    [][]            |
|                                                                                    [][]            |
|                                                    []                                              |
|                                                    []                                              |
|                                                    []                                              |
|                                                                                                    |
|                                            [][][]      [][][]                                      |
|                                                                                                    |
|                                                    []                                              |
|                                                    []                                              |
|                                                    []                                              |
|                                                                                      []            |
|                                                                                    []  []          |
|                                                                                    []    []        |
|                                                                                      [][]          |
+----------------------------------------------------------------------------------------------------+

```

