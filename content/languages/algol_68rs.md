+++
title = "ALGOL 68RS"
description = ""
date = 2010-08-11T22:22:05Z
aliases = []
[extra]
id = 3142
[taxonomies]
categories = []
tags = []
+++


ALGOL 68RS from [RSRE](https://en.wikipedia.org/wiki/Royal_Signals_and_Radar_Establishment) was a portable compiler system written in ALGOL 68RS (bootstrapped from [ALGOL 68R](https://rosettacode.org/wiki/ALGOL_68R)), and implemented on a variety of systems including the [ICL 2900](https://en.wikipedia.org/wiki/wiki_ICL_2900_Series)/[Series 39](https://en.wikipedia.org/wiki/ICL_Series_39), [Multics](https://rosettacode.org/wiki/Multics) and [DEC VAX/VMS](https://en.wikipedia.org/wiki/VAX).

The language was based on the Revised Report, but with similar subset restrictions to [ALGOL 68R](https://rosettacode.org/wiki/ALGOL_68R).
This compiler survives in the form of an Algol68-to-C compiler aka [ELLA ALGOL 68](https://rosettacode.org/wiki/ELLA_ALGOL_68).

## Modules
''ALGOL 68RS'' allow the programmer the ability of creating ''modules'', these can be '''use'''d by other programs and also allowed separate compilation.

Syntax:

```algol68
PROGRAM [ (holelist ) ] progtitle
[ CONTEXT holename IN progtitle ]
[ USE uselist ]
enclosed clause
FINISH
```


```algol68
DECS dectitle
CONTEXT holename IN progtitle
[ USE uselist ]
decsbody
KEEP keeplist
FINISH
```


###  Example 


```algol68
PROGRAM (thinking, displaying) chess
BEGIN
  MODE PIECE = STRUCT(...);
  [12]PIECE pieces;
  ...
  HERE thinking(PIECE, pieces, BOARD, game);
  ...
  HERE displaying(PIECE, pieces, BOARD, game);
  ...
END
```



```algol68
PROGRAM thinkmodule
CONTEXT thinking IN chess
...
FINISH
```


An entire program can thus be '''composed''' of pieces of other modules, particularly if a ''hole'' contains a sub-''hole'' (eg displaypiece), then this ''hole'' can in-turn be pulled into the composition.

Example:

```algol68
PROGRAM abc
COMPOSE game(thinking=thinkmodule, displaying=displayingmodule(piece=displaypiece))
FINISH
```

