+++
title = "Execute Brain****/Modula-3"
description = ""
date = 2010-02-06T14:23:12Z
aliases = []
[extra]
id = 3348
[taxonomies]
categories = []
tags = []
+++

{{implementation|Brainf***}}{{collection|RCBF}}

This interpreter (in [[Modula-3]]) is a [[Rosetta Code:Example Translation|translation]] of the [[RCBF/Ada|Ada RCBF interpreter]], and has the same limits.


```modula3
MODULE Bfi EXPORTS Main;

IMPORT Stdio, Wr, Rd, Params, FileRd, Text;

<*FATAL ANY*>

TYPE TData = [Bottom..LAST(INTEGER)];

CONST
  Bottom = -1;
  MemLast = 30000;
  ProgLast = 30000;

VAR
  M := ARRAY [0..MemLast] OF TData {0, ..};
  progName := Params.Get(1);
  program: FileRd.T;
  P: ARRAY [0..ProgLast] OF CHAR;
  Px: TEXT; (* workaround for char arrays. *)
  mp, pp, progLen, level: INTEGER;
  data: CHAR;
  OK: BOOLEAN;
  endInput := FALSE;
  endOutput := FALSE;

PROCEDURE MsgErr(msg: TEXT) =
  BEGIN
    Wr.PutText(Stdio.stdout, "\n");
    Wr.PutText(Stdio.stderr, "** Error: " & msg & "\n");
    OK := FALSE;
  END MsgErr;

BEGIN
  program := FileRd.Open(progName);
  Px := Rd.GetText(program, LAST(P));
  Rd.Close(program);
  progLen := Text.Length(Px);
  
  (* Px has type TEXT, but we want ARRAY OF CHAR, so we write
     the characters of Px into the char array P. *)
  Text.SetChars(P, Px);
  
  pp := 0;
  mp := 0;
  OK := TRUE;
  WHILE OK AND (pp < progLen) DO
    CASE P[pp] OF
    | '+' =>
      M[mp] := M[mp] + 1;
    | '-' =>
      IF M[mp] <= Bottom THEN
        MsgErr("Arithmetic underflow");
      ELSE
        M[mp] := M[mp] - 1;
      END;
    | '.' =>
      IF endOutput THEN
        MsgErr("Attempt to write past EOF");
      ELSIF M[mp] < 0 THEN
        endOutput := TRUE;
      ELSE
        Wr.PutChar(Stdio.stdout, VAL(M[mp], CHAR));
      END;
    | ',' =>
      IF endInput THEN
        MsgErr("Attempt to read past EOF");
      ELSE
        LOOP
          IF Rd.EOF(Stdio.stdin) THEN
            M[mp] := Bottom;
            endInput := TRUE;
            EXIT;
          ELSE
            data := Rd.GetChar(Stdio.stdin);
            M[mp] := ORD(data);
            IF data >= ' ' THEN
              EXIT;
            END;
          END;
        END;
      END;
    | '>' =>
      mp := mp + 1;
      IF mp > MemLast THEN
        MsgErr("Memory pointer overflow");
      END;
    | '<' =>
      mp := mp - 1;
      IF mp < 0 THEN
        MsgErr("Memory pointer underflow");
      END;
    | '[' =>
      IF M[mp] = 0 THEN
        pp := pp + 1;
        level := 0;
        WHILE pp < progLen AND (level > 0 OR P[pp] # ']') DO
          IF P[pp] = '[' THEN
            level := level + 1;
          END;
          IF P[pp] = ']' THEN
            level := level - 1;
          END;
          pp := pp + 1;
        END;
        IF pp >= progLen THEN
          MsgErr("No matching ']'");
        END;
      END;
    | ']' =>
      IF M[mp] # 0 THEN
        pp := pp - 1;
        level := 0;
        WHILE pp >= 0 AND (level > 0 OR P[pp] # '[') DO
          IF P[pp] = ']' THEN
            level := level + 1;
          END;
          IF P[pp] = '[' THEN
            level := level - 1;
          END;
          pp := pp - 1;
        END;
        IF pp < 0 THEN
          MsgErr("No matching '['");
        END;
      END;
    ELSE
      (* ignore *)
    END;
    pp := pp + 1;
  END;

Wr.PutText(Stdio.stdout, "\n");
END Bfi.
```

