+++
title = "Execute Brainfuck/ALGOL 68"
description = ""
date = 2014-02-24T19:42:58Z
aliases = []
[extra]
id = 2375
[taxonomies]
categories = []
tags = []
+++

{{implementation|Brainfuck}}{{collection|RCBF}}

An implementation of BF in [[ALGOL 68]] for Rosetta Code.

```algol68
MODE BYTE = SHORT SHORT SHORT INT;

 MODE CADDR = BYTE; # code address #
 MODE OPCODE = BYTE;
 OPCODE nop = 0;

 MODE DADDR = BYTE; # data address #
 MODE DATA = BYTE;
 DATA zero = 0;

 PROC run = ([] OPCODE code list)VOID:(
   [-255:255]DATA data list;  # finite data space #
   FOR i FROM LWB data list TO UPB data list DO data list[i] := zero OD;

   DADDR data addr := ( UPB data list + LWB data list ) OVER 2;
   CADDR code addr := LWB code list;

   [0:127]OPCODE assembler; # 7 bit ascii only #

   STRING op code list="><+-.,[]";
   []PROC VOID op list= []PROC VOID(
     # ? # VOID: SKIP, # NOP #
     # > # VOID: data addr +:= 1,
     # < # VOID: data addr -:= 1,
     # + # VOID: data list[data addr] +:= 1,
     # - # VOID: data list[data addr] -:= 1,
     # . # VOID: print(REPR data list[data addr]),
     # , # VOID: data list[data addr]:=ABS read char,
     # [ # VOID:
             IF data list[data addr] = zero THEN
               # skip to the end of the loop, allowing for nested loops #
               INT br level := 0;
               WHILE
                 IF   code list[code addr] = ABS "["
                 THEN
                   br level +:= 1;
                 ELIF code list[code addr] = ABS "]"
                 THEN
                   br level -:= 1
                 FI;
                 IF br level > 0
                 THEN
                   code addr +:= 1;
                   TRUE
                 ELSE
                   FALSE
                 FI
               DO SKIP OD
             FI,
     # ] # VOID:
             IF data list[data addr] /= zero THEN
               # skip to the start of the loop, allowing for nested loops #
               INT br level := 0;
               WHILE
                 IF   code list[code addr] = ABS "["
                 THEN
                   br level +:= 1
                 ELIF code list[code addr] = ABS "]"
                 THEN
                   br level -:= 1
                 FI;
                 code addr -:= 1;
                 br level < 0
               DO SKIP OD
             FI
   )[:@0];

   FOR op TO UPB assembler DO assembler[op] := nop OD; # insert NOP #
   FOR op TO UPB op code list DO assembler[ABS op code list[op]] := op OD;

   WHILE code addr <= UPB code list DO
     op list[ABS assembler[ABS code list[code addr]]];
     code addr +:= 1
   OD
 );

 STRING code list := "++++++[>+++++++++++<-]>-."; # print the ascii letter A #

 [UPB code list]BYTE byte code list; # convert to bytes #
 FOR i TO UPB code list DO byte code list[i] := ABS code list[i] OD;

 run(byte code list)
```

