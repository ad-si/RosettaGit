+++
title = "Template:Prelude/sort.a68"
description = ""
date = 2010-10-11T15:33:46Z
aliases = []
[extra]
id = 4116
[taxonomies]
categories = []
tags = []
+++

COMMENT
   This routine is used in more then one place, and is essentially a
   template that can by used for many different types, eg INT, LONG INT...
 USAGE
   MODE SORTSTRUCT = INT, LONG INT, STRUCT(STRING name, addr) etc
   OP < = (SORTSTRUCT a,b)BOOL: ~;
 
   PR READ "prelude/sort.a68" PR;
   [3]SORTSTRUCT list := (a,b,c);
   print((in place shell sort(list), new line))
 END COMMENT

  PROC in place shell sort = (REF FLEX []SORTSTRUCT seq)REF[]SORTSTRUCT:(
      INT inc := ( UPB seq + LWB seq + 1 ) OVER 2;
      WHILE inc NE 0 DO
          FOR index FROM LWB seq TO UPB seq DO
              INT i := index;
              SORTSTRUCT el = seq[i];
              WHILE ( i  - LWB seq >= inc | NOT(seq[i - inc] < el) | FALSE ) DO
                  seq[i] := seq[i - inc];
                  i -:= inc
              OD;
              seq[i] := el
          OD;
          inc := IF inc = 2 THEN 1 ELSE ENTIER(inc * 5 / 11) FI
      OD;
      seq
  );

  PROC in place shell sort reverse = (REF FLEX []SORTSTRUCT seq)REF[]SORTSTRUCT:(
      INT inc := ( UPB seq + LWB seq + 1 ) OVER 2;
      WHILE inc NE 0 DO
          FOR index FROM LWB seq TO UPB seq DO
              INT i := index;
              SORTSTRUCT el = seq[i];
              WHILE ( i  - LWB seq >= inc | seq[i - inc] < el | FALSE ) DO
                  seq[i] := seq[i - inc];
                  i -:= inc
              OD;
              seq[i] := el
          OD;
          inc := IF inc = 2 THEN 1 ELSE ENTIER(inc * 5 / 11) FI
      OD;
      seq
  );
  SKIP
<noinclude>{{template}}</noinclude>
