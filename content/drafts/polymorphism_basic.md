+++
title = "Polymorphism/BASIC"
description = ""
date = 2009-07-17T09:43:13Z
aliases = []
[extra]
id = 1978
[taxonomies]
categories = []
tags = []
+++

{{Programming-example-page|Polymorphism}}
'''Interpeter:''' [[QuickBasic]] 4.5, PB 7.1

```qbasic
 DECLARE SUB PointInit0 (pthis AS Point)
 DECLARE SUB PointInit1 (pthis AS Point, x0 AS INTEGER)
 DECLARE SUB PointInit2 (pthis AS Point, x0 AS INTEGER, y0 AS INTEGER)
 DECLARE FUNCTION PointGetX%(pthis AS Point)
 DECLARE FUNCTION PointGetY%(pthis AS Point)
 DECLARE SUB PointSetX (pthis AS Point, x0 AS INTEGER)
 DECLARE SUB PointSetY (pthis AS Point, y0 AS INTEGER)
 DECLARE SUB PointPrint (pthis AS Point)

 DECLARE SUB CircleInit0 (pthis AS Circle)
 DECLARE SUB CircleInit1 (pthis AS Circle, x0 AS INTEGER)
 DECLARE SUB CircleInit2 (pthis AS Circle, x0 AS INTEGER, y0 AS INTEGER)
 DECLARE SUB CircleInit3 (pthis AS Circle, x0 AS INTEGER, y0 AS INTEGER, r0 AS INTEGER)
 DECLARE SUB CircleInitP0 (pthis AS Circle, p as Point)
 DECLARE SUB CircleInitP0 (pthis AS Circle, p as Point, r0 AS INTEGER)
 DECLARE FUNCTION CircleGetX%(pthis AS Circle)
 DECLARE FUNCTION CircleGetY%(pthis AS Circle)
 DECLARE FUNCTION CircleGetR%(pthis AS Circle)
 DECLARE SUB CircleSetX (pthis AS Circle, x0 AS INTEGER)
 DECLARE SUB CircleSetY (pthis AS Circle, y0 AS INTEGER)
 DECLARE SUB CircleSetR (pthis AS Circle, r0 AS INTEGER)
 DECLARE SUB CirclePrint (pthis AS Circle)
 DECLARE SUB PolyPrint (pthis AS ANY, type%)

 TYPE Point
   x AS INTEGER
   y AS INTEGER
 END TYPE
 TYPE Circle
   p AS Point
   r AS INTEGER
 END TYPE

 DIM SHARED POINT%, CIRCLE%
 POINT% = 0
 CIRCLE% = 1

 DIM p AS Point
 DIM c AS Circle

 PointInit p
 CircleInit c

 REM No virtual function call possible
 PointPrint p
 CirclePrint c

 REM Faked virtual function
 PolyPrint p, POINT%
 PolyPrint c, CIRCLE%
 END

  SUB PolyPrint (pthis AS ANY, type%)
  IF (type% = CIRCLE%) THEN
    CirclePrint pthis
  ELSE
    PointPrint pthis
  END IF

  END SUB

  SUB PointInit0 (pthis AS Point)
    pthis.x = 0
    pthis.y = 0
  END SUB

  SUB PointInit1 (pthis AS Point, x0 AS INTEGER)
    pthis.x = x0
    pthis.y = 0
  END SUB

  SUB PointInit2 (pthis AS Point, x0 AS INTEGER, y0 AS INTEGER)
    pthis.x = x0
    pthis.y = y0
  END SUB

  FUNCTION PointGetX% (pthis AS Point)
    PointGetX% = pthis.x
  END SUB

  FUNCTION PointGetY% (pthis AS Point)
    PointGetY% = pthis.y
  END SUB

  SUB PointSetX (pthis AS Point, x0 AS INTEGER)
    pthis.x = x0
  END SUB

  SUB PointSetY (pthis AS Point, y0 AS INTEGER)
    pthis.y = y0
  END SUB

  SUB PointPrint (pthis AS Point)
    PRINT "Point"
  END SUB

  SUB CircleInit0 (pthis AS Circle)
    pthis.x = 0
    pthis.y = 0
    pthis.r = 0
  END SUB

  SUB CircleInit1 (pthis AS Circle, x0 AS INTEGER)
    pthis.x = x0
    pthis.y = y0
    pthis.r = 0
  END SUB

  SUB CircleInit2 (pthis AS Circle, x0 AS INTEGER, y0 AS INTEGER)
    pthis.x = x0
    pthis.y = y0
    pthis.r = 0
  END SUB

  SUB CircleInit3 (pthis AS Circle, x0 AS INTEGER, y0 AS INTEGER, r0 AS INTEGER)
    pthis.x = x0
    pthis.y = y0
    pthis.r = r0
  END SUB

  SUB CircleInitP0 (pthis AS Circle, p as Point)
    pthis.x = p.x
    pthis.y = p.y
    pthis.r = 0
  END SUB

  SUB CircleInitP0 (pthis AS Circle, p as Point, r0 AS INTEGER)
    pthis.x = p.x
    pthis.y = p.y
    pthis.r = r0
  END SUB

  FUNCTION CircleGetX% (pthis AS Circle)
    CircleGetX% = pthis.x
  END SUB

  FUNCTION CircleGetY% (pthis AS Circle)
    CircleGetY% = pthis.y
  END SUB

  FUNCTION CircleGetR% (pthis AS Circle)
    CircleGetR% = pthis.r
  END SUB

  SUB CircleSetX (pthis AS Circle, x0 AS INTEGER)
    pthis.x = x0
  END SUB

  SUB CircleSetY (pthis AS Circle, y0 AS INTEGER)
    pthis.y = y0
  END SUB

  SUB CircleSetR (pthis AS Circle, r0 AS INTEGER)
    pthis.r = r0
  END SUB

  SUB CirclePrint (pthis AS Circle)
    PRINT "Circle"
  END SUB
```

